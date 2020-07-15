if(regenerate == TRUE){
  re_model_spec <- stan_model("code/t-a_i-p.stan")
  
  #ratings <- read_csv("data/ind_rem.csv")
  ratings <- generate_ratings(N = 1000, R = 5, p = .4, a = c(.1,.3,.5,.7,.9)) # p = prob(true class 1)
  
  re_model <- rstan::sampling(object = re_model_spec, 
                              data = list(N = nrow(ratings), 
                                          R = length(unique(ratings$RaterID)), 
                                          S = length(unique(ratings$SubjectID)), 
                                          rating = ratings$RatedClass,
                                          P = mean(ratings$RatedClass),
                                          rater_index = ratings$RaterID,
                                          subject_index = ratings$SubjectID), 
                              iter = 1000,
                              warmup = 200,
                              thin = 1)
  
  
  saveRDS(re_model,"data/ind_rem.RDS")
  write_csv(ratings, "data/ind_rem.csv")
} else {
  re_model <- readRDS("data/ind_rem.RDS")
  ratings <- read_csv("data/ind_rem.csv")
}
# take a look 
# launch_shinystan(re_model)

# get the coefficients and add them to the datafarme

re_model_coefs <- broom::tidy(re_model)

rater_acc    <- re_model_coefs %>% 
  filter(str_detect(term, "accuracy")) %>% 
  separate(term, into = c("junk","RaterID"), sep = "\\[") %>% 
  mutate(RaterID = as.integer(gsub("\\]","", RaterID))) %>% 
  select(RaterID, AccEst = estimate, SE = std.error)

class1_probs    <- re_model_coefs %>% 
  filter(str_detect(term, "truth")) %>% 
  separate(term, into = c("junk","SubjectID"), sep = "\\[") %>% 
  mutate(SubjectID = as.integer(gsub("\\]","", SubjectID))) %>% 
  select(SubjectID, Class1Prob = estimate)

ratings <- ratings %>% 
  left_join(class1_probs) %>% 
  left_join(rater_acc)

# plot accuracies
#library(tidybayes)
p1 <- re_model %>%
  spread_draws(accuracy[i]) %>%
  rename(RaterID = i) %>% 
  left_join(ratings %>% select(RaterID, Accuracy) %>% distinct()) %>% 
  ggplot(aes(y = reorder(factor(RaterID),Accuracy), x = accuracy)) +
  stat_halfeye(.width = c(.90, .90)) + 
  geom_point(aes(x = Accuracy), shape =5, size = 3) +
  theme_bw() +
  #  (axis.text.y=element_blank(),
  #        panel.background = element_blank(),
  #        panel.grid.major.x =  element_blank(),
  #        panel.grid.major.y = element_line( size=.5, color="black" ) ) +
  xlab("Rater Accuracy") +
  ylab("Rater") 


#plot the estimated probabililties of class 1
p2 <- ggplot(ratings %>% 
               select(SubjectID,Class1Prob,TrueClass) %>% 
               group_by(SubjectID,Class1Prob,TrueClass) %>% 
               distinct() %>% ungroup(), aes(x = Class1Prob, fill = as.factor(TrueClass))) +
  geom_histogram(alpha = .5, position = "identity", color = "black", bins = 20) +
  theme_bw() +
  scale_y_log10() +
  scale_fill_manual(values=c("black","white")) +
  labs(fill="Class") +
  xlab("Estimated Probability of Class 1") +
  ylab("")

plot_grid(p1, p2, labels = c('A', 'B'))