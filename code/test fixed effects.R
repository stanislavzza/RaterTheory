# this script tests to see if we can recover the parameters used to create the data
# for fixed values of truth, accuracy, and guessing rate

# see the stan source code for the model
fixed_model_spec <- stan_model("code/fixed_effects.stan")

# get the data
true_1s <- .2 
N       <- 500
R       <- 5
accuracy <- .7
sim_ratings <- sim_raters(c(true_1s, 1-true_1s), accuracy, N, R)
S <- sum(sim_ratings[,1])/(N*R)

fixed_model <- sampling(object = fixed_model_spec, 
                        data = list(N = N, R = R, S = S, count = sim_ratings[,1]), 
                        iter = 1000,
                        warmup = 200,
                        thin = 1)

# take a look 
# launch_shinystan(estimated_model)

print(fixed_model)


############################# Test with wine data ##############################
ratings <- read_csv("data/Wine Ratings.csv") 

out <- data.frame()

for(i in 1:3){

  count <- rowSums(ratings <=i)
  
  
  fixed_model <- sampling(object = fixed_model_spec, 
                          data = list(N = length(count), R = 4, S = 1, count = count), 
                          iter = 1000,
                          warmup = 200,
                          thin = 1)
  
  stats <- broom::tidy(fixed_model) %>% mutate(Cut = i)

  out <- rbind(out,stats)
}

ggplot(out, aes(x = Cut, y = estimate, 
                ymin = estimate -2*std.error, ymax = estimate + 2*std.error,
                linetype = term)) +
  geom_point(position = position_dodge(width = .05)) +
  geom_line(position = position_dodge(width = .05)) +
  geom_errorbar(width = .02,position = position_dodge(width = .05)) +
  theme_minimal()

