# Generates conditional statistics for the wine levels
# given the medal as Class 1, the others collectively become Class 0

get_wine_stats <- function(medal){

  ratings <- read_csv("data/Wine Ratings.csv") %>% 
    mutate(SubjectID = row_number()) %>% 
    gather(RaterID,Rating,-SubjectID) %>% 
    mutate(RaterID = as.integer(substr(RaterID,2,2)),
           RatedClass = (Rating <= medal) + 0)  
  
  re_model_spec <- stan_model("code/individual_random_effects.stan")
  
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
  
  
  tdf <- broom::tidy(re_model) 
  
  acc <- tdf %>% 
    filter(str_detect(term,"accuracy")) %>% 
    separate(term, into = c("junk","RaterID"), sep = "\\[") %>% 
    mutate(RaterID = as.integer(gsub("\\]","", RaterID))) %>% 
    select(RaterID, AccEst = estimate, SE = std.error) %>% 
    mutate(Medal = medal)
  
  truth <- tdf %>% 
    filter(str_detect(term,"truth")) %>% 
    separate(term, into = c("junk","SubjectID"), sep = "\\[") %>% 
    mutate(SubjectID = as.integer(gsub("\\]","", SubjectID))) %>% 
    select(SubjectID, ProbTrue= estimate, SE = std.error) %>% 
    mutate(Medal = medal)
 
  return(list(accuracy = acc, truth = truth))   
}

get_all_wine <- function(){
  
  accuracy <- data.frame()
  truth    <- data.frame()
  
  for(medal in 1:3){
    out <- get_wine_stats(medal)
    accuracy <- rbind(accuracy,out$accuracy)
    truth    <- rbind(truth,out$truth)
  }
  
  return(list(accuracy = accuracy, truth = truth))
  
}
