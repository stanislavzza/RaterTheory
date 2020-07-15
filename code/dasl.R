# Generates conditional statistics for the wine levels
# given the medal as Class 1, the others collectively become Class 0

get_dasl_data <- function(fname){

  ratings <- read_csv(fname) %>% 
    mutate(Year = floor((TermNum+1)/2),
           SubjectID = paste0(FurmanID,"-",Year)) %>% 
    select(SubjectID, RaterID, Rating, Year, RaterAge)
  
  rater_n <- ratings %>% 
    count(RaterID) %>% 
    filter(n >= 30) %>% 
    select(-n)
  
  ratings <- ratings %>% 
    inner_join(rater_n) 
  
  subject_n <- ratings %>% 
    count(SubjectID) %>% 
    filter(n > 2) %>% 
    select(-n)
  
  ratings <- ratings %>% 
    inner_join(subject_n) %>% 
    mutate(SubjectID = as.integer(as.factor(SubjectID)), # unique per year
           RaterID = as.integer(as.factor(RaterID))) 
    
  
  return(ratings)
}
 
get_dasl_stats <- function(cutpoint){
  
  ratings <- get_dasl_data() %>% 
    mutate(RatedClass = (Rating <= cutpoint) + 0)  
  
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
  
  saveRDS(re_model,"data/dasl_re.RDS")
  tdf <- broom::tidy(re_model) 
  
  acc <- tdf %>% 
    filter(str_detect(term,"accuracy")) %>% 
    separate(term, into = c("junk","RaterID"), sep = "\\[") %>% 
    mutate(RaterID = as.integer(gsub("\\]","", RaterID))) %>% 
    select(RaterID, AccEst = estimate, SE = std.error) %>% 
    mutate(Cut = cutpoint)
  
  truth <- tdf %>% 
    filter(str_detect(term,"truth")) %>% 
    separate(term, into = c("junk","SubjectID"), sep = "\\[") %>% 
    mutate(SubjectID = as.integer(gsub("\\]","", SubjectID))) %>% 
    select(SubjectID, ProbTrue= estimate, SE = std.error) %>% 
    mutate(Cut = cutpoint) %>% 
    left_join(ratings %>% select(SubjectID, Year))
  
  
  return(list(accuracy = acc, truth = truth))   
}
