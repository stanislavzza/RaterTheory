# Fleiss kappa for two classes
# k is the number of class 1 votes
# n is the number of raters
# test k = c(2,2,0,1,3,5); n = 5
fleiss <- function(k, n){
  
  match_rate <- function(k,n){
    (choose(k,2) + choose(n-k,2))/choose(n,2)
  }
  
  df <- data.frame(k = k, n = n )
  observed <- mean(match_rate(df$k, df$n))
  chance   <- sum(df$k)/sum(df$n)
  chance   <- chance^2 + (1-chance)^2
  
  return((observed - chance)/(1-chance))
  
}

# ratings in long form: SubjectID, RaterID, Rating
pairwise_accuracy <- function(ratings){
  
  rating_values <- sort(unique(ratings$Rating))
  L             <- length(rating_values)
  my_rows <- rating_values[2:L]
  my_cols <- rating_values[1:(L-1)]
  
  out <- expand.grid(rating_1 = my_rows, rating_2 = my_cols, a = NA) %>%
    filter(rating_1 > rating_2) 
  
  for( i in 1:nrow(out)) {
     rval1  <- out$rating_1[i]
     rval2 <-  out$rating_2[i]
      
     # filter to the conditional values and count cases
     tdf <- ratings %>% 
                filter(Rating %in% c(rval1,rval2)) %>% 
              group_by(SubjectID) %>% 
              summarize( k = sum(Rating == rval1),
                         n = n()) %>% 
              filter(n > 1)
       
     out$a[i] <- sqrt(fleiss(tdf$k,tdf$n)) %>% round(2) # NaN if negative kappa
  }
       
  out %>% 
    pivot_wider(rating_1, rating_2, names_sep = "_", values_from ="a") %>% 
    rename(rating = rating_1) %>% 
    return()
}
