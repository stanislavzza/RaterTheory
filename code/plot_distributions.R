#' Get binary classification distributions
#' comparing sample proportions of ratings for Class 1 to
#' the expert model and the t-a-p model.
#' @param counts an integer vector counting Class 1 (versus Class 0) votes per subject
#' @param n_raters an integer vector with number of raters per subject
#' @param t a real vector of samples of t
#' @param a a real vector of samples of a
#' @param p a real vector of samples of p
#' @details The t, a, p vectors probably come from a Bayes model output. For the expert model, use p = NA,
#' which sets p = t
#' @return A ggplot object with observed fractions versus two and three (if p isn't NA) parameter models

get_distributions <- function(counts, R){
   N <- length(counts)*R
   
   # ensure the table has all possible values 0..R
   counts <- c(counts,0:R)
   cnts <- table(counts) - 1
   
   #cnts <- cnts*R
   pt <- prop.table(cnts)
   df <- data.frame(k = as.integer(names(pt)), cnts*R, pt) %>% 
     select(k, n = Freq, observed = Freq.1)
   
   # get the binomial rates
   class_1_rate <- sum(counts)/(R*length(counts))
   df$random <- dbinom(df$k,R,class_1_rate)
  
   # t-a-t version with t = E(class 1)
   # a = sqrt(kappa)
   a <- sqrt(abs(fleiss(counts,R)))
   E1 <- class_1_rate
   true_rate <- a + (1-a)*E1
   false_rate <- (1-a)*E1
   df$expert <- E1*dbinom(df$k,R,true_rate) + (1-E1)*dbinom(df$k,R,false_rate)
   
   # get the t-a-p model version
   tat_model <- stan_model("code/fixed_effects.stan")
     
   tat_samples <- sampling(object = tat_model, 
                             data = list(N = length(counts), R = R, count = counts), 
                             iter = 1000,
                             warmup = 200,
                             thin = 1)
   
   stats <- broom::tidy(tat_samples) %>% 
    select(term, estimate) %>% 
     spread(term, estimate) 
   
   a <- stats$accuracy
   t <- stats$t
   p <- stats$p
   
   true_rate <- a + (1-a)*p
   false_rate <- (1-a)*p
   df$`t-a-p` <- t*dbinom(df$k,R,true_rate) + (1-t)*dbinom(df$k,R,false_rate)
   
   ######################### Error Estimates ###########################
   # bootstrap confidence intervals for the original counts
   out <- data.frame()
   for(i in 1:1000){
     my_samples <- sample(counts,length(counts),replace = TRUE) 
     pt <- prop.table(table(my_samples))
     tdf <- data.frame(k = as.integer(names(pt)), pt) %>% 
       select(k, Freq) %>% 
       mutate(Sim = i)
     out <- rbind(out, tdf)
   }
   
   cis_observed <- out %>% 
     group_by(k) %>% 
     summarize(lower = quantile(Freq,c(.05)),
               upper = quantile(Freq,c(.95))) %>% 
     mutate(Model = "observed")
   
   # simulate 95% ci assuming the random rate
   out <- data.frame()
   for(i in 1:1000){
     my_samples <- rbinom(N, R, class_1_rate) 
     pt <- prop.table(table(my_samples))
     tdf <- data.frame(k = as.integer(names(pt)), pt) %>% 
       select(k, Freq) %>% 
       mutate(Sim = i)
     out <- rbind(out, tdf)
   }
   
   cis_random <- out %>% 
     group_by(k) %>% 
     summarize(lower = quantile(Freq,c(.05)),
               upper = quantile(Freq,c(.95))) %>% 
     mutate(Model = "random")
   
   cis <- rbind(cis_observed,cis_random)
   
   # combine the output
   df %>% 
     gather(Model,p,-k, -n) %>% 
     left_join(cis) %>% 
     return()
}

plot_distributions <- function(df){
  
  # df<- get_distributions(counts,R)
  df$Model <- factor(df$Model, levels = c("observed","random","expert","t-a-p"))
  
  ggplot(df, aes(x = k, y = p, shape = Model, linetype = Model, ymin = lower, ymax = upper)) +
    geom_point(size = 1.5,  position = position_dodge(width = .5)) +
    geom_errorbar(width = 0, position = position_dodge(width = .5)) +
    theme_bw() +
    ylab("Probability")
  
  
}
