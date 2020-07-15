library(tidyverse)
library(knitr)

source("R/fleiss.R")
source("R/lambda.R")
source("R/resample.R")
source("R/sim.R")
########## Lambda ############

# evenly distributed true cases
even <- sim_raters(rep(.2,5), .9, 1000, 5)
prop.table(colSums(even))
kappa_even <- pairwise_lambda(even)

kappa_even %>%
  select(col1,col2,lambda_kappa) %>%
  spread(col2, lambda_kappa) %>%
  kable(digits = 2)

# skewed true cases
skewed <- sim_raters(c(.01,.05,.1,.34, .5), .5, 1000, 5)
kappa_skewed <- pairwise_lambda(skewed, equalize = FALSE) #setting to TRUE is VERY slow

kappa_skewed %>%
  select(col1,col2,lambda_kappa) %>%
  spread(col2, lambda_kappa) %>%
  kable(digits = 2)

########## Fleiss ############

kappa_even %>%
  select(col1,col2,fleiss_kappa) %>%
  spread(col2, fleiss_kappa) %>%
  kable(digits = 2)

kappa_skewed %>%
  select(col1,col2,fleiss_kappa) %>%
  spread(col2, fleiss_kappa) %>%
  kable(digits = 2)

########## Graph the kappas ###############

p <- seq(.01,.99,.01)

out <- data.frame(p = p, lambda = NA, fleiss = NA)

for(i in 1:length(p)){
  
  kappas <- sim_raters(rep(.2,5), p[i], 1000, 5) %>%
    pairwise_lambda() %>%
    summarize(lambda = mean(lambda_kappa),
              fleiss = mean(fleiss_kappa))
  
  out$lambda[i] <- kappas$lambda
  out$fleiss[i] <- kappas$fleiss
}

gout1 <- out %>%
  gather(kappa, value, -p)

ggplot(gout1, aes(x = p, y = value, group = kappa, color = kappa)) +
  geom_smooth(se = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")


# skewed
p <- seq(.01,.99,.01)

out <- data.frame(p = p, lambda = NA, fleiss = NA)

for(i in 1:length(p)){
  
  kappas <- sim_raters(c(.01,.05,.1,.34, .5), p[i], 1000, 5) %>%
    pairwise_lambda() %>%
    summarize(lambda = mean(lambda_kappa),
              fleiss = mean(fleiss_kappa))
  
  out$lambda[i] <- kappas$lambda
  out$fleiss[i] <- kappas$fleiss
}

gout2 <- out %>%
  gather(kappa, value, -p)

ggplot(gout2, aes(x = p, y = value, group = kappa, color = kappa)) +
  geom_smooth(se = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")


###### Resampling to eliminate skew ################

# given p_matrix, with two columns, and sampling size S, create a balanced resample
sampled <- resample_matrix(even[,1:2],1000)

table(sampled)

sum(sampled == 2) / nrow(sampled) - .5
