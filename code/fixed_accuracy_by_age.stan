//
//   Stan model specification for fixed rater accuracy and no random effects
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// functions to make the code simpler below
functions {
  real p_true(real a, real s) {  // convenience function for binomial probability for 
    return a + (1.0-a)*s;        // subjects that are class 1 in reality
  }
  real p_false(real a, real s) {  // convenience function for binomial probability for
    return (1.0-a)*s;            // subjects that are class 2 in reality
  }
  
  real logistic(real x){
    return 1/(1+exp(-x));
  }
  
}

// The ratings summary (number of 1-ratings per case) and descriptives
data {
  int<lower=0> N;   // number of subjects
  int n_raters[N];   // number of raters
  real age[N];
  real<lower=0, upper = 1> S; // rate of 1s in classification ratings
  int count[N];  // count of ratings of category 1 for subject i
}

// The parameter to estimate
parameters {
  real b0;
  real b1;
}

// The model to be estimated. We model the output
// count (of 1s) by the binomial mixture described
// in the paper. S is the fraction of 1-ratings in the whole data set
// The log_sum_exp function is useful for this--we take the log of each binomial 
// likelihood using built-in functions, and the log_sum_exp function exponentiates,
// adds, and then takes the log to get the actual likelihood we care about. 
// cf http://modernstatisticalworkflow.blogspot.com/2016/10/finite-mixture-models-in-stan.html
model {
  b0 ~ normal(0,4);
  b1 ~ normal(0,4);
  
  for(i in 1:N) {  // for each subject rated
    
    target += log_sum_exp(log(S)   + binomial_lpmf(count[i] | n_raters[i], p_true(logistic(b0 + b1*age[i]),S)),
                          log(1-S) + binomial_lpmf(count[i] | n_raters[i], p_false(logistic(b0 + b1*age[i]),S)));
  }
}

