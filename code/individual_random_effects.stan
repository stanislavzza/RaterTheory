//
//   Stan model specification for fixed rater accuracy and random truth
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
  
}

// The ratings summary (number of 1-ratings per case) and descriptives
data {
  int<lower=0> N;   // number of ratings
  int<lower=0> S;   // number of subjects being rated
  int<lower=0> R;   // number of raters
  real<lower=0, upper = 1> P; // proportion of 1s in classification ratings
  int rating[N];  // rating for the ith subject by some raters
  int rater_index[N];  // the ID of the rater for the Nth vote. Should be in 1:R
  int subject_index[N]; // the ID of the subject for the Nth vote. Should be in 1:S
}

// The parameter to estimate
parameters {
  real<lower=0, upper = 1> accuracy[R];  // fixed effect--constant accuracy for all raters
  real<lower=0, upper = 1> truth[S];  // random truth value for each subject
}

// The model to be estimated. We model the output
// count (of 1s) by the binomial mixture described
// in the paper. S is the fraction of 1-ratings in the whole data set
// The log_sum_exp function is useful for this--we take the log of each binomial 
// likelihood using built-in functions, and the log_sum_exp function exponentiates,
// adds, and then takes the log to get the actual likelihood we care about. 
// cf http://modernstatisticalworkflow.blogspot.com/2016/10/finite-mixture-models-in-stan.html
model {
  // Priors
  for(i in 1:R){ 
    accuracy[i]  ~ uniform(0,1);
  }
  
  for(i in 1:S){ 
    truth[i]  ~ uniform(0,1);
  }
  
  // Likelihood model
  for(i in 1:N) {
      target += log_sum_exp( log(truth[subject_index[i]]) + 
                             binomial_lpmf(rating[i] | 1 , p_true(accuracy[rater_index[i]],P)),
                            log(1-truth[subject_index[i]]) + 
                            binomial_lpmf(rating[i] | 1, p_false(accuracy[rater_index[i]],P)));
  }
}

