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

  //we need to push the truth value s toward 0 or 1 to recover accuracy
  // since those parameters generate data with zeros or ones

  real sigmoid(real x){
    return(1/(1+exp(-(x-.5)*30)));
  }
  
  // rate = rating  (0 or 1)
  // acc = accuracy for rater
  // truth = probability of Class 1
  // pop = global frequency of Class 1
  
  real log_lik(real rate, real acc, real truth, real pop){
    real cond_prob;
    
    // Conditional probability of class 1 rating (1)
    cond_prob = acc*sigmoid(truth) + (1.0 - acc)*pop;
    
    // Conditional probability of class 2 rating (0)
    if(rate == 0) cond_prob = (1.0 - cond_prob);
    
    // return log likelihood
    return(log(cond_prob));
  }
}

// The ratings summary (number of 1-ratings per case) and descriptives
data {
  int<lower=0> N;   // number of ratings
  int<lower=0> S;   // number of subjects being rated
  int<lower=0> R;   // number of raters
  real<lower=0, upper = 1> P; // proportion of 1s in classification ratings
  int rating[N];  // rating for the ith subject by some rater
  int rater_index[N];  // the ID of the rater for the Nth vote. Should be in 1:R
  int subject_index[N]; // the ID of the subject for the Nth vote. Should be in 1:S
}

// The parameter to estimate
parameters {
  real<lower=0, upper = 1> accuracy[R];  // random effect 
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
  
  for(i in 1:R){ 
    accuracy[i]  ~ uniform(0,1);
  }
  
  for(i in 1:S){ 
    truth[i]  ~ uniform(0,1);
  }
  
  // Likelihood model
  for(i in 1:N) {
      target += log_lik(rating[i], accuracy[rater_index[i]], truth[subject_index[i]], P);
  }
}

