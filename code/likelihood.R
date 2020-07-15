# probability distribution function for n ratings of class 1
# conditional = TRUE is the distribution based on rows of data
# whereas condition = FALSE is the entire data set
n_prob <- function(ES,n,R,a, conditional = TRUE){

  if(conditional == TRUE){
    ap <- 1 - a
   return(ES*dbinom(n,R,a + ap*ES) + (1-ES)*dbinom(n,R, ap*ES))
  } else{
    return(dbinom(n,R,ES))
  }
}

# likelihood function for a, given a set of ratings that are
# sums of classifications in two columns: class 1 and class 2
likelihood <- function(a,R,ratings){
  ES <- sum(ratings[,1])/(nrow(ratings)*R)
  c_n <- table(ratings[,1])
  ns  <- as.integer(names(c_n))
  ap <- 1 - a
  
  sum <- 0
  for(n in ns){
    cnt <- c_n[n == ns]
    sum <- sum + cnt*log(n_prob(ES,n,R,a)) #ES*dbinom(n,R,a + ap*ES) + (1-ES)*dbinom(n,R, ap*ES))
  }
  
  return(sum)
}

# agreement rate
agree <- function(n,R){
  (n*(n-1) + (R-n)*(R-n-1))/(R*(R-1))
  #(2*n^2 - n*R + R)/(R*(R-1))
}

# individual rating likelihood function
# wants vectors for each argument
#rating_likelihood <- function(rating, accuracy, truth, P)