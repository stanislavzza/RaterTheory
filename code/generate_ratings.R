#' Generate simulated binary ratings
#' @description Given paramerters for random variable distributions and size of the sample, 
#' generate individual ratings on subjects on a binary classification scale and return
#' characteristics about the raters and subjects.
#' @param N The number of subjects to rate
#' @param R The number of raters for each subject (constant)
#' @param p The  proportion of subjects that are rated class 1 on inaccurate ratings. By default,
#' when t is NA, this is also used for the true proportion of Class 1, which is the expert rater assumption.
#' For more general ratings, set t to something different.
#' @param a  An array of the alpha, beta parameters that describe a beta distribution for
#' generating random accuracy coefficients for each rater, OR a single constant, which 
#' then assigns all raters that accuracy OR an array with more than two values, for the actual rater accuracies.
#' @param t Allows us to specify full t-a-p data, instead of p-a-p expert rater data. Should be a probability of
#' true Class 1 per subject.
#' @return A long dataframe with SubjectID, RaterID, TrueClass, RatedClass, Accuracy, and CondProb, the last of which is the
#' conditional probability that the subject will be rated class 1, given the true class, accuracy, and background truth rate p.
#' @examples 
#' r <- generate_ratings(N = 100, R = 5, p = .1, a = c(1,1))
#' table(r$TrueClass, r$RatedClass)
#' @export
#' 
generate_ratings <- function(N,R,p,a, t= NA){
  
  # prepare data frames for output
  subjects <- data.frame(SubjectID = 1:N, TrueClass = NA)
  raters   <- data.frame(RaterID = 1:R, Accuracy = NA)
  ratings  <- expand.grid(SubjectID = subjects$SubjectID, RaterID = raters$RaterID, RatedClass = NA)

  # assign true class, where 1 = class 1, 0 = other class
  if(is.na(t)){
      subjects$TrueClass <- sample(0:1,N, replace = TRUE, prob = c(1-p, p))
  } else {
    subjects$TrueClass <- sample(0:1,N, replace = TRUE, prob = c(1-t, t))
  }
  # assign accuracy
  if (length(a) == 1){
    raters$Accuracy <- a
  } else if (length(a) == 2) { # for R > 2
    raters$Accuracy <- rbeta(R, a[1], a[2])
  } else if (length(a) == R) {
    raters$Accuracy <- a
  } else {
    stop("Accuracy parameter a is the wrong length.")
  }
  
  # convenience function
  # takes a single probability and returns 1 with that probability, 0 otherwise
  bernoulli <- function(prob){
    sample(0:1,1, replace = TRUE, prob = c(1-prob, prob))
  }
  
  # assign ratings and return
  output <- ratings %>% 
    left_join(subjects, by = "SubjectID") %>% 
    left_join(raters, by = "RaterID") %>% 
    mutate( CondProb = TrueClass*Accuracy + (1-Accuracy)*p,
      RatedClass  = map_dbl(CondProb, bernoulli))  
  
  
  return(output)
}
