#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung, Nolan Pokpongkiat, Anna Nguyen

# Autograder: Homework 2, Prevalence
###############################################
source("setup/autograder_setup.R")

AutograderSetUp(16)

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber <<- 1
  num_tests <<- 1
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = TRUE == TRUE, 
             correct_message = "Correct!",
             error_message = "Incorrect.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


# --------------------------------------------
CheckProblem2 = function() {
  problemNumber <<- 2
  num_tests <<- 3
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = is.data.frame(smaller_sleep_data), 
             correct_message = "Final answer is a data frame",
             error_message = "Make sure your final answer is a data frame.")
  
  CheckPoint(checkpoint_number = 2,
             test = ncol(smaller_sleep_data) == 3,
             correct_message = "Final answer is a data frame with three columns",
             error_message = "Did you subset your columns correctly?")
  
  CheckPoint(checkpoint_number = 3,
             test = (names(smaller_sleep_data) == c("awake", "brainwt", "bodywt")),
             correct_message = "Final answer has been subsetted with correct columns.",
             error_message = "Did you subset your columns correctly?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


# --------------------------------------------
CheckProblem3 = function() {
  problemNumber <<- 3
  num_tests <<- 3
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = is.data.frame(smaller_sleep_data), 
             correct_message = "Final answer is a data frame",
             error_message = "Make sure your final answer is a data frame.")
  
  CheckPoint(checkpoint_number = 2,
             test = ncol(smaller_sleep_data) == 3,
             correct_message = "Final answer is a data frame with three columns",
             error_message = "Did you subset your columns correctly?")
  
  CheckPoint(checkpoint_number = 3,
             test = (names(smaller_sleep_data) == c("awake", "brainwt", "bodywt")),
             correct_message = "Final answer has been subsetted with correct columns.",
             error_message = "Did you subset your columns correctly?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}





