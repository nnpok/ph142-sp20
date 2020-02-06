#################################################
# Autograder tests for PH142 lab02-cesarean-delivery-sol
#
###############################################
sol_path <- "setup/src/"
source("setup/autograder_setup.R")

# Replace with number of problems
setup_autograder(2)

# --------------------------------------------
check_problem1 = function() {
  problem_num <- 1 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 5 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p1),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p1$data, dat_sim),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p1$mapping$x) == "X",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = quo_get_expr(p1$mapping$y) == "Y",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 5,
             test = "GeomPoint" %in% class(p1$layers[[1]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a scatterplot in ggplot?")
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------
check_problem2 = function() {
  problem_num <- 2 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "Y" %in% names(p2$model),
             correct_message = "Correct!",
             error_message = "Incorrect! Try again.")
  
  checkpoint(checkpoint_number = 2,
             test = "X" %in% names(p2$model),
             correct_message = "Correct!",
             error_message = "Incorrect! Try again.")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}



capture.output(check_problem1(), file='NUL')
capture.output(check_problem2(), file='NUL')