#################################################
# Autograder tests for PH142 lab10
#
###############################################
sol_path <- "setup/src/"
source("setup/autograder_setup.R")

# Replace with number of problems
setup_autograder(7)

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
             test = "numeric" %in% class(p1),
             correct_message = "A numeric vector is made.",
             error_message = "You did not make a numeric vector.")
  
  checkpoint(checkpoint_number = 2,
             test = length(p1) == 2,
             correct_message = "Correct number of elements",
             error_message = "Your list has the wrong number of elements")
  
  checkpoint(checkpoint_number = 3,
             test = p1[1] < p1[2],
             correct_message = "Correct order for elements",
             error_message = "Your bounds are not stored in the correct order.")
  
  
  checkpoint(checkpoint_number = 4,
             test = 0.21 < p1[1] & p1[1] < 0.22,
             correct_message = "Correct lower bound!",
             error_message = "Your lower bound has a wrong number.")
  
  checkpoint(checkpoint_number = 5,
             test = 0.52 < p1[2] & p1[2] < 0.53,
             correct_message = "Correct upper bound!",
             error_message = "Your upper bound is wrong.")
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p2) == "htest" & 
               length(p2) == 9,
             correct_message = "Correct!",
             error_message = "Is p2 a prop test?")
  
  checkpoint(checkpoint_number = 2,
             test = p2$method == "1-sample proportions test with continuity correction",
             correct_message = "Correct!",
             error_message = "Is your answer using the correct method?")
  
  checkpoint(checkpoint_number = 3,
             test =  p2$data.name == "14 out of 38, null probability 0.5",
             correct_message = "Correct!",
             error_message = "Did you the use the correct input?")
  

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
check_problem3 = function() {
  problem_num <- 3 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 5 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "numeric" %in% class(p3),
             correct_message = "A numeric vector is made.",
             error_message = "You did not make a numeric vector.")
  
  checkpoint(checkpoint_number = 2,
             test = length(p3) == 2,
             correct_message = "Correct number of elements",
             error_message = "Your list has the wrong number of elements")
  
  checkpoint(checkpoint_number = 3,
             test = p3[1] < p3[2],
             correct_message = "Correct order for elements",
             error_message = "Your bounds are not stored in the correct order.")
  
  
  checkpoint(checkpoint_number = 4,
             test = 0.23 < p3[1] & p3[1] < 0.24,
             correct_message = "Correct lower bound!",
             error_message = "Your lower bound has a wrong number.")
  
  checkpoint(checkpoint_number = 5,
             test = 0.52 < p3[2] & p3[2] < 0.53,
             correct_message = "Correct upper bound!",
             error_message = "Your upper bound is wrong.")

  
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
check_problem4 = function() {
  problem_num <- 4 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p4) == "htest" & 
               length(p4) == 9,
             correct_message = "Correct!",
             error_message = "Is p4 a binom test?")
  checkpoint(checkpoint_number = 2,
             test = p4$method == "Exact binomial test",
             correct_message = "Correct!",
             error_message = "Is your answer using the correct method?")
  
  checkpoint(checkpoint_number = 3,
             test =  p4$data.name == "14 and 38",
             correct_message = "Correct!",
             error_message = "Did you the use the correct input?")
  
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

check_problem5 = function() {
  problem_num <- 5 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 0 # num of checkpoints
  
  problem_types[problem_num] <<- "free-response" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
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

check_problem6 = function() {
  problem_num <- 6 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 0 # num of checkpoints
  
  problem_types[problem_num] <<- "free-response" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
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

check_problem7 = function() {
  problem_num <- 7 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 0 # num of checkpoints
  
  problem_types[problem_num] <<- "free-response" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  
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


capture.output(check_problem1(), file='NUL')
capture.output(check_problem2(), file='NUL')
capture.output(check_problem3(), file='NUL')
capture.output(check_problem4(), file='NUL')
capture.output(check_problem5(), file='NUL')
capture.output(check_problem6(), file='NUL')
capture.output(check_problem7(), file='NUL')
