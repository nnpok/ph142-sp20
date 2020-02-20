#################################################
# Autograder tests for PH142 lab04-probability-sol
#
###############################################
sol_path <- "setup/src/"
source("setup/autograder_setup.R")

# Replace with number of problems
setup_autograder(10)

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
             test = length(p1) == 9,
             correct_message = "Correct number of elements",
             error_message = "Your list has the wrong number of elements")
  
  checkpoint(checkpoint_number = 3,
             test = p1[1:3] == c(475, 25, 500),
             correct_message = "Correct first column!",
             error_message = "Your first column has a wrong number.")
  
  
  checkpoint(checkpoint_number = 4,
             test = p1[4:6] == c(475, 9025, 9500),
             correct_message = "Correct second column!",
             error_message = "Your second column has a wrong number.")
  
  checkpoint(checkpoint_number = 5,
             test = p1[7:9] == c(950, 9050, 10000),
             correct_message = "Correct row totals!",
             error_message = "Your row totals are wrong.")
  
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
             test = class(p2) == "numeric" & 
               length(p2) == 1,
             correct_message = "Correct!",
             error_message = "Is p2 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = p2 > 1,
             correct_message = "Correct!",
             error_message = "Is your answer converted to percent?")
  
  checkpoint(checkpoint_number = 3,
             test = p2 == 50,
             correct_message = "Correct!",
             error_message = "Did you compute the correct probability?")
  
  
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
             test = length(p3) == 9,
             correct_message = "Correct number of elements",
             error_message = "Your list has the wrong number of elements")
  
  checkpoint(checkpoint_number = 3,
             test = p3[1:3] == c(190, 10, 200),
             correct_message = "Correct first column!",
             error_message = "Your first column has a wrong number.")
  
  
  checkpoint(checkpoint_number = 4,
             test = p3[4:6] == c(490, 9310, 9800),
             correct_message = "Correct second column!",
             error_message = "Your second column has a wrong number.")
  
  checkpoint(checkpoint_number = 5,
             test = p3[7:9] == c(680, 9320, 10000),
             correct_message = "Correct row totals!",
             error_message = "Your row totals are wrong.")
  
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
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p4) == "numeric" & 
               length(p4) == 1,
             correct_message = "Correct!",
             error_message = "Is p2 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = p4 > 1,
             correct_message = "Correct!",
             error_message = "Is your answer converted to percent?")
  
  checkpoint(checkpoint_number = 3,
             test = round(p4, 1) == p4,
             correct_message = "Correct!",
             error_message = "Did you round your answer?")
  
  
  checkpoint(checkpoint_number = 4,
             test = p4 == 27.9,
             correct_message = "Correct!",
             error_message = "Did you compute the correct probability?")
  
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
  num_tests <<- 6 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "numeric" %in% class(p6),
             correct_message = "A numeric vector is made.",
             error_message = "You did not make a numeric vector.")
  
  checkpoint(checkpoint_number = 2,
             test = length(p6) == 4,
             correct_message = "Correct number of elements",
             error_message = "Your list needs 4 elements")
  
  checkpoint(checkpoint_number = 3,
             test = p6[1] == 20,
             correct_message = "Correct probability for P(D|A', S)",
             error_message = "Incorrect probability for P(D|A', S).")
  
  
  checkpoint(checkpoint_number = 4,
             test = p6[2] == 15,
             correct_message = "Correct probability for P(D|A', S')",
             error_message = "Incorrect probability for P(D|A', S')")
  
  checkpoint(checkpoint_number = 5,
             test = p6[3] == 60,
             correct_message = "Correct probability for P(D|A, S)",
             error_message = "Incorrect probability for P(D|A, S)")
  
  checkpoint(checkpoint_number = 6,
             test = p6[4] == 30,
             correct_message = "Correct probability for P(D|A, S')",
             error_message = "Incorrect probability for P(D|A, S')")
  
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

check_problem8 = function() {
  problem_num <- 8 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p8) == "numeric" & 
               length(p8) == 1,
             correct_message = "Correct!",
             error_message = "Is p8 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = p8 > 1,
             correct_message = "Correct!",
             error_message = "Is your answer converted to percent?")
  
  checkpoint(checkpoint_number = 3,
             test = round(p8, 0) == p8,
             correct_message = "Correct!",
             error_message = "Did you round your answer?")
  
  
  checkpoint(checkpoint_number = 4,
             test = p8 == 28,
             correct_message = "Correct!",
             error_message = "Did you compute the correct probability?")
  
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

check_problem9 = function() {
  problem_num <- 9 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "numeric" %in% class(p9),
             correct_message = "A numeric vector is made.",
             error_message = "You did not make a numeric vector.")
  
  checkpoint(checkpoint_number = 2,
             test = length(p9) == 2,
             correct_message = "Correct number of elements",
             error_message = "Your list needs 2 elements")
  
  checkpoint(checkpoint_number = 3,
             test = p9[1] == 17,
             correct_message = "Correct probability for P(D|A')",
             error_message = "Incorrect probability for P(D|A').")
  
  
  checkpoint(checkpoint_number = 4,
             test = p9[2] ==39,
             correct_message = "Correct probability for P(D|A)",
             error_message = "Incorrect probability for P(D|A)")
  
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

check_problem10 = function() {
  problem_num <- 10 # problem number
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
capture.output(check_problem8(), file='NUL')
capture.output(check_problem9(), file='NUL')
capture.output(check_problem10(), file='NUL')