#################################################
# Autograder tests for PH142 lab05-probability-sol
#
###############################################
sol_path <- "setup/src/"
source("setup/autograder_setup.R")

# Replace with number of problems
setup_autograder(15)

# --------------------------------------------
check_problem1 = function() {
  problem_num <- 1 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p1) == "numeric" & 
               length(p1) == 1,
             correct_message = "Correct!",
             error_message = "Is p1 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p1, qnorm(0.8, mean = 0, sd = 1)) ,
             correct_message = "Correct!",
             error_message = "Did you compute the correct z-value?")
  
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
             test = class(p2) == "numeric" & 
               length(p2) == 1,
             correct_message = "Correct!",
             error_message = "Is p2 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p2, qnorm(0.35, mean = 0, sd = 1, lower.tail=FALSE)) ,
             correct_message = "Correct!",
             error_message = "Did you compute the correct z-value?")
  
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
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p3) == "numeric" & 
               length(p3) == 1,
             correct_message = "Correct!",
             error_message = "Is p3 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p3, qnorm(0.25, mean = 3350, sd = 440)) ,
             correct_message = "Correct!",
             error_message = "Did you compute the correct percentile?")
  
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
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p4) == "numeric" & 
               length(p4) == 1,
             correct_message = "Correct!",
             error_message = "Is p4 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p4, qnorm(0.90, mean = 3350, sd = 440)) ,
             correct_message = "Correct!",
             error_message = "Did you compute the correct percentile?")
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = length(p5) == 2,
             correct_message = "Correct!",
             error_message = "Is p5 a vector of 2 values?")
  
  checkpoint(checkpoint_number = 2,
             test = p5[1] == qnorm(0.25, mean = 3350, sd = 440) ,
             correct_message = "Correct!",
             error_message = "Did you compute the correct value for 25th percentile")

 checkpoint(checkpoint_number = 2,
             test = p5[2] == qnorm(0.75, mean = 3350, sd = 440) ,
             correct_message = "Correct!",
             error_message = "Did you compute the correct value for 75th percentile")
  
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
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p6) == "numeric" & 
               length(p6) == 1,
             correct_message = "Correct!",
             error_message = "Is p6 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p6, qnorm(0.25, mean = 2750, sd = 560)) ,
             correct_message = "Correct!",
             error_message = "Did you compute the correct percentile?")
  
  
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
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p7) == "numeric" & 
               length(p7) == 1,
             correct_message = "Correct!",
             error_message = "Is p7 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p7, qnorm(0.9, mean = 2750, sd = 560)) ,
             correct_message = "Correct!",
             error_message = "Did you compute the correct percentile?")
  
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = length(p8) == 2,
             correct_message = "Correct!",
             error_message = "Is p8 a vector of 2 values?")
  
  checkpoint(checkpoint_number = 2,
             test = p8[1] == qnorm(0.25, mean = 2750, sd = 560) ,
             correct_message = "Correct!",
             error_message = "Did you compute the correct value for 25th percentile")

checkpoint(checkpoint_number = 2,
           test = p8[2] == qnorm(0.75, mean = 2750, sd = 560),
           correct_message = "Correct!",
           error_message = "Did you compute the correct value for 75th percentile")
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p9) == "numeric" & 
               length(p9) == 1,
             correct_message = "Correct!",
             error_message = "Is p9 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p9/100, round(pnorm(55, mean = 50, sd = 5, lower.tail=FALSE)*100, 2)/100),
             correct_message = "Correct!",
             error_message = "Did you compute the correct probability?")
  
  checkpoint(checkpoint_number = 3,
             test = all.equal(p9, round(pnorm(55, mean = 50, sd = 5, lower.tail=FALSE)*100, 2), lower.tail=FALSE),
             correct_message = "Correct!",
             error_message = "Did you convert to a percentage?")
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p10) == "numeric" & 
               length(p10) == 1,
             correct_message = "Correct!",
             error_message = "Is p10 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p10/100, round((pnorm(55, mean = 50, sd = 5) - pnorm(40, mean = 50, sd = 5))*100, 2)/100),
             correct_message = "Correct!",
             error_message = "Did you compute the correct probability?")
  
  checkpoint(checkpoint_number = 3,
             test = all.equal(p10, round((pnorm(55, mean = 50, sd = 5) - pnorm(40, mean = 50, sd = 5))*100, 2), lower.tail=FALSE),
             correct_message = "Correct!",
             error_message = "Did you convert to a percentage?")
  
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

check_problem11 = function() {
  problem_num <- 11 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  
  checkpoint(checkpoint_number = 1,
             test = class(p11) == "numeric" & 
               length(p11) == 1,
             correct_message = "Correct!",
             error_message = "Is p11 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p11/100, round(pnorm(1, mean = 0.8, sd = 0.078, lower.tail=FALSE)*100,2)/100),
             correct_message = "Correct!",
             error_message = "Did you compute the correct probability?")
  
  checkpoint(checkpoint_number = 3,
             test = all.equal(p11, round(pnorm(1, mean = 0.8, sd = 0.078, lower.tail=FALSE)*100,2)),
             correct_message = "Correct!",
             error_message = "Did you convert to a percentage?")
  
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

check_problem12 = function() {
  problem_num <- 12 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p12) == "numeric" & 
               length(p12) == 1,
             correct_message = "Correct!",
             error_message = "Is p12 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p12, pnorm(-2.25, 0, 1)),
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

check_problem13 = function() {
  problem_num <- 13 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  
  checkpoint(checkpoint_number = 1,
             test = class(p13) == "numeric" & 
               length(p13) == 1,
             correct_message = "Correct!",
             error_message = "Is p13 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p13, pnorm(-2.25, 0, 1, lower.tail = F)),
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

check_problem14 = function() {
  problem_num <- 14 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p14) == "numeric" & 
               length(p14) == 1,
             correct_message = "Correct!",
             error_message = "Is p14 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p14, pnorm(1.77, 0, 1, lower.tail = F)),
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

check_problem15 = function() {
  problem_num <- 15 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  
  checkpoint(checkpoint_number = 1,
             test = class(p15) == "numeric" & 
               length(p15) == 1,
             correct_message = "Correct!",
             error_message = "Is p15 a numeric value?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(p15, pnorm(1.77, 0, 1) - pnorm(-2.25, 0, 1)),
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
capture.output(check_problem11(), file='NUL')
capture.output(check_problem12(), file='NUL')
capture.output(check_problem13(), file='NUL')
capture.output(check_problem14(), file='NUL')
capture.output(check_problem15(), file='NUL')