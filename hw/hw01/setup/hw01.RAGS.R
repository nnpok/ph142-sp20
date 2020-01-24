#################################################
# Autograder tests for PH142 hw01-data-manipulation-dplyr

###############################################
sol_path <- "setup/src/"
source("setup/autograder_setup.R")

# Replace with number of problems
setup_autograder(16)

# --------------------------------------------
check_problem1 = function() {
  problem_num <- 1 # problem number
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = length(p1) == 4, 
             correct_message = "Correct!",
             error_message = "You should list 4 functions")
  
  checkpoint(checkpoint_number = 2,
             test = all(p1 %in% c("rename", 
                                  "select", 
                                  "arrange", 
                                  "filter", 
                                  "mutate", 
                                  "group_by", 
                                  "summarize")), 
             correct_message = "Correct!",
             error_message = "Incorrect.")
  
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
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(sleep_small), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_small) == 3,
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
  checkpoint(checkpoint_number = 3,
             test = (names(sleep_small) == c("awake", "brainwt", "bodywt")),
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(sleep_small_colon), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_small_colon) == 3,
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
  checkpoint(checkpoint_number = 3,
             test = (names(sleep_small_colon) == c("awake", "brainwt", "bodywt")),
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
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
             test = is.data.frame(sleep_no_vore), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_no_vore) == 10,
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
  checkpoint(checkpoint_number = 3,
             test = !("vore" %in% names(sleep_no_vore)),
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
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
  num_tests <<- 1 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = p5 == "returns all rows that start with sl", 
             correct_message = "Correct!",
             error_message = "Incorrect.")
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(sleep_sl), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_sl) == 3,
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
  checkpoint(checkpoint_number = 3,
             test = ("sleep_total" %in% names(sleep_sl)) &&
                    ("sleep_rem" %in% names(sleep_sl)) &&
                    ("sleep_cycle" %in% names(sleep_sl)),
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
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
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(sleep_over16), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_over16) == 11,
             correct_message = "Correct!",
             error_message = "Did you subset from the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(sleep_over16) == 8,
             correct_message = "Correct!",
             error_message = "Did you subset your rows correctly?")
  
  checkpoint(checkpoint_number = 4,
             test = identical(sleep_over16, readRDS(paste0(sol_path, "p7.RDS"))),
             correct_message = "Correct!",
             error_message = "Did you subset your rows correctly?")
  
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
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(sleep_mammals), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_mammals) == 11,
             correct_message = "Correct!",
             error_message = "Did you subset from the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(sleep_mammals) == 3,
             correct_message = "Correct!",
             error_message = "Did you subset your rows correctly?")
  
  checkpoint(checkpoint_number = 4,
             test = identical(sleep_mammals, readRDS(paste0(sol_path, "p8.RDS"))),
             correct_message = "Correct!",
             error_message = "Did you subset your rows correctly?")
  
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
             test = is.data.frame(sleep_hg), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_hg) == 11,
             correct_message = "Correct!",
             error_message = "Did you subset from the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(sleep_hg) == 2,
             correct_message = "Correct!",
             error_message = "Did you subset your rows correctly?")
  
  checkpoint(checkpoint_number = 4,
             test = "Horse" %in% sleep_hg$name &&
                    "Giraffe" %in% sleep_hg$name,
             correct_message = "Correct!",
             error_message = "Did you subset your rows correctly?")
  
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
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(sleep_time), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_time) == 11,
             correct_message = "Correct!",
             error_message = "Did you arrange the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(sleep_time) == 83,
             correct_message = "Correct!",
             error_message = "You should keep all observations.")
  
  checkpoint(checkpoint_number = 4,
             test = identical(sleep_time, readRDS(paste0(sol_path, "p10.RDS"))),
             correct_message = "Correct!",
             error_message = "Did you arrange your rows correctly?")
  
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
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(sleep_time_rev), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_time_rev) == 11,
             correct_message = "Correct!",
             error_message = "Did you arrange the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(sleep_time_rev) == 83,
             correct_message = "Correct!",
             error_message = "You should keep all observations.")
  
  checkpoint(checkpoint_number = 4,
             test = identical(sleep_time_rev, readRDS(paste0(sol_path, "p11.RDS"))),
             correct_message = "Correct!",
             error_message = "Did you arrange your rows correctly?")
  
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
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(sleep_time_vore), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_time_vore) == 11,
             correct_message = "Correct!",
             error_message = "Did you arrange the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(sleep_time_vore) == 83,
             correct_message = "Correct!",
             error_message = "You should keep all observations.")
  
  checkpoint(checkpoint_number = 4,
             test = identical(sleep_time_vore, readRDS(paste0(sol_path, "p12.RDS"))),
             correct_message = "Correct!",
             error_message = "Did you arrange your rows correctly?")
  
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
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(sleep_ratio), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_ratio) == 12,
             correct_message = "Correct!",
             error_message = "Do you have the correct number of columns?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(sleep_ratio) == 83,
             correct_message = "Correct!",
             error_message = "You should keep all observations.")
  
  checkpoint(checkpoint_number = 4,
             test = identical(sleep_ratio, readRDS(paste0(sol_path, "p13.RDS"))),
             correct_message = "Correct!",
             error_message = "Did you arrange your rows correctly?")
  
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
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(sleep_r_bw), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(sleep_r_bw) == 13,
             correct_message = "Correct!",
             error_message = "Do you have the correct number of columns?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(sleep_r_bw) == 83,
             correct_message = "Correct!",
             error_message = "You should keep all observations.")
  
  checkpoint(checkpoint_number = 4,
             test = identical(sleep_r_bw, readRDS(paste0(sol_path, "p14.RDS"))),
             correct_message = "Correct!",
             error_message = "Did you arrange your rows correctly?")
  
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
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(avg_sleep_time), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(avg_sleep_time) == 1 &&
                    nrow(avg_sleep_time) == 1,
             correct_message = "Correct!",
             error_message = "Incorrect.")
  
  checkpoint(checkpoint_number = 3,
             test = is.numeric(avg_sleep_time$sleep_avg),
             correct_message = "Correct!",
             error_message = "Did you name your column correctly?")
  
  checkpoint(checkpoint_number = 4,
             test = all.equal(avg_sleep_time$sleep_avg, 10.43373, tol = 0.01),
             correct_message = "Correct!",
             error_message = "Incorrect.")
  
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
check_problem16 = function() {
  problem_num <- 16 # problem number
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(avg_by_vore), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(avg_by_vore) == 2 &&
               nrow(avg_by_vore) == 5,
             correct_message = "Correct!",
             error_message = "Incorrect.")
  
  checkpoint(checkpoint_number = 3,
             test = identical(names(avg_by_vore), c("vore", "sleep_avg")),
             correct_message = "Correct!",
             error_message = "Incorrect.")
  
  checkpoint(checkpoint_number = 4,
             test = identical(avg_by_vore, readRDS(paste0(sol_path, "p16.RDS"))),
             correct_message = "Correct!",
             error_message = "Did you arrange your rows correctly?")
  
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
capture.output(check_problem16(), file='NUL')