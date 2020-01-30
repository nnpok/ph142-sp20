#################################################
# Autograder tests for PH142 lab02-cesarean-delivery-sol
#
###############################################
sol_path <- "setup/src/"
source("setup/autograder_setup.R")

# Replace with number of problems
setup_autograder(8)

# --------------------------------------------
check_problem1 = function() {
  problem_num <- 1 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(CS_data),
             correct_message = "Correct!",
             error_message = "Check the type of CS_data.")
  
  checkpoint(checkpoint_number = 2,
             test = nrow(CS_data) == 137 && ncol(CS_data) == 9,
             correct_message = "Correct!",
             error_message = "Data frame is of the wrong size.")
  
  checkpoint(checkpoint_number = 3,
             test = identical(CS_data, readRDS(paste0(sol_path, "p1.RDS"))),
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

# --------
check_problem2 = function() {
  problem_num <- 2 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p2),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p2$data, CS_data),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p2$mapping$x) == "Income_Group",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = "GeomBar" %in% class(p2$layers[[1]]$geom), 
             correct_message = "A bar chart has been defined in ggplot",
             error_message = "Did you define a bar chart in ggplot?")
  
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

#--------
check_problem3 = function() {
  problem_num <- 3 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p3),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p3$data, CS_data),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p3$mapping$x) == "Income_Group_order",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = "GeomBar" %in% class(p3$layers[[1]]$geom), 
             correct_message = "A bar chart has been defined in ggplot",
             error_message = "Did you define a bar chart in ggplot?")
  
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

#------
check_problem4 = function() {
  problem_num <- 4 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p4),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p4$data, CS_data),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p4$mapping$x) == "Region",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = "GeomBar" %in% class(p4$layers[[1]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a bar chart in ggplot?")
  
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

#------
check_problem5 = function() {
  problem_num <- 5 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p5),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p5$data, CS_data),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p5$mapping$x) == "GDP_2006",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = "GeomBar" %in% class(p5$layers[[1]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a histogram in ggplot?")
  
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


#------
check_problem6 = function() {
  problem_num <- 6 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 5 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p6),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p6$data, CS_data),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p6$mapping$x) == "GDP_2006",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = "GeomBar" %in% class(p6$layers[[1]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a histogram in ggplot?")
  
  checkpoint(checkpoint_number = 5,
             test = length(p6$layers[[1]]$stat_params$binwidth) != 0, 
             correct_message = "Correct!",
             error_message = "Did you set a bin width?")
  
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


#------
check_problem7 = function() {
  problem_num <- 7 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 5 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p7),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p7$data, CS_data),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p7$mapping$x) == "CS_rate_100",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = "GeomBar" %in% class(p7$layers[[1]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a histogram in ggplot?")
  
  checkpoint(checkpoint_number = 5,
             test = length(p7$layers[[1]]$stat_params$binwidth) != 0, 
             correct_message = "Correct!",
             error_message = "Did you set a bin width?")
  
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

#------
check_problem8 = function() {
  problem_num <- 8 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 10 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p8),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p8$data, CS_data),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p8$mapping$x) == "CS_rate_100",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = "GeomBar" %in% class(p8$layers[[1]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a histogram in ggplot?")
  
  checkpoint(checkpoint_number = 5,
             test = length(p8$layers[[1]]$stat_params$binwidth) != 0, 
             correct_message = "Correct!",
             error_message = "Did you set a bin width?")
  
  checkpoint(checkpoint_number = 6,
             test = length(p8$layers[[1]]$aes_params)!=0, 
             correct_message = "Correct!",
             error_message = "Did you set the fill color?")
  
  checkpoint(checkpoint_number = 7,
             test = length(p8$labels$y) != 0, 
             correct_message = "Correct!",
             error_message = "Did you set the y-axis label?")
  
  checkpoint(checkpoint_number = 8,
             test = length(p8$labels$x) != 0, 
             correct_message = "Correct!",
             error_message = "Did you set the x-axis label?")
  
  checkpoint(checkpoint_number = 9,
             test = length(p8$labels$y) != 0, 
             correct_message = "Correct!",
             error_message = "Did you set the y-axis label?")
  
  checkpoint(checkpoint_number = 10,
             test = length(p8$labels$title) != 0, 
             correct_message = "Correct!",
             error_message = "Did you set the title?")
  
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