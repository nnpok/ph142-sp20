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
             test = identical(p1$data, CS_data),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p1$mapping$x) == "GDP_2006",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = quo_get_expr(p1$mapping$y) == "CS_rate_100",
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(CS_data_log),
             correct_message = "Correct!",
             error_message = "Did you name your new data set CS_data_log?")
  
  checkpoint(checkpoint_number = 2,
             test = CS_data_log$log_CS == log(CS_data_log$CS_rate_100),
             correct_message = "Correct!",
             error_message = "Did you transform CS_rate_100 correctly?")
  
  checkpoint(checkpoint_number = 3,
             test = CS_data_log$log_GDP == log(CS_data_log$GDP_2006),
             correct_message = "Correct!",
             error_message = "Did you transform GDP_2006 correctly?")

  
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
             test = "ggplot" %in% class(p3),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p3$data, CS_data_log),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p3$mapping$x) == "log_GDP",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = quo_get_expr(p3$mapping$y) == "log_CS",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 5,
             test = "GeomPoint" %in% class(p3$layers[[1]]$geom), 
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
check_problem4 = function() {
  problem_num <- 4 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 6 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p4),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p4$data, CS_data_log),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p4$mapping$x) == "log_GDP",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = quo_get_expr(p4$mapping$y) == "log_CS",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 5,
             test = "GeomPoint" %in% class(p4$layers[[1]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a scatterplot in ggplot?")
  
  checkpoint(checkpoint_number = 6,
             test = "GeomSmooth" %in% class(p4$layers[[2]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a geom_smooth in ggplot?")
  
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
  num_tests <<- 7 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p5),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p5$data, CS_data_log),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p5$mapping$x) == "log_GDP",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = quo_get_expr(p5$mapping$y) == "log_CS",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 5,
             test = "GeomPoint" %in% class(p5$layers[[1]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a scatterplot in ggplot?")
  
  checkpoint(checkpoint_number = 6,
             test = "GeomSmooth" %in% class(p5$layers[[2]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a geom_smooth in ggplot?")
  
  checkpoint(checkpoint_number = 7,
             test = p5$labels$colour == "Income_Group", 
             correct_message = "Correct!",
             error_message = "Did you set the plot to color by Income_Group?")
  
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
             test = is.data.frame(CS_data_sub),
             correct_message = "Correct!",
             error_message = "Did you name your new data set CS_data_sub?")
  
  checkpoint(checkpoint_number = 2,
             test = "Low income" %in% CS_data_sub$Income_Group,
             correct_message = "Correct!",
             error_message = "Did you filter Income_Group correctly?")
  
  checkpoint(checkpoint_number = 3,
             test = "Lower middle income" %in% CS_data_sub$Income_Group,
             correct_message = "Correct!",
             error_message = "Did you filter Income_Group correctly?")
  
  checkpoint(checkpoint_number = 4,
             test = "Upper middle income" %in% CS_data_sub$Income_Group,
             correct_message = "Correct!",
             error_message = "Did you filter Income_Group correctly?")
  
  checkpoint(checkpoint_number = 5,
             test = !("High income: nonOECD" %in% CS_data_sub$Income_Group),
             correct_message = "Correct!",
             error_message = "Did you filter Income_Group correctly?")
  
  checkpoint(checkpoint_number = 6,
             test = !("High income: OECD" %in% CS_data_sub$Income_Group),
             correct_message = "Correct!",
             error_message = "Did you filter Income_Group correctly?")
  
  
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
  num_tests <<- 7 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p7),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p7$data, CS_data_sub),
             correct_message = "Correct!",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p7$mapping$x) == "log_GDP",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 4,
             test = quo_get_expr(p7$mapping$y) == "log_CS",
             correct_message = "Correct!",
             error_message = "Did you plot the right variable?")
  
  checkpoint(checkpoint_number = 5,
             test = "GeomPoint" %in% class(p7$layers[[1]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a scatterplot in ggplot?")
  
  checkpoint(checkpoint_number = 6,
             test = "GeomSmooth" %in% class(p7$layers[[2]]$geom), 
             correct_message = "Correct!",
             error_message = "Did you define a geom_smooth in ggplot?")
  
  checkpoint(checkpoint_number = 7,
             test = p7$labels$colour == "Income_Group", 
             correct_message = "Correct!",
             error_message = "Did you set the plot to color by Income_Group?")
  
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
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "log_CS" %in% names(p8$model),
             correct_message = "Correct!",
             error_message = "Incorrect! Try again.")
  
  checkpoint(checkpoint_number = 2,
             test = "log_GDP" %in% names(p8$model),
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
capture.output(check_problem3(), file='NUL')
capture.output(check_problem4(), file='NUL')
capture.output(check_problem5(), file='NUL')
capture.output(check_problem6(), file='NUL')
capture.output(check_problem7(), file='NUL')
capture.output(check_problem8(), file='NUL')