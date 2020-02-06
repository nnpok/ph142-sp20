#################################################
# Autograder tests for PH142 hw03-predict-insurance.Rmd

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
             test = is.data.frame(insure_data), 
             correct_message = "",
             error_message = "Incorrect. Try again.")
  
  checkpoint(checkpoint_number = 2,
             test = typeof(insure_data$sex) == "character", 
             correct_message = "",
             error_message = "Please use the version of this data-reading function with _ instead of .")
  
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
             test = is.numeric(p2), 
             correct_message = "",
             error_message = "Please assign p2 to a numeric.")
  
  checkpoint(checkpoint_number = 2,
             test = p2 > 1000 && p2 < 2000, 
             correct_message = "",
             error_message = "FAILED")
  
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
             test = is.numeric(p2), 
             correct_message = "",
             error_message = "Please assign p2 to a numeric.")
  
  checkpoint(checkpoint_number = 2,
             test = p2 > 1000 && p2 < 2000, 
             correct_message = "",
             error_message = "FAILED")
  
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
             test = is.vector(p3), 
             correct_message = "",
             error_message = "Please assign p3 to a vector of strings.")
  
  checkpoint(checkpoint_number = 2,
             test = typeof(p3) == "character", 
             correct_message = "",
             error_message = "Please enter your variable names as strings (ie in quotations)")
  
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
             test = is.numeric(p4), 
             correct_message = "",
             error_message = "Make sure p4 is numeric.")
  
  checkpoint(checkpoint_number = 2,
             test = p4 < 5, 
             correct_message = "",
             error_message = "FAILED")
  
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
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.vector(p5), 
             correct_message = "",
             error_message = "Please assign p5 to a vector of strings.")
  
  checkpoint(checkpoint_number = 2,
             test = typeof(p5) == "character", 
             correct_message = "",
             error_message = "Please enter your variable names as strings (ie in quotations)")
  
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
             test = is.vector(p6), 
             correct_message = "",
             error_message = "Please assign p6 to a vector of strings.")
  
  checkpoint(checkpoint_number = 2,
             test = typeof(p6) == "character", 
             correct_message = "",
             error_message = "Please enter your variable names as strings (ie in quotations)")
  
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
  num_tests <<- 1 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = p8 == "ordinal" ||
               p8 == "nominal" ||
               p8 == "continuous" ||
               p8 == "discrete", 
             correct_message = "",
             error_message = "Please set p8 to one of the provided choices.")
  
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
  num_tests <<- 1 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = p9 == "Smokers of normal BMI" ||
               p9 == "Smokers of overweight BMI" ||
               p9 == "Smokers who have abnormal BMI" ||
               p9 == "All people at risk of high medical charges", 
             correct_message = "",
             error_message = "Please set p9 to one of the provided choices.")
  
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
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(insure_subset),
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 1,
             test = nrow(insure_subset) < 100 && nrow(insure_subset) > 0,
             correct_message = "",
             error_message = "FAILED")
  
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
  max_scores[problem_num] <<- 3 # total pts possible
  num_tests <<- 6 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p11),
             correct_message = "",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p11$data, insure_subset),
             correct_message = "",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p11$mapping$x) == "age" || 
               quo_get_expr(p11$mapping$x) == "charges",
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 4,
             test = quo_get_expr(p11$mapping$y) == "age" || 
               quo_get_expr(p11$mapping$y) == "charges",
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 5,
             test = "GeomPoint" %in% class(p11$layers[[1]]$geom), 
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 6,
             test = !is.null(p11$labels$title), 
             correct_message = "",
             error_message = "FAILED")
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(insure_model) == "lm",
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 2,
             test = "charges" %in% names(insure_model$model),
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 3,
             test = "age" %in% names(insure_model$model),
             correct_message = "",
             error_message = "")
  
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
  max_scores[problem_num] <<- 3 # total pts possible
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
check_problem14 = function() {
  problem_num <- 14 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 7 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p14),
             correct_message = "",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p14$data, insure_subset),
             correct_message = "",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p14$mapping$x) == "age" || 
               quo_get_expr(p14$mapping$x) == "charges",
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 4,
             test = quo_get_expr(p14$mapping$y) == "age" || 
               quo_get_expr(p14$mapping$y) == "charges",
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 5,
             test = "GeomPoint" %in% class(p14$layers[[1]]$geom), 
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 6,
             test = !is.null(p14$labels$title), 
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 7,
             test = "GeomAbline" %in% class(p14$layers[[2]]$geom), 
             correct_message = "",
             error_message = "FAILED")
  
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
  max_scores[problem_num] <<- 2 # total pts possible
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
check_problem16 = function() {
  problem_num <- 16 # problem number
  max_scores[problem_num] <<- 2 # total pts possible
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
check_problem17 = function() {
  problem_num <- 17 # problem number
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(insure_better_model) == "lm",
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 2,
             test = "charges" %in% names(insure_better_model$model),
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 3,
             test = "age" %in% names(insure_better_model$model),
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 4,
             test = length(insure_better_model$model$charges) == 52,
             correct_message = "",
             error_message = "")
  
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
check_problem18 = function() {
  problem_num <- 18 # problem number
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 8 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p18),
             correct_message = "",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p18$data, insure_subset),
             correct_message = "",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p18$mapping$x) == "age" || 
               quo_get_expr(p18$mapping$x) == "charges",
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 4,
             test = quo_get_expr(p18$mapping$y) == "age" || 
               quo_get_expr(p18$mapping$y) == "charges",
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 5,
             test = "GeomPoint" %in% class(p18$layers[[1]]$geom), 
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 6,
             test = !is.null(p18$labels$title), 
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 7,
             test = "GeomAbline" %in% class(p18$layers[[2]]$geom), 
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 8,
             test = "GeomAbline" %in% class(p18$layers[[3]]$geom), 
             correct_message = "",
             error_message = "FAILED")
  
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
check_problem19 = function() {
  problem_num <- 19 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = typeof(insure_model_r2) == "double",
             correct_message = "",
             error_message = "Please assign a numeric to insure_model_r2")
  
  checkpoint(checkpoint_number = 2,
             test = insure_model_r2 > 0 && insure_model_r2 < 1,
             correct_message = "",
             error_message = "")
  
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
check_problem20 = function() {
  problem_num <- 20 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = typeof(insure_better_model_r2) == "double",
             correct_message = "",
             error_message = "Please assign a numeric to insure_better_model_r2")
  
  checkpoint(checkpoint_number = 2,
             test = insure_better_model_r2 > 0 && insure_better_model_r2 < 1,
             correct_message = "",
             error_message = "")
  
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
check_problem21 = function() {
  problem_num <- 21 # problem number
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(p21),
             correct_message = "",
             error_message = "Please assign a numeric to insure_better_model_r2")
  
  checkpoint(checkpoint_number = 2,
             test = typeof(p21$corr) == "double",
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 3,
             test = typeof(p21$corr_sq) == "double",
             correct_message = "",
             error_message = "")
  
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
check_problem22 = function() {
  problem_num <- 22 # problem number
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(p22),
             correct_message = "",
             error_message = "Please assign a numeric to insure_better_model_r2")
  
  checkpoint(checkpoint_number = 2,
             test = typeof(p22$corr) == "double",
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 3,
             test = typeof(p22$corr_sq) == "double",
             correct_message = "",
             error_message = "")
  
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
check_problem23 = function() {
  problem_num <- 23 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(insure_smokers),
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 1,
             test = nrow(insure_smokers) < 500 && nrow(insure_smokers) > 0,
             correct_message = "",
             error_message = "FAILED")
  
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
check_problem24 = function() {
  problem_num <- 24 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 6 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p24),
             correct_message = "",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p24$data, insure_smokers),
             correct_message = "",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p24$mapping$x) == "age" || 
               quo_get_expr(p24$mapping$x) == "charges",
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 4,
             test = quo_get_expr(p24$mapping$y) == "age" || 
               quo_get_expr(p24$mapping$y) == "charges",
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 5,
             test = "GeomPoint" %in% class(p24$layers[[1]]$geom), 
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 6,
             test = "FacetWrap" %in% class(p24$facet), 
             correct_message = "",
             error_message = "FAILED")
  
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
check_problem25 = function() {
  problem_num <- 25 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 6 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p25),
             correct_message = "",
             error_message = "You did not define a ggplot.")
  
  checkpoint(checkpoint_number = 2,
             test = identical(p25$data, insure_smokers),
             correct_message = "",
             error_message = "Did you use the right dataset?")
  
  checkpoint(checkpoint_number = 3,
             test = quo_get_expr(p25$mapping$x) == "age" || 
               quo_get_expr(p25$mapping$x) == "charges",
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 4,
             test = quo_get_expr(p25$mapping$y) == "age" || 
               quo_get_expr(p25$mapping$y) == "charges",
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 5,
             test = "GeomPoint" %in% class(p25$layers[[1]]$geom), 
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 6,
             test = "FacetWrap" %in% class(p25$facet), 
             correct_message = "",
             error_message = "FAILED")
  
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
check_problem26 = function() {
  problem_num <- 26 # problem number
  max_scores[problem_num] <<- 3 # total pts possible
  num_tests <<- 9 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(normal_mod) == "lm",
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 2,
             test = "charges" %in% names(normal_mod$model),
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 3,
             test = "age" %in% names(normal_mod$model),
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 4,
             test = class(overweight_mod) == "lm",
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 5,
             test = "charges" %in% names(overweight_mod$model),
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 6,
             test = "age" %in% names(overweight_mod$model),
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 7,
             test = class(obese_mod) == "lm",
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 8,
             test = "charges" %in% names(obese_mod$model),
             correct_message = "",
             error_message = "")
  
  checkpoint(checkpoint_number = 9,
             test = "age" %in% names(obese_mod$model),
             correct_message = "",
             error_message = "")
  
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
check_problem27 = function() {
  problem_num <- 27 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.numeric(p27),
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 1,
             test = p27 < 20000 && p27 > 10000,
             correct_message = "",
             error_message = "FAILED")
  
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
check_problem28 = function() {
  problem_num <- 28 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.numeric(p28),
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 1,
             test = p28 < 20000 && p28 > 10000,
             correct_message = "",
             error_message = "FAILED")
  
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
check_problem29 = function() {
  problem_num <- 29 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.numeric(p29),
             correct_message = "",
             error_message = "FAILED")
  
  checkpoint(checkpoint_number = 1,
             test = p29 < 40000 && p29 > 20000,
             correct_message = "",
             error_message = "FAILED")
  
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
check_problem30 = function() {
  problem_num <- 30 # problem number
  max_scores[problem_num] <<- 3 # total pts possible
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
capture.output(check_problem17(), file='NUL')
capture.output(check_problem18(), file='NUL')
capture.output(check_problem19(), file='NUL')
capture.output(check_problem20(), file='NUL')
capture.output(check_problem21(), file='NUL')
capture.output(check_problem22(), file='NUL')
capture.output(check_problem23(), file='NUL')
capture.output(check_problem24(), file='NUL')
capture.output(check_problem25(), file='NUL')
capture.output(check_problem26(), file='NUL')
capture.output(check_problem27(), file='NUL')
capture.output(check_problem28(), file='NUL')
capture.output(check_problem29(), file='NUL')
capture.output(check_problem30(), file='NUL')

