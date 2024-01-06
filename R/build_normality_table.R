# Function build_normality_table ------------------------------------------

#' Builds the normality table
#'
#' @description
#' The normality table identifies the best performing normality tests depending
#' on sample size. Sample sizes are referenced by fibonacci numbers. The optimal
#' test can then be applied by selecting the best performing test at the nearest
#' sample size neighbor in comparison to the maximum achievement.
#'
#' Current normality tests included are:
#' * Anderson-Darling
#' * Cramer-von Mises
#' * D'Agostino
#' * Jarque–Bera (classic, robust, with and without contingency coefficient)
#' * Kolmogorov-Smirnov
#' * Lilliefors
#' * Pearson chi-square (with and without adjustment)
#' * Shapiro-Francia
#' * Shapiro-Wilk
#'
#' @param rounds Number of round, the tests shall run.
#' @param fibonacci Number of fibonacci patterns to be applied (min = 5).
#' @param alpha Maximum accepted p-value.
#' @param alphacc Maximum accepted contingency coefficient for `jc` and `rc`.
#' @param test If TRUE a test is assumed. Prompting and saving are omitted.
#'
#' @return Returns a data frame.
#'
#' @importFrom numbers fibonacci
#' @importFrom stats rbeta rchisq rnorm runif sd
#' @importFrom usethis use_data
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @export
#'
#' @examples
#' normality_table <- build_normality_table(rounds = 2, fibonacci = 7, test = TRUE)
build_normality_table <- function(rounds = 100, fibonacci = 30, alpha = .05, alphacc = .30, test = FALSE) {
  # Check if certain
  if (test == FALSE) {
    prmt <- readline(prompt = cat(paste0(
      "\nBuilding the normality table might take a long time. With recommended\n",
      "settings, rebuilding will take about 1.12 hours. Are you sure, you want\n",
      "to rebuild the normality table?\n\n",
      "1: Yes\n",
      "2: No"
    )))

    if (as.integer(prmt) == 2) {
      return()
    }
  }

  # Set definitions
  start_time <- Sys.time()
  tests <- c("ad", "cv", "da", "jb", "jc", "ks", "ll", "pf", "pt", "rb", "rc", "sf", "sw")
  if (fibonacci < 5) {
    fibonacci <- 5
  }
  fibo <- fibonacci(fibonacci, sequence = TRUE)
  fibo <- fibo[fibo > 3]
  loop_counter <- 0
  mean_counter <- 0
  test_data_length <- 8

  # Define list
  normality_table <- data.frame()
  for (s in 1:length(fibo)) {
    normality_table <- rbind(normality_table, c(fibo[s], rep(0, length(tests) + 1)))
  }
  colnames(normality_table) <- c("n", tests, "max")

  # Set progress bar
  progress_bar <- utils::txtProgressBar(min = 0, max = rounds * length(fibo) * length(tests) * test_data_length, style = 3, char = "=")

  for (i in 1:rounds) {
    for (s in 1:length(fibo)) {
      # Define the test data
      test_data <- list(
        "TRUE" = rnorm(fibo[s]),
        "TRUE" = rbeta(fibo[s], 5, 5),
        "TRUE" = rbeta(fibo[s], 4, 6),
        "TRUE" = rbeta(fibo[s], 6, 4),
        "FALSE" = rbeta(fibo[s], 10, 2),
        "FALSE" = rbeta(fibo[s], 2, 10),
        "FALSE" = -rchisq(fibo[s], runif(1, min = 1, max = 5)),
        "FALSE" = rchisq(fibo[s], runif(1, min = 1, max = 5))
      )
      if (test_data_length != length(test_data)) {
        stop("Defined `test_data_length` does not match actual `length(test_data)`")
      }

      for (t in 1:length(tests)) {
        # Run tests
        if (
          # Special conditions for tests
          (tests[t] == "ad" && fibo[s] <= 7) ||
            (tests[t] == "cv" && fibo[s] <= 7) ||
            (tests[t] == "da" && fibo[s] <= 8) ||
            (tests[t] == "sf" && fibo[s] <= 4) ||
            (tests[t] == "da" && fibo[s] >= 46340) ||
            (tests[t] == "sw" && fibo[s] >= 5000) ||
            (tests[t] == "sf" && fibo[s] >= 5000)
        ) {
          # Do nothing

          # Update loop counter
          loop_counter <- loop_counter + length(test_data)
        } else {
          # Get test results
          for (td in 1:length(test_data)) {
            normality_table[s, t + 1] <- normality_table[s, t + 1] + as.integer(get_result(
              tests[t], test_data[[td]], as.logical(names(test_data[td])),
              alpha = alpha, alphacc = alphacc
            ))

            # Update loop counter
            loop_counter <- loop_counter + 1
          }
        }

        # Update progress bar
        setTxtProgressBar(progress_bar, value = loop_counter)
      }

      # Clean up
      rm(test_data)
    }

    # Update mean counter
    mean_counter <- mean_counter + test_data_length
  }

  # Clean up
  close(progress_bar)
  rm(loop_counter)

  # Calculate results
  normality_table[2:(length(tests) + 1)] <- normality_table[2:length(tests)] / mean_counter
  normality_table$max <- apply(normality_table[, 2:(length(tests) + 1)], 1, max)
  rm(mean_counter)

  # Print time used
  end_time <- Sys.time()
  print(round(end_time - start_time, 2))
  rm(start_time, end_time)

  # Save the table
  if (test == FALSE) {
    usethis::use_data(normality_table, overwrite = TRUE)
  }

  return(normality_table)
}


# Function get_result ----------------------------------------------------

#' Get the result of a normality test
#'
#' @description
#' Returns 1 if the test has confirmed the expected result.
#' Returns 0 if the test did not confirm the expected result.
#'
#' @param test Shorthand of the test to run.
#' @param data A numeric vector of data values. Missing values are allowed.
#' @param expected Boolean of expected normality (TRUE|FALSE).
#' @param alpha Maximum accepted p-value.
#' @param alphacc Maximum accepted contingency coefficient for `jc` and `rc`.
#'
#' @return Integer 0|1.
#'
#' @export
#'
#' @examples
#' get_result("sf", rnorm(233), TRUE)
#' get_result("rc", rbeta(233, 10, 2), TRUE)
get_result <- function(test, data, expected, alpha = .05, alphacc = .30) {
  if (run_test(test, data, alpha = alpha, alphacc = alphacc) == expected) {
    return(1)
  }
  return(0)
}


# Function run_test -------------------------------------------------------

#' Runs a normality test
#'
#' @param test Shorthand of the test to run.
#' @param data A numeric vector of data values. Missing values are allowed.
#' @param alpha Maximum accepted p-value.
#' @param alphacc Maximum accepted contingency coefficient for `jc` and `rc`.
#'
#' @return Boolean if normal.
#'
#' @importFrom nortest ad.test cvm.test lillie.test pearson.test sf.test
#' @importFrom stats ks.test na.omit shapiro.test
#' @importFrom lawstat rjb.test
#' @importFrom moments agostino.test
#'
#' @export
#'
#' @examples
#' run_test("ad", rnorm(100))
#' run_test("cv", runif(100))
#' run_test("jc", rbeta(100, 10, 2))
#' run_test("rc", rbeta(100, 2, 10))
run_test <- function(test, data, alpha = .05, alphacc = .30) {
  if (test == "ad") { # Anderson Darling
    is_normal <- suppressWarnings(
      ifelse(ad.test(data)$p.value < alpha, FALSE, TRUE)
    )
  } else if (test == "cv") { # Cramer-von Mises
    is_normal <- suppressWarnings(
      ifelse(cvm.test(data)$p.value < alpha, FALSE, TRUE)
    )
  } else if (test == "da") { # D'Agostino
    is_normal <- suppressWarnings(
      ifelse(agostino.test(data, alternative = "two.sided")$p.value < alpha, FALSE, TRUE)
    )
  } else if (test == "jb") { # Jarque–Bera (classic)
    is_normal <- suppressWarnings(
      ifelse(rjb.test(na.omit(data), option = "JB")$p.value < alpha, FALSE, TRUE)
    )
  } else if (test == "jc") { # Jarque–Bera CC (classic)
    is_normal <- suppressWarnings(
      ifelse(as.numeric(sqrt(
        rjb.test(na.omit(data), option = "JB")$statistic / (
          length(na.omit(data)) + rjb.test(na.omit(data), option = "JB")$statistic
        )
      )) < alphacc, TRUE, FALSE)
    )
  } else if (test == "ks") { # Kolmogorov-Smirnov
    is_normal <- suppressWarnings(
      ifelse(ks.test(
        data, "pnorm",
        mean = mean(data, na.rm = TRUE),
        sd = sd(data, na.rm = TRUE)
      )$p.value < alpha, FALSE, TRUE)
    )
  } else if (test == "ll") { # Lilliefors
    is_normal <- suppressWarnings(
      ifelse(lillie.test(data)$p.value < alpha, FALSE, TRUE)
    )
  } else if (test == "pf") { # Pearson chi-square
    is_normal <- suppressWarnings(
      ifelse(pearson.test(data, adjust = FALSE)$p.value < alpha, FALSE, TRUE)
    )
  } else if (test == "pt") { # Pearson chi-square (adjusted)
    is_normal <- suppressWarnings(
      ifelse(pearson.test(data, adjust = TRUE)$p.value < alpha, FALSE, TRUE)
    )
  } else if (test == "rb") { # Jarque–Bera (robust)
    is_normal <- suppressWarnings(
      ifelse(rjb.test(na.omit(data), option = "RJB")$p.value < alpha, FALSE, TRUE)
    )
  } else if (test == "rc") { # Jarque–Bera CC (robust)
    is_normal <- suppressWarnings(
      ifelse(as.numeric(sqrt(
        rjb.test(na.omit(data), option = "RJB")$statistic / (
          length(na.omit(data)) + rjb.test(na.omit(data), option = "RJB")$statistic
        )
      )) < alphacc, TRUE, FALSE)
    )
  } else if (test == "sf") { # Shapiro-Francia
    is_normal <- suppressWarnings(
      ifelse(sf.test(data)$p.value < alpha, FALSE, TRUE)
    )
  } else if (test == "sw") { # Shapiro-Wilk
    is_normal <- suppressWarnings(
      ifelse(shapiro.test(data)$p.value < alpha, FALSE, TRUE)
    )
  }

  return(is_normal)
}
