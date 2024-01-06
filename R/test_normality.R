#' Performs a check on normality
#'
#' @param x A numeric vector of data values.
#' @param data An optional data frame including `x` column.
#' @param alpha Maximum accepted p-value for significance tests.
#' @param alphacc Maximum accepted contingency coefficient for `jc` and `rc`.
#'
#' @return Returns a list of all results.
#'
#' @importFrom common spaces
#' @importFrom crayon bold green red
#' @importFrom dplyr filter
#' @importFrom graphics boxplot hist mtext par
#' @importFrom lawstat rjb.test
#' @importFrom moments agostino.test
#' @importFrom nortest ad.test cvm.test lillie.test pearson.test sf.test
#' @importFrom psych describe
#' @importFrom readr read_csv
#' @importFrom stats na.omit shapiro.test density qqline qqnorm sd
#' @importFrom stringr str_trim
#'
#' @export
#'
#' @examples
#' test_normality(ToothGrowth$len)
#' test_normality("len", ToothGrowth)
#' test_normality(ToothGrowth[["len"]])
#' test_normality(runif(233))
#' test_normality(rnorm(233))
test_normality <- function(x, data = "", alpha = .05, alphacc = .30) {
  # Initiate List to be returned --------------------------------------------

  return_list <- list()


  # Build param list --------------------------------------------------------

  # Check conditions and define params
  if (is.numeric(x)) {
    x_name <- deparse(substitute(x))
    if (grepl("[$]", x_name) == TRUE) {
      x_var_name <- gsub(".*[$]", "", x_name)
    } else {
      x_var_name <- gsub("\"", "", gsub(".*\\[([^]]+)\\].*", "\\1", x_name))
    }
    x <- x
  } else if (is.character(x) && is.data.frame(data)) {
    x_name <- paste0(deparse(substitute(data)), "$", x)
    x_var_name <- x
    x <- data[[x]]
  } else {
    stop("\n\tis.numeric(x) ist nicht TRUE")
  }

  return_list$param <- append(
    return_list$param,
    list(
      x_name = x_name,
      x_var_name = x_var_name,
      x = x,
      alpha = alpha
    )
  )


  # Describe data -----------------------------------------------------------

  describer <- psych::describe(x)
  return_list$descriptive <- append(
    return_list$descriptive,
    list(
      x = describer
    )
  )


  # Find nearest neighbour in normality table -------------------------------

  # Get the performance of tests
  test_performance <- normality_table |>
    dplyr::filter(
      normality_table$n == normality_table$n[
        which(
          abs(normality_table$n - length(x)) == min(abs(normality_table$n - length(x)))
        )[1]
      ]
    )


  # Run normality tests -----------------------------------------------------

  # Anderson-Darling
  if (test_performance$ad == test_performance$max) {
    test <- ad.test(x)
    return_list$test <- append(return_list$test, list(
      ad.test = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "ad",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # Cramer-von Mises
  if (test_performance$cv == test_performance$max) {
    test <- cvm.test(x)
    return_list$test <- append(return_list$test, list(
      cvm.test = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "cv",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # D'Agostino
  if (test_performance$da == test_performance$max) {
    test <- agostino.test(x, alternative = "two.sided")
    return_list$test <- append(return_list$test, list(
      agostino.test = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "da",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # Jarque–Bera (classic)
  if (test_performance$jb == test_performance$max) {
    test <- rjb.test(na.omit(x), option = "JB")
    return_list$test <- append(return_list$test, list(
      rjb.test.classic = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "jb",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # Jarque–Bera CC (classic)
  if (test_performance$jc == test_performance$max) {
    test <- rjb.test(na.omit(x), option = "JB")
    coefficient <- as.numeric(sqrt(test$statistic / (NROW(na.omit(x)) + test$statistic)))
    return_list$test <- append(return_list$test, list(
      rjb.test.classic.cc = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "jc",
        coefficient = coefficient,
        is.normal = ifelse(!is.null(coefficient) && coefficient <= .30, TRUE, FALSE)
      )
    ))
  }

  # Kolmogorov-Smirnov
  if (test_performance$ks == test_performance$max) {
    test <- ks.test(
      data, "pnorm",
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE)
    )
    return_list$test <- append(return_list$test, list(
      ks.test = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "ks",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # Lilliefors
  if (test_performance$ll == test_performance$max) {
    test <- lillie.test(x)
    return_list$test <- append(return_list$test, list(
      lillie.test = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "ll",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # Pearson chi-square
  if (test_performance$pf == test_performance$max) {
    test <- pearson.test(x, adjust = FALSE)
    return_list$test <- append(return_list$test, list(
      pearson.test = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "pf",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # Pearson chi-square (adjusted)
  if (test_performance$pt == test_performance$max) {
    test <- pearson.test(x, adjust = TRUE)
    return_list$test <- append(return_list$test, list(
      pearson.test.adj = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "pt",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # Jarque–Bera (robust)
  if (test_performance$rb == test_performance$max) {
    test <- rjb.test(na.omit(x), option = "RJB")
    return_list$test <- append(return_list$test, list(
      rjb.test.robust = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "rb",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # Jarque–Bera CC (robust)
  if (test_performance$rc == test_performance$max) {
    test <- rjb.test(na.omit(x), option = "RJB")
    coefficient <- as.numeric(sqrt(test$statistic / (NROW(na.omit(x)) + test$statistic)))
    return_list$test <- append(return_list$test, list(
      rjb.test.robust.cc = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "rc",
        coefficient = coefficient,
        is.normal = ifelse(!is.null(coefficient) && coefficient <= .30, TRUE, FALSE)
      )
    ))
  }

  # Shapiro-Francia
  if (test_performance$sf == test_performance$max) {
    test <- sf.test(x)
    return_list$test <- append(return_list$test, list(
      sf.test = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "sf",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # Shapiro-Wilk
  if (test_performance$sw == test_performance$max) {
    test <- shapiro.test(x)
    return_list$test <- append(return_list$test, list(
      shapiro.test = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "sw",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # Check if multiple tests were used
  is_multiple <- NROW(return_list$test)


  # Check if normality is TRUE or FALSE -------------------------------------

  # If multiple tests were used, declare all
  if (is_multiple > 1) {
    # Define counters
    loop_false <- 0
    loop_true <- 0

    # Loop through tests
    for (i in return_list$test) {
      # Count true/false
      loop_false <- loop_false + ifelse(i$is.normal == FALSE, 1, 0)
      loop_true <- loop_true + ifelse(i$is.normal == TRUE, 1, 0)
    }

    # Add is.normal result (bool)
    return_list <- append(return_list, list(
      is.normal = ifelse(loop_true > loop_false, TRUE, FALSE)
    ))

    rm(loop_false, loop_true)
  } else {
    # Add is.normal result (bool)
    return_list <- append(return_list, list(
      is.normal = ifelse(return_list$test[[1]]$is.normal, TRUE, FALSE)
    ))
  }
  return_list$is.normal <- ifelse(!is.na(return_list$is.normal) && !is.null(return_list$is.normal), return_list$is.normal, FALSE)


  # Reporting ---------------------------------------------------------------

  # Set report call
  return_list$report <- paste0("test_normality.report(test_normality(", return_list$param$x_name, ", alpha = ", alpha, "))")

  # If multiple tests were used, declare all
  if (NROW(return_list$test) > 1) {
    # Loop through tests
    build_result <- list()
    for (i in return_list$test) {
      # Update result
      build_result <- append(
        build_result,
        paste0(
          ifelse(return_list$is.normal == TRUE,
            paste0(spaces(9), green(bold("\u2714"))),
            paste0(spaces(13), red(bold("\u2717")))
          ),
          " ", str_trim(i$method), " (", i$method.alt, "), ", names(i$statistic), ifelse(!is.null(i$parameter), paste0("(", i$parameter, ")"), ""),
          " = ", round(i$statistic, 2), ", ", reportp(i$p.value), pstars(i$p.value, ls = TRUE),
          ifelse(!is.null(i$coefficient), paste0(", CC = ", round(i$coefficient, 2)), ""), "."
        )
      )
    }

    # Report
    return_list$result <- paste0(
      ifelse(return_list$is.normal == TRUE, green(paste0(bold("\u2714"), " (Normal)")), red(paste0(bold("\u2717"), " (Not normal)"))),
      " The distribution is considered ", ifelse(return_list$is.normal == TRUE, "normal", "not normal"), ".\n",
      paste(build_result, collapse = "\n"), "\n\n"
    )
    rm(build_result)
  } else {
    # Report
    return_list$result <- paste0(
      ifelse(return_list$is.normal == TRUE, green(paste0(bold("\u2714"), " (Normal)")), red(paste0(bold("\u2717"), " (Not normal)"))),
      " ", str_trim(return_list$test[[1]]$method), " (", return_list$test[[1]]$method.alt, "), ",
      names(return_list$test[[1]]$statistic), ifelse(!is.null(return_list$test[[1]]$parameter), paste0("(", return_list$test[[1]]$parameter, ")"), ""),
      " = ", round(return_list$test[[1]]$statistic, 2), ", ", reportp(return_list$test[[1]]$p.value), pstars(return_list$test[[1]]$p.value, ls = TRUE),
      ifelse(!is.null(return_list$test[[1]]$coefficient), paste0(", CC = ", round(return_list$test[[1]]$coefficient, 2)), ""), ".\n"
    )
  }


  # Plot --------------------------------------------------------------------

  return_list$plot <- paste0(
    "par(mfrow = c(2, 2))
    plot(density(c(", paste(na.omit(x), collapse = ","), ")), main = 'Density')
    hist(c(", paste(na.omit(x), collapse = ","), "), main = 'Histogram', xlab = '", return_list$param$x_var_name, "')
    qqnorm(c(", paste(na.omit(x), collapse = ","), "), main = 'Normal Q-Q Plot')
    qqline(c(", paste(na.omit(x), collapse = ","), "))
    boxplot(c(", paste(na.omit(x), collapse = ","), "), main = 'Boxplot', xlab = '", return_list$param$x_var_name, "', horizontal = TRUE)
    mtext('Distribution of ", return_list$param$x_var_name, "', side = 3, line = -1.25, font = 2, outer = TRUE)"
  )

  return(return_list)
}


# Reporting class for test_normality ------------------------------------------

#' Class to build a full report for test_normality
#'
#' @param object Object of test_normality function
#'
#' @return Returns a full test report with simple figures
#'
#' @export
#'
#' @examples
#' report(test_normality(ToothGrowth$len))
#' report(test_normality("len", ToothGrowth))
#' report(test_normality(ToothGrowth[["len"]]))
#' report(test_normality(runif(233)))
#' report(test_normality(rnorm(233)))
test_normality.report <- function(object) {
  headline(paste0("Testing ", object$param$x_var_name, " for normality"), 1)

  headline(paste0("Descriptive"), 2)
  rownames(object$descriptive$x) <- object$param$x_var_name
  print(as.data.frame(object$descriptive$x)[c(2:5, 8:9, 11:13)])

  headline(paste0("Normality test", if (NROW(names(object$test)) > 1) {
    "s"
  }), 2)
  for (i in names(object$test)) {
    print_htest(object$test[[i]])
    cat("\n")
  }

  headline("Summary", 2)
  for (i in names(object$test)) {
    cat(paste0("A ", object$test[[i]]$method, " (", object$test[[i]]$method.alt, ") was performed to assess the distribution of ", object$param$x_var_name, ".\n"))
    cat(paste0(
      "The test provided evidence that the distribution ", ifelse(object$test[[i]]$is.normal == TRUE, "did not depart ", "departed "), "significantly from normality, ",
      names(object$test[[i]]$statistic), ifelse(!is.null(object$test[[i]]$parameter), paste0("(", object$test[[i]]$parameter, ")"), ""), " = ", round(object$test[[i]]$statistic, 2), ", ", reportp(object$test[[i]]$p.value),
      ifelse(!is.null(object$test[[i]]$coefficient), paste0(", CC = ", round(object$test[[i]]$coefficient, 2)), ""), ".\n\n"
    ))
  }
  cat(paste0("The results indicate that the data is ", if (object$is.normal == FALSE) ("not "), "normally distributed.\n\n"))
  cat(object$result, "\n")

  report(object, elm = "plot")
}
