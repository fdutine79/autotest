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
#' @importFrom crayon bold green red yellow
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
#' test_normality(ToothGrowth["len"])
#' test_normality(runif(233))
test_normality <- function(x, data = "", alpha = .05, alphacc = .30) {
  # Initiate List to be returned --------------------------------------------

  return_list <- list()


  # Build param list --------------------------------------------------------

  # Check conditions and define params
  strctr <- c("numeric")
  params_list <- force_structure(sys.calls(), strctr)

  return_list$param <- append(
    params_list,
    list(
      alpha = alpha,
      alphacc = alphacc
    )
  )

  x <- return_list$param$x


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
          abs(
            normality_table$n - length(na.omit(x))
          ) == min(
            abs(normality_table$n - length(na.omit(x)))
          )
        )[1]
      ]
    )


  # Run normality tests -----------------------------------------------------

  # Anderson-Darling
  if (test_performance$ad == test_performance$max) {
    test <- ad.test(na.omit(x))
    return_list$tests <- append(return_list$tests, list(
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
    test <- cvm.test(na.omit(x))
    return_list$tests <- append(return_list$tests, list(
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
    test <- agostino.test(na.omit(x), alternative = "two.sided")
    return_list$tests <- append(return_list$tests, list(
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
    return_list$tests <- append(return_list$tests, list(
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
    coefficient <- as.numeric(
      sqrt(test$statistic / (NROW(na.omit(x)) + test$statistic))
    )
    return_list$tests <- append(return_list$tests, list(
      rjb.test.classic.cc = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "jc",
        coefficient = coefficient,
        is.normal = ifelse(
          !is.null(coefficient) && coefficient <= alphacc,
          TRUE,
          FALSE
        )
      )
    ))
  }

  # Kolmogorov-Smirnov
  if (test_performance$ks == test_performance$max) {
    test <- ks.test(
      na.omit(x), "pnorm",
      mean = mean(na.omit(x), na.rm = TRUE),
      sd = sd(na.omit(x), na.rm = TRUE)
    )
    return_list$tests <- append(return_list$tests, list(
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
    test <- lillie.test(na.omit(x))
    return_list$tests <- append(return_list$tests, list(
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
    test <- pearson.test(na.omit(x), adjust = FALSE)
    return_list$tests <- append(return_list$tests, list(
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
    test <- pearson.test(na.omit(x), adjust = TRUE)
    return_list$tests <- append(return_list$tests, list(
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
    return_list$tests <- append(return_list$tests, list(
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
    coefficient <- as.numeric(
      sqrt(test$statistic / (NROW(na.omit(x)) + test$statistic))
    )
    return_list$tests <- append(return_list$tests, list(
      rjb.test.robust.cc = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "rc",
        coefficient = coefficient,
        is.normal = ifelse(
          !is.null(coefficient) && coefficient <= alphacc,
          TRUE,
          FALSE
        )
      )
    ))
  }

  # Shapiro-Francia
  if (test_performance$sf == test_performance$max) {
    test <- sf.test(na.omit(x))
    return_list$tests <- append(return_list$tests, list(
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
    test <- shapiro.test(na.omit(x))
    return_list$tests <- append(return_list$tests, list(
      shapiro.test = c(
        test,
        p.stars = pstars(test$p.value),
        method.alt = "sw",
        is.normal = ifelse(test$p.value >= alpha, TRUE, FALSE)
      )
    ))
  }

  # Check if multiple tests were used
  is_multiple <- NROW(return_list$tests)


  # Check if normality is TRUE or FALSE -------------------------------------

  # If multiple tests were used, declare all
  if (is_multiple > 1) {
    # Define counters
    loop_false <- 0
    loop_true <- 0

    # Loop through tests
    for (i in return_list$tests) {
      # Count true/false
      loop_false <- loop_false + ifelse(i$is.normal == FALSE, 1, 0)
      loop_true <- loop_true + ifelse(i$is.normal == TRUE, 1, 0)
    }

    # Add is.normal result (bool)
    if (loop_true == loop_false) {
      par(mfrow = c(2, 2))
      plot(density(x), main = "Density")
      hist(x, main = "Histogram", xlab = return_list$param$x_var_name)
      qqnorm(x, main = "Normal Q-Q Plot")
      qqline(x)
      boxplot(x, main = "Boxplot", xlab = return_list$param$x_var_name, horizontal = TRUE)
      mtext("Decision template of normality", side = 3, line = -1.25, font = 2, outer = TRUE)

      cat(paste0(yellow(bold("\u26A0"), "(Warning)"), "\tA manual decision is required\n"))
      cat(paste0(
        "\nThe tests do not agree, whether the distribution is normal. Please analyse the\n",
        "distribution manually and decide.\n\n"
      ))

      print(describer)

      prmt <- readline(prompt = cat(paste0(
        "\n\n",
        "1: Normal\n",
        "2: Not normal"
      )))
      return_list <- append(return_list, list(
        is.normal = ifelse(prmt == 1, TRUE, FALSE)
      ))
      rm(prmt)
    } else {
      return_list <- append(return_list, list(
        is.normal = ifelse(loop_true > loop_false, TRUE, FALSE)
      ))
    }

    rm(loop_false, loop_true)
  } else {
    # Add is.normal result (bool)
    return_list <- append(return_list, list(
      is.normal = ifelse(return_list$tests[[1]]$is.normal, TRUE, FALSE)
    ))
  }
  return_list$is.normal <- ifelse(
    !is.na(return_list$is.normal) && !is.null(return_list$is.normal),
    return_list$is.normal,
    FALSE
  )


  # Reporting ---------------------------------------------------------------

  # Set report call
  return_list$report <- paste0(
    "test_normality.report(test_normality(",
    return_list$param$x_name, ", ",
    "alpha = ", alpha, ", ",
    "alphacc = ", alphacc,
    "))"
  )

  # If multiple tests were used, declare all
  if (is_multiple > 1) {
    # Loop through tests
    build_result <- list()
    for (i in return_list$tests) {
      # Update result
      build_result <- append(
        build_result,
        paste0(
          ifelse(i$is.normal == TRUE,
            paste0(spaces(9), green(bold("\u2714"))),
            paste0(spaces(13), red(bold("\u2717")))
          ),
          " ", str_trim(i$method), " (", i$method.alt, "), ",
          paste0(
            names(i$statistic),
            ifelse(!is.null(i$parameter), paste0("(", i$parameter, ")"), ""),
            " = ", round(i$statistic, 2),
            collapse = ", "
          ), ", ", reportp(i$p.value), pstars(i$p.value, ls = TRUE),
          ifelse(
            !is.null(i$coefficient),
            paste0(", CC = ", round(i$coefficient, 2)),
            ""
          ), "."
        )
      )
    }

    # Report
    return_list$result <- paste0(
      resultcol(return_list$is.normal, "n"),
      "The distribution is considered ", ifelse(
        return_list$is.normal == TRUE,
        "normal",
        "not normal"
      ), ".\n",
      paste(build_result, collapse = "\n"), "\n\n"
    )
    rm(build_result)
  } else {
    # Report
    return_list$result <- paste0(
      resultcol(return_list$is.normal, "n"),
      str_trim(return_list$tests[[1]]$method),
      " (", return_list$tests[[1]]$method.alt, "), ",
      paste0(
        names(return_list$tests[[1]]$statistic),
        ifelse(
          !is.null(return_list$tests[[1]]$parameter),
          paste0("(", return_list$tests[[1]]$parameter, ")"),
          ""
        ),
        " = ", round(return_list$tests[[1]]$statistic, 2),
        collapse = ", "
      ), ", ", reportp(return_list$tests[[1]]$p.value),
      pstars(return_list$tests[[1]]$p.value, ls = TRUE),
      ifelse(
        !is.null(return_list$tests[[1]]$coefficient),
        paste0(", CC = ", round(return_list$tests[[1]]$coefficient, 2)),
        ""
      ), ".\n"
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

  headline(paste0("Normality test", if (NROW(names(object$tests)) > 1) {
    "s"
  }), 2)
  for (i in names(object$tests)) {
    print_htest(object$tests[[i]])
    cat("\n")
  }

  headline("Summary", 2)
  for (i in names(object$tests)) {
    cat(paste0("A ", object$tests[[i]]$method, " (", object$tests[[i]]$method.alt, ") was performed to assess the distribution of ", object$param$x_var_name, ".\n"))
    cat(paste0(
      "The test provided evidence that the distribution ", ifelse(
        object$tests[[i]]$is.normal == TRUE,
        "did not depart ",
        "departed "
      ), "significantly from normality, ",
      names(object$tests[[i]]$statistic), ifelse(
        !is.null(object$tests[[i]]$parameter),
        paste0("(", object$tests[[i]]$parameter, ")"),
        ""
      ), " = ", round(object$tests[[i]]$statistic, 2), ", ", reportp(object$tests[[i]]$p.value),
      ifelse(
        !is.null(object$tests[[i]]$coefficient),
        paste0(", CC = ", round(object$tests[[i]]$coefficient, 2)),
        ""
      ), ".\n\n"
    ))
  }
  cat(paste0("The results indicate that the data is ", if (object$is.normal == FALSE) ("not "), "normally distributed.\n\n"))
  cat(object$result, "\n")

  report(object, elm = "plot")
}
