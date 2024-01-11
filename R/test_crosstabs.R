# Function test_crosstabs -------------------------------------------------

#' Perform a crosstabs analysis
#'
#' @param x A numeric vector or a factor.
#' @param y A numeric vector or a factor of the same length as `x`.
#' @param data An optional data frame including `x` and `y` columns.
#' @param alternative Directed hypotheses; greater means `group 1 > group 2` (ignored for df > 1).
#' @param alpha Maximum accepted p-value.
#'
#' @return Returns a list of all results.
#' @seealso NCmisc::list.functions.in.file(filename = rstudioapi::getSourceEditorContext()$path)
#' @importFrom crayon bold green red
#' @importFrom effectsize oddsratio
#' @importFrom graphics mosaicplot
#' @importFrom RcmdrMisc colPercents
#' @importFrom stats addmargins chisq.test fisher.test
#' @importFrom stringr str_trim
#' @importFrom vcd assocstats
#' @export
#'
#' @examples
#' test_crosstabs("cyl", "vs", mtcars)
#' test_crosstabs(mtcars$cyl, mtcars$vs)
#' test_crosstabs(mtcars[["cyl"]], mtcars[["vs"]])
test_crosstabs <- function(x, y, data = "", alternative = "two.sided", alpha = .05) {
  # Initiate List to be returned -------------------------------------------

  return_list <- list()


  # Build param list --------------------------------------------------------

  # Check conditions and define params
  if (!is.data.frame(data)) {
    x_name <- deparse(substitute(x))
    y_name <- deparse(substitute(y))
    if (grepl("[$]", x_name) == TRUE) {
      x_var_name <- gsub(".*[$]", "", x_name)
    } else {
      x_var_name <- gsub("\"", "", gsub(".*\\[([^]]+)\\].*", "\\1", x_name))
    }
    if (grepl("[$]", y_name) == TRUE) {
      y_var_name <- gsub(".*[$]", "", y_name)
    } else {
      y_var_name <- gsub("\"", "", gsub(".*\\[([^]]+)\\].*", "\\1", y_name))
    }
    x <- as.character(x)
    y <- as.character(y)
  } else {
    x_name <- paste0(deparse(substitute(data)), "$", x)
    y_name <- paste0(deparse(substitute(data)), "$", y)
    x_var_name <- x
    y_var_name <- y
    x <- as.character(data[[x]])
    y <- as.character(data[[y]])
  }

  return_list$param <- append(
    return_list$param,
    list(
      x_name = x_name,
      y_name = y_name,
      x_var_name = x_var_name,
      y_var_name = y_var_name,
      x = x,
      y = y,
      alternative = alternative,
      alpha = alpha
    )
  )


  # Describe data -----------------------------------------------------------

  tab <- table(x, y, dnn = c(x_var_name, y_var_name))
  df <- (nrow(tab) - 1) * (ncol(tab) - 1)
  N <- max(NROW(x), NROW(y))
  correct <- ifelse(N < 40, TRUE, FALSE)
  expected <- suppressWarnings(chisq.test(tab, correct = correct)$expected)
  difference <- tab - expected

  # Force alternative to 'two.sided', if df > 1
  if (df > 1) {
    return_list$param$alternative <- "two.sided"
    alternative <- "two.sided"
  }

  return_list$descriptive <- append(
    return_list$descriptive,
    list(
      x = list(
        n = NROW(x)
      ),
      y = list(
        n = NROW(y)
      ),
      x_by_y = list(
        tab = tab,
        df = df,
        N = N,
        absolute = addmargins(tab),
        relative = colPercents(tab),
        expected = expected,
        difference = difference
      )
    )
  )


  # Set requirements --------------------------------------------------------

  requirements <- TRUE

  if (min(expected) < 5) {
    requirements <- FALSE
  }

  # Update requirements
  return_list$reqs$min.exp <- min(expected)


  # Run Test ----------------------------------------------------------------

  if (requirements == FALSE) {
    # Fisher's exact test ---------------------------------------------------

    test <- fisher.test(
      tab,
      alternative = alternative,
      hybrid = ifelse(df > 1, TRUE, FALSE),
      conf.int = TRUE,
      conf.level = 1 - alpha,
      simulate.p.value = TRUE
    )

    # Update test
    return_list$test <- append(
      return_list$test,
      list(
        fisher.test = c(
          test,
          p.stars = pstars(test$p.value),
          method.alt = "Fisher"
        )
      )
    )
  } else {
    # Pearson's Chi-squared test --------------------------------------------

    test <- suppressWarnings(chisq.test(
      tab,
      correct = correct
    ))

    # Check for directed hypothesis
    # only for 2x2 matrix (df = 1)
    if (alternative != "two.sided" && df == 1) {
      if (
        (alternative == "greater" && difference[1] > difference[3]) ||
          (alternative == "less" && difference[1] < difference[3])
      ) {
        test$p.value <- test$p.value / 2
      } else {
        test$p.value <- pmin(1 - (test$p.value / 2), 1)
      }
    }

    # Update test
    return_list$test <- append(
      return_list$test,
      list(
        chisq.test = c(
          test,
          p.stars = pstars(test$p.value),
          method.alt = "Pearson"
        )
      )
    )
  }


  # Get Effect Size ---------------------------------------------------------

  p <- test$p.value

  if (is.na(p)) {
    warning("\n\tp.value is NA. Result cannot be calculated")
  } else {
    # Add is.significant result (bool)
    return_list <- append(return_list, list(
      is.significant = ifelse(p < alpha, TRUE, FALSE)
    ))

    # Only for Pearson chisq.test as Fisher has no effect size
    if (return_list$test[[1]]$method.alt == "Pearson") {
      # Find Magnitude
      coeff <- assocstats(tab)$contingency # Contingency Coefficient
      coeff_max <- sqrt((min(ncol(tab), nrow(tab)) - 1) / min(ncol(tab), nrow(tab))) # Maximum Coefficient
      coeff_cor <- coeff / coeff_max # Corrected Coefficient

      if (coeff_cor < 0.25) {
        magnitude <- "Small"
      } else if (coeff_cor < 0.66) {
        magnitude <- "Medium"
      } else {
        magnitude <- "Large"
      }

      # Build translate list
      translate_list <- list()
      translate_list <- append(
        translate_list,
        list(
          cc = assocstats(tab)$contingency,
          phi = assocstats(tab)$phi,
          v = assocstats(tab)$cramer,
          w = sqrt(return_list$test[[1]]$statistic / N)
        )
      )
      if (df == 1) {
        translate_list <- append(
          translate_list,
          list(
            or = effectsize::oddsratio(tab, ci = 1 - alpha)
          )
        )
      }
      for (i in c("d", "r", "eta", "f", "chi", "z")) {
        translate_list[i] <- effsize_translate(return_list$test[[1]]$statistic, "chi", i, N)
      }

      # Update estimate
      return_list$test[[1]]$estimate <- append(
        return_list$test[[1]]$estimate,
        list(
          magnitude = magnitude,
          cc.corr = coeff_cor,
          translate = translate_list
        )
      )
      rm(translate_list, magnitude)
    }


    # Reporting -------------------------------------------------------------

    # Set report call
    return_list$report <- paste0(
      "test_crosstabs.report(test_crosstabs(", return_list$param$x_name, ", ", return_list$param$y_name, ", alternative = '", alternative, "', alpha = ", alpha, "))"
    )

    return_list$result <- paste0(
      ifelse(p < alpha, green(paste0(bold("\u2713"), " (Significant)\t")), red(paste0(bold("\u2717"), " (Not signif.)\t"))),
      gsub("\\n\t", "", str_trim(return_list$test[[1]]$method)),
      " (", return_list$test[[1]]$method.alt, ")",
      ifelse(return_list$test[[1]]$method.alt == "Pearson",
        paste0(", X2(", return_list$test[[1]]$parameter, ") = ", format(round(as.numeric(return_list$test[[1]]$statistic), 2), nsmall = 2)),
        ""
      ),
      ", ", reportp(p), pstars(p, ls = TRUE),
      ifelse(return_list$test[[1]]$method.alt == "Pearson",
        paste0(
          ", CCcorr = ", format(round(as.numeric(return_list$test[[1]]$estimate$cc.corr), 2), nsmall = 2),
          " (", return_list$test[[1]]$estimate$magnitude, ")", "\n"
        ),
        ifelse(df == 1, paste0("OR = ", return_list$test[[1]]$estimate$`odds ratio`, "\n"), "\n")
      )
    )


    # Plot --------------------------------------------------------------------

    return_list$plot <- paste0(
      "mosaicplot(
        table(c(", paste(paste0('\"', return_list$param$x, '\"'), collapse = ","), "), c(", paste(paste0('\"', return_list$param$y, '\"'), collapse = ","), ")),
        main = 'Crosstabs Testing: ", return_list$param$x_var_name, ", ", return_list$param$y_var_name, "',
        xlab = '", return_list$param$x_var_name, "',
        ylab = '", return_list$param$y_var_name, "',
        color = FALSE)"
    )
  }

  return(return_list)
}


# Reporting class for test_crosstabs ---------------------------------------

#' Class to build a full report for test_crosstabs
#'
#' @param object Object of test_crosstabs function.
#'
#' @return Returns a full test report with simple figures.
#' @seealso NCmisc::list.functions.in.file(filename = rstudioapi::getSourceEditorContext()$path)
#' @importFrom common spaces
#' @importFrom crayon bold green red
#' @export
#'
#' @examples
#' report(test_crosstabs("cyl", "vs", mtcars))
#' report(test_crosstabs(mtcars$cyl, mtcars$vs))
#' report(test_crosstabs(mtcars[["cyl"]], mtcars[["vs"]]))
test_crosstabs.report <- function(object) {
  headline(paste0("Crosstabs Testing: '", object$param$x_var_name, "', '", object$param$y_var_name, "'"), 1)

  headline(paste0("Descriptive"), 2)
  headline(paste0("Observed frequency"), 3)
  print(object$descriptive$x_by_y$absolute)
  headline(paste0("Observed relative frequency"), 3)
  print(object$descriptive$x_by_y$relative)
  headline(paste0("Expected frequency"), 3)
  print(object$descriptive$x_by_y$expected)

  headline(paste0("Test Requirements"), 2)
  cat(paste0(
    "Minimum expected frequency: ", spaces(calc_space(
      paste0("Minimum expected frequency: "), c(), 33
    )), ifelse(object$reqs$min.exp >= 5, paste0(green(bold("\u2714"), "(n >= 5) ")), paste0(red(bold("\u2717"), "(n < 5) "))),
    format(round(as.numeric(object$reqs$min.exp), 2), nsmall = 2)
  ), "\n")

  headline(paste0(gsub("\\n\t", "", str_trim(object$test[[1]]$method)), " (", object$test[[1]]$method.alt, ")"), 2)
  print_htest(object$test[[1]])

  headline(paste0("Delta observed/expected frequency"), 2)
  print(object$descriptive$x_by_y$difference)
  if (object$is.significant == TRUE && object$descriptive$x_by_y$df == 1) {
    if (object$test[[1]]$method.alt == "Pearson") {
      cat("\n")
      print(object$test[[1]]$estimate$translate$or)
    } else {
      cat("\n")
      print(object$test[[1]]$estimate$`odds ratio`)
    }
  }

  headline("Summary", 2)
  cat(paste0("A ", object$test[[1]]$method.alt, "-test of independence was computed to assess the relationship between ", object$param$x_var_name, " and ", object$param$y_var_name, ".\n"))
  cat(paste0(
    "The assumption was that ",
    ifelse(object$param$alternative == "two.sided",
      paste0("there are differences among ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[2], " groups '", names(object$descriptive$x_by_y$difference[1, ][1]), "' and '", names(object$descriptive$x_by_y$difference[1, ][2]), "' regarding ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[1], "."),
      ifelse(object$param$alternative == "greater",
        paste0(colnames(as.data.frame(object$descriptive$x_by_y$difference))[1], " '", names(object$descriptive$x_by_y$difference[, 1][1]), "' is more likely among ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[2], " group '", names(object$descriptive$x_by_y$difference[1, ][1]), "' than in ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[2], " group '", names(object$descriptive$x_by_y$difference[1, ][2]), "'."),
        paste0(colnames(as.data.frame(object$descriptive$x_by_y$difference))[1], " '", names(object$descriptive$x_by_y$difference[, 1][1]), "' is more likely among ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[2], " group '", names(object$descriptive$x_by_y$difference[1, ][2]), "' than in ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[2], " group '", names(object$descriptive$x_by_y$difference[1, ][1]), "'.")
      )
    ), "\n"
  ))
  cat(paste0("The relationship was ", ifelse(object$is.significant == FALSE, "not ", ""), "significant", ifelse(object$test[[1]]$method.alt == "Pearson" && object$is.significant == TRUE, paste0(" with ", tolower(object$test[[1]]$estimate$magnitude), " effects"), ""), ".\n"))
  if (object$descriptive$x_by_y$df == 1 && object$is.significant == TRUE) {
    or_from_test <- ifelse(
      object$test[[1]]$method.alt == "Pearson",
      format(round(as.numeric(object$test[[1]]$estimate$translate$or$Odds_ratio), 2), nsmall = 2),
      format(round(as.numeric(object$test[[1]]$estimate$`odds ratio`), 2), nsmall = 2)
    )

    if (object$descriptive$x_by_y$difference[1] > object$descriptive$x_by_y$difference[3]) {
      cat(paste0(firstup(colnames(as.data.frame(object$descriptive$x_by_y$difference))[2]), " '", names(object$descriptive$x_by_y$difference[1, ][1]), "' for ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[1], " '", names(object$descriptive$x_by_y$difference[, 1][1]), "' is ", or_from_test, " times more likely than ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[2], " '", names(object$descriptive$x_by_y$difference[1, ][2]), "' for ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[1], " '", names(object$descriptive$x_by_y$difference[, 1][1]), "'.\n"))
    } else {
      cat(paste0(firstup(colnames(as.data.frame(object$descriptive$x_by_y$difference))[2]), " '", names(object$descriptive$x_by_y$difference[1, ][2]), "' for ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[1], " '", names(object$descriptive$x_by_y$difference[, 1][1]), "' is ", or_from_test, " times more likely than ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[2], " '", names(object$descriptive$x_by_y$difference[1, ][1]), "' for ", colnames(as.data.frame(object$descriptive$x_by_y$difference))[1], " '", names(object$descriptive$x_by_y$difference[, 1][1]), "'.\n"))
    }
  }
  cat("\n")

  cat(object$result, "\n")

  report(object, elm = "plot")
}
