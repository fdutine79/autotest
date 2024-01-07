# Function test_ttest -----------------------------------------------------

#' Perform a parametric T-/Welch-Test or a non-parametric Wilcoxon/MWU test
#'
#' @param x Name of column; dependent variable; metric values.
#' @param y Name of grouping column; independent variable; categorical.
#' @param data An optional data frame including `x` and `y` columns.
#' @param paired TRUE if dependent groups, else `FALSE`.
#' @param alternative Directed hypotheses; greater means `group 1 > group 2`.
#' @param alpha Maximum accepted p-value.
#'
#' @return Returns a list of all results.
#' @seealso NCmisc::list.functions.in.file(filename = rstudioapi::getSourceEditorContext()$path)
#' @importFrom car leveneTest
#' @importFrom crayon bold green red
#' @importFrom dplyr filter select tibble
#' @importFrom effsize cohen.d
#' @importFrom graphics boxplot par
#' @importFrom psych describeBy
#' @importFrom stats t.test qnorm wilcox.test
#' @importFrom stringr str_trim
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
#' test_ttest("len", "supp", ToothGrowth)
#' test_ttest(ToothGrowth$len, ToothGrowth$supp)
#' test_ttest(ToothGrowth[["len"]], ToothGrowth[["supp"]])
test_ttest <- function(x, y, data = "", paired = FALSE, alternative = "two.sided", alpha = .05) {
  # Initiate List to be returned --------------------------------------------

  return_list <- list()


  # Build param list --------------------------------------------------------

  # Check conditions and define params
  if (is.numeric(x) && !is.data.frame(data)) {
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
    x <- x
    y <- as.factor(y)
  } else if (is.character(x) && is.character(y) && is.data.frame(data)) {
    x_name <- paste0(deparse(substitute(data)), "$", x)
    y_name <- paste0(deparse(substitute(data)), "$", y)
    x_var_name <- x
    y_var_name <- y
    x <- data[[x]]
    y <- as.factor(data[[y]])
  } else {
    stop("\n\tis.numeric(x) ist nicht TRUE")
  }
  if (NROW(unique(y)) != 2) {
    stop("\n\tGrouping factor must have exactly 2 levels")
  }
  if (length(unique(x)) == 1) {
    # All 'x' values are identical
    return(return_list)
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
      paired = paired,
      alternative = alternative,
      alpha = alpha
    )
  )


  # Describe data -----------------------------------------------------------

  describer <- psych::describeBy(x, y, mat = TRUE, digits = 2)

  return_list$descriptive <- append(
    return_list$descriptive,
    list(x_by_y = describer)
  )


  # Check if "paired" is possible -------------------------------------------

  if (paired == TRUE && describer$n[1] != describer$n[2]) {
    stop(
      paste0("\n\tCannot handle 'paired = TRUE'. Lengths of '", describer$group[1], "' and '", describer$group[2], "' must match")
    )
  }


  # Set requirements --------------------------------------------------------

  requirements_normal <- TRUE
  requirements_size <- TRUE


  # Test each group for requirements ----------------------------------------

  return_list$reqs$normal <- list()
  return_list$reqs$group.size <- list()

  dataframe <- tibble(x, y)
  for (item in 1:as.integer(nrow(table(y)))) {
    group_metrics <- as.numeric(unlist(as.vector(
      dataframe |>
        dplyr::filter(y == describer$group1[item]) |>
        dplyr::select(x)
    )))

    # Assign dynamic Variable -----------------------------------------------
    # The "G" character declaration (char_decl) is neccessary due to
    # numeric groups (0|1)

    char_decl <- suppressWarnings(
      ifelse(is.na(as.numeric(describer$group1[item])) == FALSE, "G", "")
    )
    assign(paste0(char_decl, describer$group1[item]), group_metrics)


    # Check for normality ---------------------------------------------------

    normal_group <- eval(parse(text = paste0("test_normality(", paste0(char_decl, describer$group1[item]), ")")))

    if (describer$n[item] < 2 || normal_group$is.normal == FALSE) {
      requirements_normal <- FALSE
    }
    return_list$reqs$normal <- append(
      return_list$reqs$normal,
      eval(parse(text = paste0("list(
        ", char_decl, describer$group1[item], " = normal_group
      )")))
    )


    # Check for group size --------------------------------------------------

    if (describer$n[item] < 30) {
      requirements_size <- FALSE
    }
    return_list$reqs$group.size <- append(
      return_list$reqs$group.size,
      eval(parse(text = paste0("list(
        ", char_decl, describer$group1[item], " = describer$n[item]
      )")))
    )
  }

  # Run Tests ---------------------------------------------------------------

  nrow_data <- NROW(x)

  if (requirements_normal == FALSE || requirements_size == FALSE) {
    # Wilcoxon/MWU Test -----------------------------------------------------

    test <- wilcox.test(x ~ y,
      paired = paired,
      alternative = alternative,
      exact = ifelse(nrow_data < 40, TRUE, FALSE), # Field (2012, p. 659)
      conf.int = TRUE
    )

    p <- test$p.value


    # Get Effect Size -------------------------------------------------------

    if (is.na(p)) {
      stop("\n\tp.value is NA. Result cannot be calculated")
    } else {
      # Update test
      return_list$test <- append(
        return_list$test,
        list(
          wilcox.test = c(
            test,
            p.stars = pstars(p),
            method.alt = ifelse(paired == TRUE, "Wilcoxon", "MWU")
          )
        )
      )

      # Add is.significant result (bool)
      return_list <- append(return_list, list(
        is.significant = ifelse(p < alpha, TRUE, FALSE)
      ))

      # Find Magnitude
      z <- qnorm(p / 2)
      r <- z / sqrt(nrow_data)

      if (abs(r) < .1) {
        magnitude <- "Negligible"
      } else if (abs(r) < .3) {
        magnitude <- "Small"
      } else if (abs(r) < .5) {
        magnitude <- "Medium"
      } else if (abs(r) < .8) {
        magnitude <- "Large"
      } else {
        magnitude <- "Very large"
      }

      # Build translate list
      translate_list <- list()
      for (i in c("d", "r", "eta", "f", "chi", "z")) {
        translate_list[i] <- effsize_translate(r, "r", i, nrow_data)
      }

      # Update estimate
      return_list$test[[1]]$estimate <- append(
        return_list$test[[1]]$estimate,
        list(
          z = z,
          r = r,
          magnitude = magnitude,
          translate = translate_list
        )
      )
      rm(translate_list, magnitude)


      # Reporting -----------------------------------------------------------

      # Set report call
      return_list$report <- paste0(
        "test_ttest.report(test_ttest(", return_list$param$x_name, ", ", return_list$param$y_name, ", paired = ", paired, ", alternative = '", alternative, "', alpha = ", alpha, "))"
      )

      return_list$result <- paste0(
        ifelse(p < alpha, green(paste0(bold("\u2713"), " (Significant)\t")), red(paste0(bold("\u2717"), " (Not significant)\t"))),
        str_trim(return_list$test[[1]]$method),
        " (", return_list$test[[1]]$method.alt, ")",
        ", ", names(return_list$test[[1]]$statistic), " = ", format(round(return_list$test[[1]]$statistic[[1]], 2), nsmall = 2),
        ", Z = ", format(round(z, 2), nsmall = 2),
        ", ", reportp(p), pstars(p, ls = TRUE),
        ", r = ", format(round(r, 2), nsmall = 2), " (", return_list$test[[1]]$estimate$magnitude, ")\n"
      )
    }
  } else {
    # Run Levene-Test to check for homogeneity of variance ------------------

    levene <- leveneTest(x, y)

    # Update Requirements
    return_list$reqs$variance <- append(
      return_list$reqs$variance, c(
        levene,
        is.homo = ifelse(levene$"Pr(>F)"[1] < .01, FALSE, TRUE)
      )
    )


    # T-Test ----------------------------------------------------------------

    test <- t.test(x ~ y,
      var.equal = return_list$reqs$variance$is.homo,
      paired = paired,
      alternative = alternative
    )

    p <- test$p.value


    # Get Effect Size -------------------------------------------------------

    if (is.na(p)) {
      stop("\n\tp.value is NA. Result cannot be calculated")
    } else {
      # Update test
      return_list$test <- append(
        return_list$test,
        list(
          t.test = c(
            test,
            p.stars = pstars(p),
            method.alt = ifelse(return_list$reqs$variance$is.homo == FALSE, "Welch-Test", "T-Test")
          )
        )
      )

      # Add is.significant result (bool)
      return_list <- append(return_list, list(
        is.significant = ifelse(p < alpha, TRUE, FALSE)
      ))

      # Find Magnitude
      d <- effsize::cohen.d(x ~ y,
        pooled = return_list$reqs$variance$is.homo,
        paired = paired
      )

      # Build translate list
      translate_list <- list()
      for (i in c("d", "r", "eta", "f", "chi", "z")) {
        translate_list[i] <- effsize_translate(as.numeric(d$estimate), "d", i, nrow_data)
      }

      # Update estimate
      return_list$test[[1]]$estimate <- append(
        return_list$test[[1]]$estimate,
        list(
          d = d,
          magnitude = d$magnitude[1],
          translate = translate_list
        )
      )
      rm(translate_list)

      # Reporting -----------------------------------------------------------

      # Set report call
      return_list$report <- paste0(
        "test_ttest.report(test_ttest(", return_list$param$x_name, ", ", return_list$param$y_name, ", paired = ", paired, ", alternative = '", alternative, "', alpha = ", alpha, "))"
      )

      return_list$result <- paste0(
        ifelse(p < alpha, green(paste0(bold("\u2713"), " (Significant)\t")), red(paste0(bold("\u2717"), " (Not significant)\t"))),
        str_trim(return_list$test[[1]]$method),
        " (", return_list$test[[1]]$method.alt, ")",
        ", t(", format(round(as.numeric(return_list$test[[1]]$parameter), 2), nsmall = 2), ") = ", format(round(return_list$test[[1]]$statistic[[1]], 2), nsmall = 2),
        ", ", reportp(p), pstars(p, ls = TRUE),
        ", d = ", format(round(as.numeric(d$estimate), 2), nsmall = 2),
        " (", as.character(d$magnitude[1]), ")\n"
      )
    }
  }

  # Plot -------------------------------------------------------------------

  plot_data <- tibble(x, y) |> drop_na()
  return_list$plot <- paste0(
    "par(mfrow = c(1, 1))
        plot <- boxplot(c(", paste(plot_data$x, collapse = ","), ") ~ factor(c(", paste(paste0('\"', plot_data$y, '\"'), collapse = ","), ")),
                      main = '", return_list$param$x_var_name, ", ", return_list$param$y_var_name, "',
                      xlab = '", return_list$param$y_var_name, "', ylab = '", return_list$param$x_var_name, "',
                      color = FALSE)"
  )

  return(return_list)
}


# Reporting class for test_ttest ----------------------------------------

#' Class to build a full report for test_ttest
#'
#' @param object Object of test_ttest function
#'
#' @return Returns a full test report with simple figures
#' @seealso NCmisc::list.functions.in.file(filename = rstudioapi::getSourceEditorContext()$path)
#' @importFrom common spaces
#' @importFrom crayon bold green red
#' @importFrom stringr str_trim
#' @export
#'
#' @examples
#' report(test_ttest("len", "supp", ToothGrowth))
#' report(test_ttest(ToothGrowth$len, ToothGrowth$supp))
#' report(test_ttest(ToothGrowth[["len"]], ToothGrowth[["supp"]]))
test_ttest.report <- function(object) {
  headline(paste0("T-Testing: '", object$param$x_var_name, "', '", object$param$y_var_name, "'"), 1)

  headline(paste0("Descriptive"), 2)
  print(object$descriptive$x_by_y)

  headline(paste0("Test Requirements"), 2)
  cat(paste0("Normality of group ", names(object$reqs$normal[1]), ": ", spaces(calc_space(
    paste0("Normality of group ", names(object$reqs$normal[1]), ": "),
    c(
      paste0("Normality of group ", names(object$reqs$normal[2]), ": "),
      paste0("Observations in group ", names(object$reqs$normal[1]), ": "),
      paste0("Observations in group ", names(object$reqs$normal[2]), ": "),
      paste0("Homogenious Variances: ")
    ), 33
  )), object$reqs$normal[[1]]$result))
  if (!is.null(object$reqs$normal[[1]]$plot)) {
    report(object$reqs$normal[[1]], elm = "plot")
  }
  cat(paste0("Normality of group ", names(object$reqs$normal[2]), ": ", spaces(calc_space(
    paste0("Normality of group ", names(object$reqs$normal[2]), ": "),
    c(
      paste0("Normality of group ", names(object$reqs$normal[1]), ": "),
      paste0("Observations in group ", names(object$reqs$normal[1]), ": "),
      paste0("Observations in group ", names(object$reqs$normal[2]), ": "),
      paste0("Homogenious Variances: ")
    ), 33
  )), object$reqs$normal[[2]]$result))
  if (!is.null(object$reqs$normal[[2]]$plot)) {
    report(object$reqs$normal[[2]], elm = "plot")
  }
  cat(paste0(
    "Observations in group ", names(object$reqs$group.size[1]), ": ", spaces(calc_space(
      paste0("Observations in group ", names(object$reqs$group.size[1]), ": "),
      c(
        paste0("Normality of group ", names(object$reqs$normal[1]), ": "),
        paste0("Normality of group ", names(object$reqs$normal[2]), ": "),
        paste0("Observations in group ", names(object$reqs$normal[2]), ": "),
        paste0("Homogenious Variances: ")
      ), 33
    )), ifelse(object$reqs$group.size[[1]] >= 30, paste0(green(bold("\u2714"), "(n >= 30) ")), paste0(red(bold("\u2717"), "(n < 30) "))),
    object$reqs$group.size[[1]], "\n"
  ))
  cat(paste0(
    "Observations in group ", names(object$reqs$group.size[2]), ": ", spaces(calc_space(
      paste0("Observations in group ", names(object$reqs$group.size[2]), ": "),
      c(
        paste0("Normality of group ", names(object$reqs$normal[1]), ": "),
        paste0("Normality of group ", names(object$reqs$normal[2]), ": "),
        paste0("Observations in group ", names(object$reqs$normal[1]), ": "),
        paste0("Homogenious Variances: ")
      ), 33
    )), ifelse(object$reqs$group.size[[2]] >= 30, paste0(green(bold("\u2714"), "(n >= 30) ")), paste0(red(bold("\u2717"), "(n < 30) "))),
    object$reqs$group.size[[2]], "\n"
  ))
  if (!is.null(object$reqs$variance)) {
    cat(paste0("Homogenious Variances: ", spaces(calc_space(
      paste0("Homogenious Variances: "),
      c(
        paste0("Normality of group ", names(object$reqs$normal[1]), ": "),
        paste0("Normality of group ", names(object$reqs$normal[2]), ": "),
        paste0("Observations in group ", names(object$reqs$normal[1]), ": ")
      ), 33
    )), paste0(
      ifelse(object$reqs$variance$is.homo == TRUE, paste0(green(bold("\u2714"), "(Homogeniuous) ")), paste0(red(bold("\u2717"), "(Heterogeniuous) "))),
      "Levene Test, F(", object$reqs$variance$Df[1], ", ", object$reqs$variance$Df[2], ") = ", format(round(as.numeric(object$reqs$variance$`F value`[1]), 2), nsmall = 2), ", ",
      reportp(object$reqs$variance$`Pr(>F)`[1]), pstars(object$reqs$variance$`Pr(>F)`[1], ls = TRUE), "."
    ), "\n"))
  }

  headline(paste0(str_trim(object$test[[1]]$method), " (", object$test[[1]]$method.alt, ")"), 2)
  print_htest(object$test[[1]])

  headline("Summary", 2)
  cat(paste0(
    "A ", ifelse(object$param$paired == TRUE, "paired ", ""), object$test[[1]]$method.alt, " for ", ifelse(object$param$paired == FALSE, "an independent", "a dependent"), " sample was computed to assess difference in means between group ",
    names(object$reqs$normal[1]), " (M = ", object$descriptive$x_by_y$mean[1], ", SD = ", object$descriptive$x_by_y$sd[1], ") and group ",
    names(object$reqs$normal[2]), " (M = ", object$descriptive$x_by_y$mean[2], ", SD = ", object$descriptive$x_by_y$mean[2], ").\n"
  ))
  cat(paste0("There was ", ifelse(object$is.significant == TRUE, "a ", "no "), "statistically significant difference between the two groups", ifelse(object$is.significant == TRUE, paste0(" with ", tolower(object$test[[1]]$estimate$magnitude), " effects"), ""), ".\n\n"))
  cat(object$result, "\n")

  report(object, elm = "plot")
}
