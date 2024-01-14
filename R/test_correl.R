# Function test_correl --------------------------------------------------

#' Perform a bivariate correlation
#'
#' @param x A numeric vector of data values.
#' @param y A numeric vector of data values.
#' @param data An optional data frame including `x` and `y` columns.
#' @param alternative Directed hypotheses; `greater` means positive correlation.
#' @param alpha Maximum accepted p-value.
#'
#' @return Returns a list of all results.
#' @seealso NCmisc::list.functions.in.file(filename = rstudioapi::getSourceEditorContext()$path)
#' @importFrom crayon bold green red
#' @importFrom dplyr tibble
#' @importFrom graphics lines
#' @importFrom grDevices rgb
#' @importFrom psych describe
#' @importFrom scales alpha
#' @importFrom stats cor.test lowess
#' @importFrom stringr str_trim
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
#' test_correl("len", "dose", ToothGrowth)
#' test_correl(ToothGrowth$len, ToothGrowth$dose)
#' test_correl(ToothGrowth[["len"]], ToothGrowth[["dose"]])
#' test_correl(rnorm(2330), rnorm(2330))
test_correl <- function(x, y, data = "", alternative = "two.sided", alpha = .05) {
  # Initiate List to be returned -------------------------------------------

  return_list <- list()


  # Build param list --------------------------------------------------------

  # Check conditions and define params
  strctr <- c("numeric", "numeric")
  params_list <- force_structure(sys.calls(), strctr)

  return_list$param <- append(
    params_list,
    list(
      alternative = alternative,
      alpha = alpha
    )
  )

  x <- return_list$param$x
  y <- return_list$param$y

  if (NROW(x) != NROW(y)) {
    warning("\n\t'x' and 'y' must have the same length")
  }


  # Describe data -----------------------------------------------------------

  return_list$descriptive <- append(
    return_list$descriptive,
    list(
      x = psych::describe(x),
      y = psych::describe(y)
    )
  )


  # Set requirements --------------------------------------------------------

  requirements_pearson <- TRUE
  requirements_spearman <- TRUE
  requirements_kendall <- TRUE


  # Check for normality -----------------------------------------------------

  normal_x <- eval(parse(text = paste0("test_normality(", return_list$param$x_name, ")")))
  normal_y <- eval(parse(text = paste0("test_normality(", return_list$param$y_name, ")")))

  if (normal_x$is.normal == FALSE || normal_y$is.normal == FALSE) {
    requirements_pearson <- FALSE
  }

  # Update requirements
  return_list$reqs$normal <- append(
    return_list$reqs$normal,
    list(
      x = normal_x,
      y = normal_y
    )
  )

  rm(normal_x, normal_y)


  # Check for size ----------------------------------------------------------

  nrow_data <- NROW(x)

  if (nrow_data < 10) {
    requirements_pearson <- FALSE
    requirements_spearman <- FALSE
  } else if (nrow_data < 30) {
    requirements_pearson <- FALSE
  }

  # Update requirements
  return_list$reqs$size <- append(
    return_list$reqs$size,
    list(
      data = nrow_data
    )
  )


  # Run Test ---------------------------------------------------------------

  test <- suppressWarnings(cor.test(x, y,
    alternative = alternative,
    method = ifelse(requirements_pearson == TRUE, "pearson", ifelse(requirements_spearman == TRUE, "spearman", "kendall")),
    exact = ifelse(requirements_spearman == TRUE || requirements_kendall == TRUE, TRUE, NULL),
    conf.level = 1 - alpha,
    continuity = ifelse(requirements_spearman == TRUE || requirements_kendall == TRUE, TRUE, FALSE)
  ))

  p <- test$p.value

  # Update test
  return_list$test <- append(
    return_list$test,
    list(
      cor.test = c(
        test,
        p.stars = pstars(p),
        method.alt = ifelse(requirements_pearson == TRUE, "Pearson", ifelse(requirements_spearman == TRUE, "Spearman", "Kendall"))
      )
    )
  )

  # Add is.significant result (bool)
  return_list <- append(return_list, list(
    is.significant = ifelse(p < alpha, TRUE, FALSE)
  ))

  if (is.na(p)) {
    warning("\n\tp.value is NA. Result cannot be calculated")

    magnitude <- NA
    translate_list <- NA
  } else {
    # Find Magnitude
    if (abs(test$estimate) < .1) {
      magnitude <- "Negligible"
    } else if (abs(test$estimate) < .3) {
      magnitude <- "Small"
    } else if (abs(test$estimate) < .5) {
      magnitude <- "Medium"
    } else if (abs(test$estimate) < .8) {
      magnitude <- "Large"
    } else {
      magnitude <- "Very large"
    }

    # Build translate list
    translate_list <- list()
    for (i in c("d", "r", "eta", "f", "chi", "z")) {
      translate_list[i] <- effsize_translate(return_list$test[[1]]$estimate[[1]], "r", i, nrow_data)
    }
  }

  # Update estimate
  return_list$test[[1]]$estimate <- append(
    return_list$test[[1]]$estimate,
    list(
      magnitude = magnitude,
      translate = translate_list
    )
  )
  rm(translate_list, magnitude)


  # Reporting ---------------------------------------------------------------

  # Set report call
  return_list$report <- paste0(
    "test_correl.report(test_correl(", return_list$param$x_name, ", ", return_list$param$y_name, ", alternative = '", alternative, "', alpha = ", alpha, "))"
  )

  return_list$result <- paste0(
    resultcol(return_list$is.significant, "s"),
    str_trim(return_list$test[[1]]$method),
    " (", return_list$test[[1]]$method.alt, ")",
    ", r", ifelse(return_list$test[[1]]$method.alt == "Spearman", "_rho",
      ifelse(return_list$test[[1]]$method.alt == "Kendall", "_tau",
        paste0("(", format(round(as.numeric(return_list$test[[1]]$parameter), 2), nsmall = 2), ")")
      )
    ), " = ", format(round(as.numeric(return_list$test[[1]]$estimate[[1]]), 2), nsmall = 2),
    " (", return_list$test[[1]]$estimate$magnitude, ")",
    ", ", reportp(p), pstars(p, ls = TRUE), "\n"
  )


  # Plot -------------------------------------------------------------------

  plot_data <- tibble(x, y) |> drop_na()
  return_list$plot <- paste0(
    "par(mfrow = c(1, 1))
    plot(c(", paste(plot_data$y, collapse = ","), ") ~ c(", paste(plot_data$x, collapse = ","), "),
      main = '", return_list$param$x_var_name, ", ", return_list$param$y_var_name, "',
      xlab = '", return_list$param$x_var_name, "', ylab = '", return_list$param$y_var_name, "'
    )
    lines(lowess(c(", paste(plot_data$y, collapse = ","), "), c(", paste(plot_data$x, collapse = ","), ")), col = scales::alpha(rgb(0,0,0), 0.15), lwd = 6, type = 'l', lend = 2)
    abline(mean(c(", paste(plot_data$y, collapse = ","), ")), 0, lwd = 2, lty = 3)
    abline(lm(c(", paste(plot_data$y, collapse = ","), ") ~ c(", paste(plot_data$x, collapse = ","), ")), lwd = 2)"
  )

  return(return_list)
}


# Reporting class for test_correl ---------------------------------------

#' Class to build a full report for test_correl
#'
#' @param object Object of test_correl function
#'
#' @return Returns a full test report with simple figures
#' @seealso NCmisc::list.functions.in.file(filename = rstudioapi::getSourceEditorContext()$path)
#' @importFrom common spaces
#' @importFrom crayon bold green red yellow
#' @importFrom dplyr mutate_if
#' @export
#'
#' @examples
#' report(test_correl("len", "dose", ToothGrowth))
#' report(test_correl(ToothGrowth$len, ToothGrowth$dose))
#' report(test_correl(ToothGrowth[["len"]], ToothGrowth[["dose"]]))
#' report(test_correl(rnorm(2330), rnorm(2330)))
test_correl.report <- function(object) {
  headline(paste0("Correlation Testing: '", object$param$x_var_name, "', '", object$param$y_var_name, "'"), 1)

  headline(paste0("Descriptive"), 2)
  rownames(object$descriptive$x) <- object$param$x_var_name
  rownames(object$descriptive$y) <- object$param$y_var_name
  print(as.data.frame(rbind(object$descriptive$x, object$descriptive$y))[c(2:5, 8:9, 11:13)] |>
    mutate_if(is.numeric, round, digits = 2))

  headline(paste0("Test Requirements"), 2)
  cat(paste0("Normality of ", object$param$x_var_name, ": ", spaces(calc_space(
    paste0("Normality of ", object$param$x_var_name, ": "),
    c(
      paste0("Normality of ", object$param$y_var_name, ": "),
      paste0("Observations: ")
    ), 33
  )), object$reqs$normal$x$result))
  if (!is.null(object$reqs$normal$x$plot)) {
    report(object$reqs$normal$x, elm = "plot")
  }
  cat(paste0("Normality of ", object$param$y_var_name, ": ", spaces(calc_space(
    paste0("Normality of ", object$param$y_var_name, ": "),
    c(
      paste0("Normality of ", object$param$x_var_name, ": "),
      paste0("Observations: ")
    ), 33
  )), object$reqs$normal$y$result))
  if (!is.null(object$reqs$normal$y$plot)) {
    report(object$reqs$normal$y, elm = "plot")
  }
  cat(paste0(
    "Observations: ", spaces(calc_space(
      paste0("Observations: "),
      c(
        paste0("Normality of ", object$param$x_var_name, ": "),
        paste0("Normality of ", object$param$y_var_name, ": ")
      ), 33
    )), ifelse(object$reqs$size$data >= 30, paste0(green(bold("\u2714"), "(n >= 30) ")), ifelse(object$reqs$size$data >= 10, paste0(yellow(bold("\u26A0"), "(n >= 10) ")), paste0(red(bold("\u2717"), "(n < 10) ")))),
    object$reqs$size$data, "\n"
  ))

  headline(paste0(object$test[[1]]$method, " (", object$test[[1]]$method.alt, ")"), 2)
  print_htest(object$test[[1]])

  headline("Summary", 2)
  cat(paste0("A ", object$test[[1]]$method.alt, " correlation coefficient was computed to assess the linear relationship between ", object$param$x_var_name, " and ", object$param$y_var_name, ".\n"))
  cat(paste0("There was ", ifelse(object$test[[1]]$estimate[[1]] < 0, "a negative", ifelse(object$test[[1]]$estimate[[1]] > 0, "a positive", "no")), " correlation between the two variables", ifelse(object$is.significant == TRUE, paste0(" with ", tolower(object$test[[1]]$estimate$magnitude), " effects"), ""), ".\n\n"))
  cat(object$result, "\n")

  report(object, elm = "plot")
}
