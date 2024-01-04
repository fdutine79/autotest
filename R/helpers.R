# Function pstars -------------------------------------------------------

#' Print stars of p-value
#'
#' @param p The p-value of a significance test.
#' @param ls Leading space: adds space before stars.
#' @param ts Trailing space: adds space after stars.
#'
#' @return String.
#' @seealso NCmisc::list.functions.in.file(filename = rstudioapi::getSourceEditorContext()$path)
#' @importFrom GGally signif_stars
#' @export
#'
#' @examples
#' pstars(.005)
pstars <- function(p, ls = FALSE, ts = FALSE) {
  ls <- ifelse(ls == TRUE, " ", "")
  ts <- ifelse(ts == TRUE, " ", "")

  return(
    ifelse(!is.na(p) && !is.null(p) && p < .05,
      paste0(ls, signif_stars(p, point = NULL), ts),
      ""
    )
  )
}


# Function reportp -----------------------------------------------------

#' Formats the p.value for reporting purposes
#'
#' @param p The p.value.
#'
#' @return Returns the p-value as string (e.g., p < .001).
#' @export
#'
#' @examples
#' reportp(0.00012)
#' reportp(0.00123)
#' reportp(0.01234)
#' reportp(0.13456)
reportp <- function(p) {
  if (is.na(p) || is.null(p)) {
    p_formatted <- paste0("p = NA")
  } else if (p < .001) {
    p_formatted <- "p < .001"
  } else if (round(p, 2) < .01) {
    p <- formatC(round(p, 3), digits = 3, format = "f")
    p <- sub("0.", ".", p)
    p_formatted <- paste0("p = ", p)
  } else if (formatC(round(p, 2), digits = 2, format = "f") >= 1) {
    p_formatted <- paste0("p = 1")
  } else {
    p <- formatC(round(p, 2), digits = 2, format = "f")
    p <- sub("0.", ".", p)
    p_formatted <- paste0("p = ", p)
  }

  return(p_formatted)
}


# Function report ---------------------------------------------------------

#' Executes a string-saved call
#'
#' @param x Stringed call, list parameter.
#' @param elm List element with stringified call to be executed.
#'
#' @return Runs the stringified function.
#' @export
#'
#' @examples
#' report(test_correl("len", "dose", ToothGrowth), elm = "plot")
#' report(test_normality(rnorm(233)), elm = "plot")
report <- function(x, elm = "report") {
  return(
    invisible(eval(parse(text = x[[elm]])))
  )
}


# Function headline -------------------------------------------------------

#' Builds a headline (h1 - h4) for reporting
#'
#' @param text The text of the headline.
#' @param h The type/hierarchy of headline `c(1:4)`.
#'
#' @return returns a formatted headline.
#' @importFrom crayon blue bold
#' @importFrom Hmisc ceil
#' @export
#'
#' @examples
#' headline("Hello World Headline", 1)
headline <- function(text = FALSE, h = 1) {
  rep <- ifelse(74 - nchar(text) <= 0, 11, 74 - nchar(text))

  if (text == FALSE) {
    h <- bold("\n", paste(replicate(75, "#"), collapse = ""), "\n\n")
  } else if (h == 1) {
    h <- blue(bold(paste0(
      paste(replicate(nchar(text) + rep + 1, "="), collapse = ""), "\n",
      paste(paste(replicate(floor((rep - 1) / 2), "="), collapse = ""), text, paste(replicate(ceil((rep - 1) / 2), "="), collapse = "")), "\n",
      paste(replicate(nchar(text) + rep + 1, "="), collapse = ""), "\n\n"
    )))
  } else if (h == 2) {
    h <- blue(bold(paste0("\n", paste(text, paste(replicate(rep, "="), collapse = "")), "\n\n")))
  } else if (h == 3) {
    h <- blue(bold(paste0("\n", paste(text, paste(replicate(rep, "-"), collapse = "")), "\n\n")))
  } else if (h == 4) {
    h <- blue(bold(paste0("\n", text, "\n\n")))
  }

  return(cat(h))
}


#' Pretty print a test result of S3 class 'htest'
#'
#' @description
#' Function is based on package 'EnvStats'.
#' https://rdrr.io/cran/EnvStats/src/R/print.htestEnvStats.R
#'
#' @param x The test result formatted as list.
#'
#' @return Returns a formatted list.
#' @seealso NCmisc::list.functions.in.file(filename = rstudioapi::getSourceEditorContext()$path)
#' @importFrom common spaces
#' @importFrom stringr str_trim
#' @export
#'
#' @examples
#' print_htest(test_correl("len", "dose", ToothGrowth)$test[[1]])
#' print_htest(test_ttest("len", "supp", ToothGrowth)$test[[1]])
print_htest <- function(x) {
  coll.string <- paste0("\n", spaces(33))
  alt.string <- x$alternative
  nv <- x$null.value

  if (!is.null(nv)) {
    nnv <- names(nv)
    if ((lnv <- length(nv)) == 1) {
      cat("Null Hypothesis:", spaces(17), paste(paste(format(nnv, justify = "left"), nv, sep = " = "), collapse = coll.string), "\n", sep = "")
      alt.string <- x$alternative
      if (!is.na(match(alt.string, c("two.sided", "less", "greater")))) {
        alt.string <- switch(alt.string,
          two.sided = "not equal to",
          less = "less than",
          greater = "greater than"
        )
        alt.string <- paste("True", nnv, "is", alt.string, nv)
      }
    } else {
      cat("Null Hypothesis:", spaces(17), paste("All", lnv, "values of", nnv[1], "=", nv[1]), "\n", sep = "")
    }
  }

  cat(paste0("Alternative Hypothesis:", spaces(10), alt.string, "\n"))
  cat(paste0("Test Name:", spaces(23), str_trim(x$method), " (", x$method.alt, ")\n"))

  if (!is.null(x$estimate)) {
    cat("Estimated Parameter(s):", spaces(10), paste(paste(format(names(x$estimate), justify = "left"), format(x$estimate, nsmall = 0), sep = " = "), collapse = coll.string), "\n", sep = "")
    if (!is.null(x$estimation.method)) {
      cat("Estimation Method:", spaces(15), x$estimation.method, "\n", sep = "")
    }
  }
  if (is.null(names(x$data.name))) {
    cat("Data:", spaces(28), x$data.name, "\n", sep = "")
  } else {
    cat("Data:", spaces(28), paste(paste(format(names(x$data.name), justify = "left"), x$data.name, sep = " = "), collapse = coll.string), "\n", sep = "")
  }
  if (!is.null(x$grouping.variable)) {
    cat("Grouping Variable:", spaces(15), x$grouping.variable, "\n", sep = "")
  }
  if (!is.null(x$subset.expression)) {
    cat("Subset With:", spaces(21), x$subset.expression, "\n", sep = "")
  }
  if (!is.null(x$parent.of.data)) {
    cat("Data Source:", spaces(21), x$parent.of.data, "\n", sep = "")
  }
  if (!is.null(x$sample.size)) {
    if (length(x$sample.size) > 1) {
      cat("Sample Sizes:", spaces(20), paste(paste(format(names(x$sample.size), justify = "left"), format(x$sample.size, nsmall = 0), sep = " = "), collapse = coll.string), "\n", sep = "")
    } else {
      cat("Sample Size:", spaces(21), x$sample.size, "\n", sep = "")
    }
  }
  if (!is.null(x$bad.obs) && any(x$bad.obs > 0)) {
    if (length(x$bad.obs) > 1) {
      cat("Number NA/NaN/Inf's:", spaces(13), paste(paste(format(names(x$bad.obs), justify = "left"), format(x$bad.obs, nsmall = 0), sep = " = "), collapse = coll.string), "\n", sep = "")
    } else {
      cat("Number NA/NaN/Inf's:", spaces(13), x$bad.obs, "\n", sep = "")
    }
  }
  if (!is.null(x$statistic)) {
    string <- ifelse(length(x$statistic) == 1, paste("Test Statistic:", spaces(18), sep = ""), paste("Test Statistics:", spaces(17), sep = ""))
    cat(string, paste(paste(format(names(x$statistic), justify = "left"), format(x$statistic, nsmall = 0), sep = " = "), collapse = coll.string), "\n", sep = "")
  }
  if (!is.null(x$parameter)) {
    string <- ifelse(length(x$parameter) > 1, paste("Test Statistic Parameters:", spaces(7), sep = ""), paste("Test Statistic Parameter:", spaces(8), sep = ""))
    cat(string, paste(paste(format(names(x$parameter), justify = "left"), format(x$parameter, nsmall = 0), sep = " = "), collapse = coll.string), "\n", sep = "")
  }
  if (length(x$p.value) == 1) {
    cat("P-value:", spaces(25), paste0(formatC(x$p.value, digits = 5, format = "f"), pstars(x$p.value, ls = TRUE)), "\n", sep = "")
  } else {
    if (!is.null(names(x$p.value))) {
      cat("P-values:", spaces(24), paste(paste(format(names(x$p.value), justify = "left"), paste0(formatC(x$p.value, digits = 5, format = "f"), pstars(x$p.value, ls = TRUE)), sep = " = "), collapse = coll.string), "\n", sep = "")
    } else {
      cat("P-values:", spaces(24), paste(paste0(formatC(x$p.value, digits = 5, format = "f"), pstars(x$p.value, ls = TRUE)), collapse = coll.string), "\n", sep = "")
    }
  }
  if (!is.null(x$conf.int)) {
    ci.pct <- format(100 * attr(x$conf.int, "conf.level"))
    cat(ci.pct, "% Confidence Interval:", spaces(33 - nchar(ci.pct) - 22), paste(paste(c("LCL", "UCL"), format(x$conf.int, nsmall = 0), sep = " = "), collapse = coll.string), "\n", sep = "")
  }
  if (!is.null(x$interval)) {
    print.intervalEstimate(x$interval)
  }

  invisible(x)
}


# Function calc_space ---------------------------------------------------

#' Calculates amount of spaces for reporting
#'
#' @param x The string for which spaces are calculated.
#' @param params vector of other strings in table.
#' @param min The minimum space distance (optional).
#'
#' @return Returns an integer of required space.
#' @export
#'
#' @examples
#' calc_space("This is a test", c("This", "is", "a", "test"), 34)
#' calc_space("This", c("This is a test", "is", "a", "test"), 34)
#' calc_space("This", c("This is a test", "is", "a", "test"))
calc_space <- function(x, params, min = 0) {
  largest <- min
  for (i in c(x, params)) {
    if (nchar(i) > largest) {
      largest <- nchar(i)
    }
  }
  return(largest - nchar(x))
}


# Function effsize_translate --------------------------------------------

#' Translate an effect size to another
#'
#' @description
#' Transformation is based on psychometrica.
#' https://www.psychometrica.de/effektstaerke.html#transform
#' Accepted effect sizes:
#'
#' * from c(d, r, eta, f, chi, z)
#' * to c(d, r, eta, f, chi, z)
#'
#' 'from'/'to' chi and z require N
#'
#' @param val The current effect size value.
#' @param from The current effect size.
#' @param to The desired effect size.
#' @param N The total number of observations.
#'
#' @return Numeric translated effect size.
#' @export
#'
#' @examples
#' effsize_translate(-0.6761231, "z", "chi", 2044)
#' effsize_translate(-0.01495496, "r", "f")
effsize_translate <- function(val, from, to, N = FALSE) {
  d <- 0.0
  r <- 0.0
  eta <- 0.0
  chi <- 0.0
  z <- 0.0

  if (from == "d") {
    d <- val
    r <- sqrt((d * d) / ((d * d) + 4))
    tmp <- d / 2
    eta <- (tmp * tmp) / (1 + (tmp * tmp))
    f <- d * 0.5
    chi <- N * r^2
    z <- r * sqrt(N)
  } else if (from == "r") {
    r <- val
    d <- (2 * r) / sqrt(1 - (r * r))
    tmp <- d / 2
    eta <- (tmp * tmp) / (1 + (tmp * tmp))
    f <- d * 0.5
    chi <- N * r^2
    z <- r * sqrt(N)
  } else if (from == "eta") {
    eta <- val
    tmp <- sqrt(eta / (1 - eta))
    d <- tmp * 2
    r <- sqrt((d * d) / ((d * d) + 4))
    f <- d * 0.5
    chi <- N * r^2
    z <- r * sqrt(N)
  } else if (from == "f") {
    f <- val
    d <- val * 2
    r <- sqrt((d * d) / ((d * d) + 4))
    tmp <- d / 2
    eta <- (tmp * tmp) / (1 + (tmp * tmp))
    chi <- N * r^2
    z <- r * sqrt(N)
  } else if (from == "chi") {
    chi <- val
    r <- sqrt(val / N)
    d <- (2 * r) / sqrt(1 - (r * r))
    tmp <- d / 2
    eta <- (tmp * tmp) / (1 + (tmp * tmp))
    f <- d * 0.5
    z <- r * sqrt(N)
  } else if (from == "z") {
    z <- val
    r <- val / sqrt(N)
    d <- (2 * r) / sqrt(1 - (r * r))
    tmp <- d / 2
    eta <- (tmp * tmp) / (1 + (tmp * tmp))
    f <- d * 0.5
    chi <- N * r^2
  }

  return(as.numeric(get(to)))
}


# Function firstup --------------------------------------------------------

#' Capitalize first letter
#'
#' @param x String (test).
#'
#' @return String (Test).
#' @export
#'
#' @examples
#' firstup("hello")
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}
