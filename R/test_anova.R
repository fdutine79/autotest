# Function test_anova --------------------------------------------------

#' Perform an ANOVA
#'
#' @param x Name of column; dependent variable; metric values.
#' @param y Name of grouping column; independent variable; categorical.
#' @param data An optional data frame including `x` and `y` columns.
#' @param alpha Maximum accepted p-value.
#'
#' @return Returns a list of all results.
#'
#' @importFrom car leveneTest
#' @importFrom crayon bold green red
#' @importFrom dplyr filter select tibble
#' @importFrom psych describeBy
#' @importFrom rstatix eta_squared kruskal_effsize
#' @importFrom stats kruskal.test oneway.test pairwise.t.test pairwise.wilcox.test
#' @importFrom stringr str_trim
#' @importFrom tidyr drop_na
#'
#' @export
#'
#' @examples
#' var <- test_anova("Sepal.Length", "Species", iris)
#' test_anova("weight", "feed", chickwts)
test_anova <- function(x, y, data = "", alpha = .05) {
  # Initiate List to be returned -------------------------------------------

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
    warning("\n\tis.numeric(x) ist nicht TRUE")
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
      alpha = alpha
    )
  )


  # Describe data -----------------------------------------------------------

  describer <- psych::describeBy(x, y, mat = TRUE, digits = 2)

  return_list$descriptive <- append(
    return_list$descriptive,
    list(x_by_y = describer)
  )


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
    # numeric groups (0|1|2)

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

    if (describer$n[item] < 20) {
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

    # Kruskal-Wallis ----------------------------------------------------------

    test <- kruskal.test(x ~ y)

    p <- test$p.value


    # Get Effect Size -------------------------------------------------------

    if (is.na(p)) {
      warning("\n\tp.value is NA. Result cannot be calculated")
    } else {
      # Update tests
      return_list$tests <- append(
        return_list$tests,
        list(
          kruskal.test = c(
            test
          )
        )
      )

      # Update Stats
      return_list$stats <- append(
        return_list$stats,
        list(
          method = str_trim(test$method),
          method.alt = "Kruskal-Wallis",
          p.value = test$p.value,
          p.stars = pstars(test$p.value),
          statistic = list(
            H = as.numeric(test$statistic)
          ),
          parameter = list(
            df = as.numeric(test$parameter)
          )
        )
      )

      # Post Hoc-Test
      posthoc <- suppressWarnings(
        pairwise.wilcox.test(x, y)
      )
      return_list$tests <- append(
        return_list$tests,
        list(
          pairwise.wilcox.test = c(
            posthoc
          )
        )
      )

      # Add is.significant result (bool)
      return_list <- append(return_list, list(
        is.significant = ifelse(p < alpha, TRUE, FALSE)
      ))

      # Find Magnitude
      dummy_df <- data.frame(
        x1 = x,
        y1 = y
      )
      effect <- kruskal_effsize(dummy_df, x1 ~ y1)
      rm(dummy_df)

      # Build translate list
      translate_list <- list()
      for (i in c("d", "r", "eta", "f", "chi", "z")) {
        translate_list[i] <- effsize_translate(effect$effsize, "eta", i, nrow_data)
      }

      # Update estimate
      return_list$stats$estimate <- append(
        return_list$stats$estimate,
        list(
          eta2 = as.numeric(effect$effsize),
          magnitude = as.character(effect$magnitude),
          translate = translate_list
        )
      )
      rm(translate_list, effect)
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


    # ANOVA -----------------------------------------------------------------

    test_aov <- aov(x ~ y)
    test_aov_summary <- summary(test_aov)

    if (return_list$reqs$variance$is.homo == FALSE) {
      test_oneway <- oneway.test(x ~ y)
      p <- test_oneway$p.value
    } else {
      p <- as.numeric(test_aov_summary[[1]]$`Pr(>F)`)[1]
    }


    # Get Effect Size -------------------------------------------------------

    if (is.na(p)) {
      warning("\n\tp.value is NA. Result cannot be calculated")
    } else {
      # Update test
      return_list$tests <- append(
        return_list$tests,
        list(
          aov = c(
            test_aov,
            summary = test_aov_summary
          )
        )
      )

      # Update Stats
      if (return_list$reqs$variance$is.homo == FALSE) {
        # For heterogeneous variances
        return_list$stats <- append(
          return_list$stats,
          list(
            method = str_trim(test_oneway$method),
            method.alt = "Welch-Test",
            p.value = p,
            p.stars = pstars(p),
            statistic = list(
              F = as.numeric(test_oneway$statistic)
            ),
            parameter = list(
              df = as.numeric(test_oneway$parameter)
            )
          )
        )
      } else {
        # For homogeneous variances
        return_list$stats <- append(
          return_list$stats,
          list(
            method = "ANOVA (assuming equal variances)",
            method.alt = "ANOVA",
            p.value = p,
            p.stars = pstars(p),
            statistic = list(
              F = as.numeric(test_aov_summary[[1]]$`F value`)[1]
            ),
            parameter = list(
              df = as.numeric(test_aov_summary[[1]]$Df)
            )
          )
        )
      }

      # Post Hoc-Test
      posthoc <- suppressWarnings(
        pairwise.t.test(x, y, pool.sd = return_list$reqs$variance$is.homo)
      )
      return_list$tests <- append(
        return_list$tests,
        list(
          pairwise.t.test = c(
            posthoc
          )
        )
      )

      # Add is.significant result (bool)
      return_list <- append(return_list, list(
        is.significant = ifelse(p < alpha, TRUE, FALSE)
      ))

      # Find Magnitude
      effect <- suppressWarnings(
        effectsize::eta_squared(test_aov)
      )

      # Update tests
      return_list$tests <- append(
        return_list$tests,
        list(
          eta_squared = c(
            effect
          )
        )
      )

      if (abs(effect$Eta2) < .01) {
        magnitude <- "Negligible"
      } else if (abs(effect$Eta2) < .06) {
        magnitude <- "Small"
      } else if (abs(effect$Eta2) < .14) {
        magnitude <- "Medium"
      } else if (abs(effect$Eta2) < .20) {
        magnitude <- "Large"
      } else {
        magnitude <- "Very large"
      }

      # Build translate list
      translate_list <- list()
      for (i in c("d", "r", "eta", "f", "chi", "z")) {
        translate_list[i] <- effsize_translate(as.numeric(effect$Eta2), "eta", i, nrow_data)
      }

      # Update estimate
      return_list$stats$estimate <- append(
        return_list$stats$estimate,
        list(
          eta2 = as.numeric(effect$Eta2),
          magnitude = magnitude,
          translate = translate_list
        )
      )
      rm(translate_list, magnitude, effect)
    }
  }


  # Reporting -----------------------------------------------------------

  # Set report call
  return_list$report <- paste0(
    "test_anova.report(test_anova(", return_list$param$x_name, ", ", return_list$param$y_name, ", alpha = ", alpha, "))"
  )

  return_list$result <- paste0(
    ifelse(p < alpha, green(paste0(bold("\u2713"), " (Significant)\t")), red(paste0(bold("\u2717"), " (Not signif.)\t"))),
    str_trim(return_list$stats$method),
    " (", return_list$stats$method.alt, ")",
    ", ", names(return_list$stats$statistic), "(", str_trim(paste0(unlist(format(round(as.numeric(return_list$stats$parameter[[1]])))), collapse = ", ")), ") = ", format(round(as.numeric(return_list$stats$statistic), 2), nsmall = 2),
    ", ", reportp(p), pstars(p, ls = TRUE),
    ", ", names(return_list$stats$estimate[1]), " = ", format(round(as.numeric(return_list$stats$estimate[1]), 2), nsmall = 2),
    " (", return_list$stats$estimate$magnitude, ")\n"
  )

  # Plot -------------------------------------------------------------------

  plot_data <- tibble(x, y) |> drop_na()
  return_list$plot <- paste0(
    "par(mfrow = c(1, 1))
    plot <- boxplot(c(", paste(plot_data$x, collapse = ", "), ") ~ factor(c(", paste(paste0('\"', plot_data$y, '\"'), collapse = ","), ")),
      main = '", return_list$param$x_var_name, ", ", return_list$param$y_var_name, "',
      xlab = '", return_list$param$y_var_name, "', ylab = '", return_list$param$x_var_name, "',
      color = FALSE)"
  )

  return(return_list)
}


# Reporting class for test_anova ----------------------------------------
report(test_anova("weight", "feed", chickwts))

test_anova.report <- function(object) {
  headline(paste0("ANOVA: '", object$param$x_var_name, "', '", object$param$y_var_name, "'"), 1)

  headline(paste0("Descriptive"), 2)
  print(object$descriptive$x_by_y)

  headline(paste0("Test Requirements"), 2)
  for (g in object$descriptive$x_by_y$group1) {
    cat(paste0("Normality of group ", names(object$reqs$normal[g]), ": ", spaces(calc_space(
      paste0("Normality of group ", names(object$reqs$normal[g]), ": "), 33
    )), object$reqs$normal[[g]]$result))
    if (!is.null(object$reqs$normal[[g]]$plot)) {
      report(object$reqs$normal[[g]], elm = "plot")
    }
  }




}
