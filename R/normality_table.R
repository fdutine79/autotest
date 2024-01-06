#' Table of successful normality tests
#'
#' @format ## `normality_table`
#' A data frame with 26 rows and 15 columns:
#' \describe{
#'   \item{n}{Sample size}
#'   \item{ad}{Anderson-Darling}
#'   \item{cv}{Cramer-von Mises}
#'   \item{da}{D'Agostino}
#'   \item{jb}{Jarque Bera (classic)}
#'   \item{jc}{Jarque–Bera CC (classic)}
#'   \item{ks}{Kolmogorov-Smirnov}
#'   \item{ll}{Lilliefors}
#'   \item{pf}{Pearson chi-square}
#'   \item{pt}{Pearson chi-square (adjusted)}
#'   \item{rb}{Jarque–Bera (robust)}
#'   \item{rb}{Jarque–Bera CC (robust)}
#'   \item{sf}{Shapiro-Francia}
#'   \item{sw}{Shapiro-Wilk}
#'   \item{max}{Maximum winner}
#' }
#' @source build_normality_table.R
#' @seealso [autotest::build_normality_table()]
"normality_table"
