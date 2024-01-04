#' Table of successful normality tests
#'
#' @format ## `normality_table`
#' A data frame with 25 rows and 11 columns:
#' \describe{
#'   \item{n}{Sample size}
#'   \item{winll}{Lilliefors}
#'   \item{winsw}{Shapiro-Wilk}
#'   \item{winpt}{Pearson with adjustment}
#'   \item{winpf}{Pearson without adjustment}
#'   \item{winad}{Anderson-Darling}
#'   \item{wincv}{Cramer-von Mises}
#'   \item{winsf}{Shapiro-Francia}
#'   \item{winjb}{Jarque Bera}
#'   \item{winjc}{Jarque Bera with contingency coefficient}
#'   \item{max}{Maximum winner}
#'   ...
#' }
#' @source Function to build normality table
"normality_table"
