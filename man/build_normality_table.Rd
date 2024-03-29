% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_normality_table.R
\name{build_normality_table}
\alias{build_normality_table}
\title{Builds the normality table}
\usage{
build_normality_table(
  rounds = 100,
  fibonacci = 30,
  alpha = 0.05,
  alphacc = 0.3,
  test = FALSE
)
}
\arguments{
\item{rounds}{Number of round, the tests shall run.}

\item{fibonacci}{Number of fibonacci patterns to be applied (min = 5).}

\item{alpha}{Maximum accepted p-value.}

\item{alphacc}{Maximum accepted contingency coefficient for \code{jc} and \code{rc}.}

\item{test}{If TRUE a test is assumed. Prompting and saving are omitted.}
}
\value{
Returns a data frame.
}
\description{
The normality table identifies the best performing normality tests depending
on sample size. Sample sizes are referenced by fibonacci numbers. The optimal
test can then be applied by selecting the best performing test at the nearest
sample size neighbor in comparison to the maximum achievement.

Current normality tests included are:
\itemize{
\item Anderson-Darling
\item Cramer-von Mises
\item D'Agostino
\item Jarque–Bera (classic, robust, with and without contingency coefficient)
\item Kolmogorov-Smirnov
\item Lilliefors
\item Pearson chi-square (with and without adjustment)
\item Shapiro-Francia
\item Shapiro-Wilk
}
}
\examples{
normality_table <- build_normality_table(rounds = 2, fibonacci = 7, test = TRUE)
}
