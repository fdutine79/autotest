#install.packages("devtools")
#install.packages("roxygen2")
#devtools::create("autotest")
devtools::document()
devtools::install()
library("autotest")

suppressMessages(library(usethis))
usethis::use_tidy_style()

#usethis::use_testthat(3)
devtools::test()
devtools::check()


###

#normality_table <- normality_table
#usethis::use_data(normality_table)



xxx <- test_normality(ToothGrowth$dose)
xxx$is.normal
report(xxx)

xxx <- test_correl(ToothGrowth$dose, ToothGrowth$len)
xxx$is.significant
report(xxx)

xxx <- test_crosstabs(mtcars$cyl, mtcars$vs)
xxx$is.significant
report(xxx)

xxx <- test_ttest(ToothGrowth$len, ToothGrowth$supp)
xxx$is.significant
report(xxx)

print_htest(test_correl("len", "dose", ToothGrowth)$test[[1]])
