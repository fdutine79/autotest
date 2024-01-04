#install.packages("devtools")
#install.packages("roxygen2")
#devtools::create("autotest")
devtools::document()
devtools::install()
library("autotest")


#usethis::use_testthat(3)
devtools::test()
devtools::check()

suppressMessages(library(usethis))
usethis::use_tidy_style()


###

library(MASS)


xxx <- test_normality(ToothGrowth$dose)
xxx$is.normal
report(xxx)

xxx <- test_correl(ToothGrowth$dose, ToothGrowth$len)
xxx$is.significant
report(xxx)

xxx <- test_correl(ToothGrowth$dose, ToothGrowth$len)
xxx$is.significant
report(xxx)

xxx <- test_crosstabs(Cars93$AirBags, Cars93$Man.trans.avail)
xxx$is.significant
report(xxx)

xxx <- test_ttest(ToothGrowth$len, ToothGrowth$supp)
xxx$is.significant
report(xxx)

print_htest(test_correl("len", "dose", ToothGrowth)$test[[1]])
