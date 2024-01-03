install.packages("devtools")
install.packages("roxygen2")
devtools::create("autotest")
devtools::document()
library("autotest")

xxx <- check_normality(ToothGrowth$dose)
xxx$is.normal
report(xxx)
