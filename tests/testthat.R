library(testthat)
library(otargen)

test_check("otargen")

# to be removed when publishing the package
#sapply(stringr::str_replace(list.files("./R/"),".R$",""),usethis::use_test)
