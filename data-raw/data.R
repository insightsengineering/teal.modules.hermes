## code to prepare `data` for testing examples
library(scda)

rADTTE <- synthetic_cdisc_data("latest")$adtte # nolint
usethis::use_data(rADTTE)
