lm_patho <- as.data.frame(utils::read.csv("data-raw/lm_patho.csv"))
usethis::use_data(DATASET, overwrite = TRUE)
