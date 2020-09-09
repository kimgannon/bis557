library(palmerpenguins)
library(usethis)
library(missForest)
library(dplyr)

#Base
penguinsi <- penguins
penguinsi <- as_tibble(missForest(as.data.frame(penguinsi))$ximp)
names(penguinsi)

#Tidy
penguinsi <- penguins %>%
  as.data.frame() %>%
  missForest() %>%
  `$`(ximp) %>%
  as_tibble()

use_data(penguinsi, overwrite = TRUE)
