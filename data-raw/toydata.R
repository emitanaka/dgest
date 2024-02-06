## code to prepare `toydata` dataset goes here
library(dplyr)
set.seed(1)
toydata <- expand.grid(gen = LETTERS[1:3], env = paste0("env", 1:3), rep = 1:2) |>
  slice_sample(prop = 0.5)

toydata

usethis::use_data(toydata, overwrite = TRUE)
