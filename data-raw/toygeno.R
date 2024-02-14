## code to prepare `toygeno` dataset goes here
set.seed(1)
toypheno <- data.frame(genotype = c("pikachu", "Bulbasur", "pikachu", "squirtle", "jigglypuff",
                                    "pikachu", "Bulbasur", "pikachu", "squirtle", "jigglypuff"),
                       env = sample(c("env1", "env2"), size = 10, replace = TRUE),
                       y1 = rnorm(10))
toygeno <- data.frame(genotype = c("pikachu", "Bulbasur", "Jiglypuff", "Charmander"),
                      SNP1 = sample(c("A", "T"), size = 4, replace = TRUE),
                      SNP2 = sample(c("G", "T"), size = 4, replace = TRUE),
                      SNP3 = sample(c("C", "G"), size = 4, replace = TRUE))

usethis::use_data(toygeno, overwrite = TRUE)
usethis::use_data(toypheno, overwrite = TRUE)
