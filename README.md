

<!-- README.md is generated from README.Rmd. Please edit that file -->

# dgest

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `dgest` R package is a collection of functions to provide digestible
summaries of genotype-by-environment (GxE) data. It serves to provide
common summaries for initial data analysis.

``` r
library(dgest)
toydata
#>   gen  env rep
#> 1   A env2   1
#> 2   A env3   1
#> 3   A env1   1
#> 4   B env1   1
#> 5   A env2   2
#> 6   B env3   2
#> 7   B env1   2
#> 8   C env2   2
#> 9   C env1   1

C <- concurrence(toydata, gen, env)

C
#>       env
#> env    env1 env2 env3
#>   env1    3    2    2
#>   env2    2    2    1
#>   env3    2    1    2

adorn_common_percentages(C)
#>    env       env1      env2      env3
#> 1 env1          3 2 (66.7%) 2 (66.7%)
#> 2 env2 2 (100.0%)         2 1 (50.0%)
#> 3 env3 2 (100.0%) 1 (50.0%)         2
```
