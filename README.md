
<!-- README.md is generated from README.Rmd. Please edit that file -->

# harmonostatics

<!-- badges: start -->

[![R-CMD-check](https://github.com/homeymusic/a_field_theory_of_musical_harmony/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/homeymusic/a_field_theory_of_musical_harmony/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/homeymusic/a_field_theory_of_musical_harmony/branch/main/graph/badge.svg?token=RMOXE1JT89)](https://codecov.io/gh/homeymusic/a_field_theory_of_musical_harmony)
[![test-coverage](https://github.com/homeymusic/a_field_theory_of_musical_harmony/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/homeymusic/a_field_theory_of_musical_harmony/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The goal of harmonostatics is to explore a field theory of musical
harmony.

## Installation

You can install the development version of harmonostatics from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("homeymusic/harmonostatics")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(harmonostatics)
## basic example code
harmony(c(0,4,7),0,"C Major")
#> # A tibble: 1 × 6
#>   semitone intervallic_name name    affinity brightness magnitude
#>      <dbl> <chr>            <chr>      <dbl>      <dbl>     <dbl>
#> 1     3.67 0:4:7            C Major     7.67      0.801      7.71
potential_energy(c(7,11,14),c(0,4,7),0,"Ionian V - 5th Degree")
#> # A tibble: 1 × 7
#>   semitone intervallic_name name  affinity brightness magnitude potential_energy
#>      <dbl> <chr>            <chr>    <dbl>      <dbl>     <dbl>            <dbl>
#> 1     10.7 7:11:14          Ioni…     7.67      0.801      7.71             61.5
```
