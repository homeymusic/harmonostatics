
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

## Examples

``` r
library(harmonostatics)
```

``` r
harmony(4,0,"Major Third")
#> # A tibble: 1 × 6
#>   semitone intervallic_name name        affinity brightness magnitude
#>      <dbl> <chr>            <chr>          <dbl>      <dbl>     <dbl>
#> 1        4 4                Major Third        6          2      6.32
```

``` r
harmony(c(0,4,7),0,"C Major")
#> # A tibble: 1 × 6
#>   semitone intervallic_name name    affinity brightness magnitude
#>      <dbl> <chr>            <chr>      <dbl>      <dbl>     <dbl>
#> 1     3.67 0:4:7            C Major     7.67      0.801      7.71
```

``` r
harmony(c(0,2,4,5,7,9,11,12),0,"Major | Ionian")
#> # A tibble: 1 × 6
#>   semitone intervallic_name  name           affinity brightness magnitude
#>      <dbl> <chr>             <chr>             <dbl>      <dbl>     <dbl>
#> 1     6.25 0:2:4:5:7:9:11:12 Major | Ionian     6.36      0.558      6.38
```

``` r
harmony(0:12,0,"Chromatic")
#> # A tibble: 1 × 6
#>   semitone intervallic_name             name      affinity brightness magnitude
#>      <dbl> <chr>                        <chr>        <dbl>      <dbl>     <dbl>
#> 1        6 0:1:2:3:4:5:6:7:8:9:10:11:12 Chromatic     5.31          0      5.31
```

``` r
potential_energy(c(0,4,7),c(0,4,7),0,"Ionian I - 1st Scale Degree")
#> # A tibble: 1 × 7
#>   semitone intervallic_name name  affinity brightness magnitude potential_energy
#>      <dbl> <chr>            <chr>    <dbl>      <dbl>     <dbl>            <dbl>
#> 1     3.67 0:4:7            Ioni…     7.67      0.801      7.71                0
```

``` r
potential_energy(c(5,9,12),c(0,4,7),0,"Ionian IV - 4th Scale Degree")
#> # A tibble: 1 × 7
#>   semitone intervallic_name name  affinity brightness magnitude potential_energy
#>      <dbl> <chr>            <chr>    <dbl>      <dbl>     <dbl>            <dbl>
#> 1     8.67 5:9:12           Ioni…     7.67      0.801      7.71             22.5
```

``` r
potential_energy(c(7,11,14),c(0,4,7),0,"Ionian V - 5th Scale Degree")
#> # A tibble: 1 × 7
#>   semitone intervallic_name name  affinity brightness magnitude potential_energy
#>      <dbl> <chr>            <chr>    <dbl>      <dbl>     <dbl>            <dbl>
#> 1     10.7 7:11:14          Ioni…     7.67      0.801      7.71             61.5
```
