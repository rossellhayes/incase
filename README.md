
<!-- README.md is generated from README.Rmd. Please edit that file -->

# incase <img src="man/figures/logo.png?raw=TRUE" align="right" height="138" />

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/incase?color=brightgreen)](https://cran.r-project.org/package=incase)
[![r-universe status
badge](https://rossellhayes.r-universe.dev/badges/incase)](https://rossellhayes.r-universe.dev/incase)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blueviolet.svg)](https://cran.r-project.org/web/licenses/MIT)
[![R build
status](https://github.com/rossellhayes/incase/workflows/R-CMD-check/badge.svg)](https://github.com/rossellhayes/incase/actions)
[![](https://codecov.io/gh/rossellhayes/incase/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rossellhayes/incase)
<!-- badges: end -->

**incase** provides a more pipe-friendly alternative to
[**dplyr**](https://github.com/tidyverse/dplyr)’s
[`case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
and [`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html).

## Installation

You can install the released version of **incase** from
[CRAN](https://cran.r-project.org/package=incase) with:

``` r
install.packages("incase")
```

or the development version from
[GitHub](https://github.com/rossellhayes/incase) with:

``` r
# install.packages("pak")
pak::pkg_install("rossellhayes/incase")
```

## Usage

### Pipe-friendly conditionals

**incase**’s `in_case()` and `if_case()` accept a vector as their first
input, allowing you to take full advantage of
[**magrittr**](https://github.com/tidyverse/magrittr)’s `.`

``` r
1:20 %>%
  in_case(
    . %% 15 == 0 ~ "fizz buzz",
    . %%  3 == 0 ~ "fizz",
    . %%  5 == 0 ~ "buzz",
    TRUE         ~ .
  )
#>  [1] "1"         "2"         "fizz"      "4"         "buzz"      "fizz"     
#>  [7] "7"         "8"         "fizz"      "buzz"      "11"        "fizz"     
#> [13] "13"        "14"        "fizz buzz" "16"        "17"        "fizz"     
#> [19] "19"        "buzz"

1:20 %>% if_case(. %% 3 == 0, "fizz", .)
#>  [1] "1"    "2"    "fizz" "4"    "5"    "fizz" "7"    "8"    "fizz" "10"  
#> [11] "11"   "fizz" "13"   "14"   "fizz" "16"   "17"   "fizz" "19"   "20"
```

### Automatic type conversion

**incase** functions automatically coerce types. This is especially
useful when dealing with integers or `NA`s.

``` r
x <- -1:5

# Replace -1 with NA
dplyr::case_when(x == -1 ~ NA, TRUE ~ x)
#> [1] NA  0  1  2  3  4  5
dplyr::case_when(x == -1 ~ NA_integer_, TRUE ~ x)
#> [1] NA  0  1  2  3  4  5
in_case(x == -1 ~ NA, TRUE ~ x)
#> [1] NA  0  1  2  3  4  5

# Replace -1 with 0
dplyr::case_when(x == -1 ~ 0, TRUE ~ x)
#> [1] 0 0 1 2 3 4 5
dplyr::case_when(x == -1 ~ 0L, TRUE ~ x)
#> [1] 0 0 1 2 3 4 5
in_case(x == -1 ~ 0, TRUE ~ x)
#> [1] 0 0 1 2 3 4 5
```

With **incase**, you no longer have to worry about specifying the type
of your `NA`s or adding `L` to your integers.

### Easy default values

`in_case()` adds `preserve` and `default` arguments as a more intuitive
alternative to `TRUE ~ ...`.\*

``` r
1:20 %>%
  in_case(
    . %% 15 == 0 ~ "fizz buzz",
    . %%  3 == 0 ~ "fizz",
    . %%  5 == 0 ~ "buzz"
  )
#>  [1] NA          NA          "fizz"      NA          "buzz"      "fizz"     
#>  [7] NA          NA          "fizz"      "buzz"      NA          "fizz"     
#> [13] NA          NA          "fizz buzz" NA          NA          "fizz"     
#> [19] NA          "buzz"

1:20 %>%
  in_case(
    . %% 15 == 0 ~ "fizz buzz",
    . %%  3 == 0 ~ "fizz",
    . %%  5 == 0 ~ "buzz",
    preserve     = TRUE
  )
#>  [1] "1"         "2"         "fizz"      "4"         "buzz"      "fizz"     
#>  [7] "7"         "8"         "fizz"      "buzz"      "11"        "fizz"     
#> [13] "13"        "14"        "fizz buzz" "16"        "17"        "fizz"     
#> [19] "19"        "buzz"

1:20 %>%
  in_case(
    . %% 15 == 0 ~ "fizz buzz",
    . %%  3 == 0 ~ "fizz",
    . %%  5 == 0 ~ "buzz",
    default      = "pass"
  )
#>  [1] "pass"      "pass"      "fizz"      "pass"      "buzz"      "fizz"     
#>  [7] "pass"      "pass"      "fizz"      "buzz"      "pass"      "fizz"     
#> [13] "pass"      "pass"      "fizz buzz" "pass"      "pass"      "fizz"     
#> [19] "pass"      "buzz"
```

### Simplified interface for recoding

`switch_case()` works as a convenient shorthand for `in_case()` when
recoding discrete values.

``` r
parties
#>  [1] "G" "I" "D" "I" "R" "D" "L" "D" NA  "L" NA  "G" "R" NA  "G" NA  "L" "D" "D"
#> [20] "I"

parties %>%
  switch_case(
    "D"         ~ "Democrat",
    "R"         ~ "Republican",
    c("G", "L") ~ "Other",
    c("I", NA)  ~ "Independent"
  )
#>  [1] "Other"       "Independent" "Democrat"    "Independent" "Republican" 
#>  [6] "Democrat"    "Other"       "Democrat"    "Independent" "Other"      
#> [11] "Independent" "Other"       "Republican"  "Independent" "Other"      
#> [16] "Independent" "Other"       "Democrat"    "Democrat"    "Independent"
```

`grep_case()` allows you to recode values with pattern matching.

``` r
countries <- c(
  "France", "Ostdeutschland", "Westdeutschland", "Nederland",
  "België (Vlaanderen)", "Belgique (Wallonie)", "Luxembourg", "Italia"
)

grep_case(
  countries,
  "Deutschland" ~ "Germany",
  "Belg"        ~ "Belgium",
  "Nederland"   ~ "Netherlands",
  "Italia"      ~ "Italy",
  preserve      = TRUE,
  ignore.case   = TRUE
)
#> [1] "France"      "Germany"     "Germany"     "Netherlands" "Belgium"    
#> [6] "Belgium"     "Luxembourg"  "Italy"
```

#### Easily recode to (ordered) factor

When you need an ordered factor, the `*_fct()` family of functions lets
you save a step by using the order of your cases as the order of your
factor levels. Use `ordered = TRUE` to create an ordered factor and
`ordered = FALSE` to make a regular-old factor.

``` r
data <- runif(10, 0, 10)
data
#>  [1] 3.9075340 7.4098608 2.1540250 8.4599089 3.0487929 8.9268192 9.9001603
#>  [8] 9.2022173 0.3128016 5.2452107

data %>% 
  in_case_fct(
    . < 3   ~ "Low",
    . < 7   ~ "Medium",
    default = "High",
    ordered = TRUE
  )
#>  [1] Medium High   Low    High   Medium High   High   High   Low    Medium
#> Levels: Low < Medium < High

parties %>%
  switch_case_fct(
    "D"         ~ "Democrat",
    "R"         ~ "Republican",
    c("G", "L") ~ "Other",
    c("I", NA)  ~ "Independent"
  )
#>  [1] Other       Independent Democrat    Independent Republican  Democrat   
#>  [7] Other       Democrat    Independent Other       Independent Other      
#> [13] Republican  Independent Other       Independent Other       Democrat   
#> [19] Democrat    Independent
#> Levels: Democrat Republican Other Independent
```

------------------------------------------------------------------------

Hex sticker fonts are [Source Sans by
Adobe](https://github.com/adobe-fonts/source-sans) and [Hasklig by Ian
Tuomi](https://github.com/i-tu/Hasklig).

Please note that **incase** is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

\* Intuitiveness may vary from person to person.
