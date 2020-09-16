
<!-- README.md is generated from README.Rmd. Please edit that file -->

# incase <img src="man/figures/logo.png?raw=TRUE" align="right" height="138" />

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/incase?color=brightgreen)](https://cran.r-project.org/package=incase)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blueviolet.svg)](https://cran.r-project.org/web/licenses/MIT)
[![R build
status](https://github.com/rossellhayes/incase/workflows/R-CMD-check/badge.svg)](https://github.com/rossellhayes/incase/actions)
[![](https://codecov.io/gh/rossellhayes/incase/branch/master/graph/badge.svg)](https://codecov.io/gh/rossellhayes/incase)
[![Dependencies](https://tinyverse.netlify.com/badge/incase)](https://cran.r-project.org/package=incase)
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
# install.packages("remotes")
remotes::install_github("rossellhayes/incase")
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
#> Error: must be a logical vector, not an integer vector.
dplyr::case_when(x == -1 ~ NA_integer_, TRUE ~ x)
#> [1] NA  0  1  2  3  4  5
in_case(x == -1 ~ NA, TRUE ~ x)
#> [1] NA  0  1  2  3  4  5

# Replace -1 with 0
dplyr::case_when(x == -1 ~ 0, TRUE ~ x)
#> Error: must be a double vector, not an integer vector.
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
#>  [1] "I" "L" "R" NA  "I" "R" "D" "R" "R" "R" "D" "I" "I" "I" "L" "D" "L" "R" "G"
#> [20] "D"

parties %>% 
  in_case(
    . == "D"           ~ "Democratic",
    . == "R"           ~ "Republican",
    . %in% c("G", "L") ~ "Other",
    . %in% c("I", NA)  ~ "Independent" 
  )
#>  [1] "Independent" "Other"       "Republican"  "Independent" "Independent"
#>  [6] "Republican"  "Democratic"  "Republican"  "Republican"  "Republican" 
#> [11] "Democratic"  "Independent" "Independent" "Independent" "Other"      
#> [16] "Democratic"  "Other"       "Republican"  "Other"       "Democratic"

parties %>%
  switch_case(
    "D"         ~ "Democrat",
    "R"         ~ "Republican",
    c("G", "L") ~ "Other",
    c("I", NA)  ~ "Independent"
  )
#>  [1] "Independent" "Other"       "Republican"  "Independent" "Independent"
#>  [6] "Republican"  "Democrat"    "Republican"  "Republican"  "Republican" 
#> [11] "Democrat"    "Independent" "Independent" "Independent" "Other"      
#> [16] "Democrat"    "Other"       "Republican"  "Other"       "Democrat"
```

-----

Hex sticker fonts are [Source Sans
Pro](https://github.com/adobe-fonts/source-sans-pro) by
[Adobe](https://www.adobe.com) and
[Hasklig](https://github.com/i-tu/Hasklig) by [Ian
Tuomi](https://github.com/i-tu).

Please note that **incase** is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

\* Intuitiveness may vary from person to person.
