
<!-- README.md is generated from README.Rmd. Please edit that file -->

# incase <img src="man/figures/logo.png?raw=TRUE" align="right" height="138" />

**incase** provides a more pipe-friendly alternative to
[**dplyr**](https://github.com/tidyverse/dplyr)’s
[`case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
and [`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html).

## Installation

You can install the development version of **incase** from
[GitHub](https://github.com/rossellhayes/incase) with:

``` r
# install.packages("remotes")
remotes::install_github("rossellhayes/incase")
```

## Example

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

As shown above, they also automatically coerce types. This is especially
useful when dealing with integers.

``` r
# Halve all odd numbers
x <- 1:10

if_case(x %% 2 != 0, x / 2, x)
#>  [1]  0.5  2.0  1.5  4.0  2.5  6.0  3.5  8.0  4.5 10.0
```

`in_case()` adds `preserve` and `default` arguments, to avoid having to
use `TRUE ~ ...`.

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

`switch_case()` works as a convenient shorthand for `in_case()` when
recoding discrete values.

``` r
parties
#>  [1] "D" "I" NA  "G" "I" "R" NA  "I" "R" "D" "D" "I" "D" "D" "I" "I" "D" "R" "I"
#> [20] "G"

parties %>% 
  in_case(
    . == "d"           ~ "Democratic",
    . == "r"           ~ "Republican",
    . %in% c("g", "l") ~ "Other",
    . %in% c("i", NA)  ~ "Independent" 
  )
#>  [1] NA            NA            "Independent" NA            NA           
#>  [6] NA            "Independent" NA            NA            NA           
#> [11] NA            NA            NA            NA            NA           
#> [16] NA            NA            NA            NA            NA

parties %>%
  switch_case(
    "d"         ~ "Democrat",
    "r"         ~ "Republican",
    c("g", "l") ~ "Other",
    c("i", NA)  ~ "Independent"
  )
#>  [1] NA            NA            "Independent" NA            NA           
#>  [6] NA            "Independent" NA            NA            NA           
#> [11] NA            NA            NA            NA            NA           
#> [16] NA            NA            NA            NA            NA
```
