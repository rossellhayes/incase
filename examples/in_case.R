# Non-piped statements are handled the same as dplyr::case_when()
x <- 1:30
in_case(
  x %% 15 == 0 ~ "fizz buzz",
  x %%  3 == 0 ~ "fizz",
  x %%  5 == 0 ~ "buzz",
  TRUE         ~ x
)

# A vector can be directly piped into in_case() without error
1:30 %>%
  in_case(
    . %% 15 == 0 ~ "fizz buzz",
    . %%  3 == 0 ~ "fizz",
    . %%  5 == 0 ~ "buzz",
    TRUE         ~ .
  )

# in_case() silently converts types
1:30 %>%
  in_case(
    . %% 15 == 0 ~ 35,
    . %%  3 == 0 ~ 5,
    . %%  5 == 0 ~ 7,
    TRUE         ~ NA
  )

x <- 1:30
try(
  dplyr::case_when(
    x %% 15 == 0 ~ 35,
    x %%  3 == 0 ~ 5,
    x %%  5 == 0 ~ 7,
    TRUE         ~ NA
  )
)

# .default and .preserve make it easier to handle unmatched values
1:30 %>%
  in_case(
    . %% 15 == 0 ~ "fizz buzz",
    . %%  3 == 0 ~ "fizz",
    . %%  5 == 0 ~ "buzz",
    .default     = "pass"
  )

1:30 %>%
  in_case(
    . %% 15 == 0 ~ "fizz buzz",
    . %%  3 == 0 ~ "fizz",
    . %%  5 == 0 ~ "buzz",
    .preserve    = TRUE
  )
