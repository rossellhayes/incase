1:3 %>%
  in_case_list(
    . < 2    ~ mtcars,
    .default = letters
  )
