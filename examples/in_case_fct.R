1:10 %>%
  in_case_fct(
    . %% 2 == 0 ~ "even",
    . %% 2 == 1 ~ "odd"
  )

switch_case_fct(
  c("a", "b", "c"),
  "c" ~ "cantaloupe",
  "b" ~ "banana",
  "a" ~ "apple"
)

switch_case_fct(
  c("a", "b", "c", "d"),
  "c" ~ "cantaloupe",
  "b" ~ "banana",
  "a" ~ "apple"
)

switch_case_fct(
  c("a", "b", "c", "d"),
  "c" ~ "cantaloupe",
  "b" ~ "banana",
  "a" ~ "apple",
  .preserve = TRUE
)

grep_case_fct(
  c("caterpillar", "dogwood", "catastrophe", "dogma"),
  "cat" ~ "feline",
  "dog" ~ "canine"
)

fn_case_fct(
  c("a", "b", "c"),
  `%in%`,
  "c" ~ "cantaloupe",
  "b" ~ "banana",
  "a" ~ "apple"
)
