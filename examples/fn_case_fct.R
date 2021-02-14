switch_case_fct(
  c("a", "b", "c"),
  "c" ~ "cantaloupe",
  "b" ~ "banana",
  "a" ~ "apple"
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

fn_case_fct(
  c("a", "b", "c", "d"),
  `%in%`,
  "c" ~ "cantaloupe",
  "b" ~ "banana",
  "a" ~ "apple",
  preserve = TRUE
)
