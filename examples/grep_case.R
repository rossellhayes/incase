words <- c("caterpillar", "dogwood", "catastrophe", "dogma")

grep_case(
  words,
  "cat" ~ "feline",
  "dog" ~ "canine"
)

caps_words <- c("caterpillar", "dogwood", "Catastrophe", "DOGMA")

grep_case(
  caps_words,
  "cat" ~ "feline",
  "dog" ~ "canine",
  ignore.case = TRUE
)
