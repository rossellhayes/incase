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
