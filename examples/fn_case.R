# Replicate switch_case()
parties <- sample(c("d", "r", "i", "g", "l"), 20, replace = TRUE)

fn_case(
  parties,
  fn = `%in%`,
  "d" ~ "Democrat",
  "r" ~ "Republican",
  "i" ~ "Independent",
  "g" ~ "Green",
  "l" ~ "Libertarian"
)

# Replicate grep_case()
countries <- c(
  "France", "Ostdeutschland", "Westdeutschland", "Nederland",
  "Belgie (Vlaanderen)", "Belgique (Wallonie)", "Luxembourg", "Italia"
)

fn_case(
  countries,
  fn = function(x, pattern, ...) grepl(pattern, x, ...),
  "Deutschland" ~ "Germany",
  "Belgi(qu)?e" ~ "Belgium",
  "Nederland"   ~ "Netherlands",
  "Italia"      ~ "Italy",
  preserve      = TRUE,
  ignore.case   = TRUE
)

# Recode values in a range
time  <- runif(10, 1, 12)
hours <- time %/% 1 %>%
  if_case(minutes > 32.5, (. + 1) %% 12, .) %>%
  switch_case(0 ~ 12, preserve = TRUE) %>%
  nombre::cardinal()

time %% 1 * 60 %>%
  fn_case(
    fn = function(x, y) {abs(x - y) <= 2.5},
    0  ~ "o'clock",
    60 ~ "o'clock",
    30 ~ "half past",
    15 ~ "quarter past",
    45 ~ "quarter to",
    5  ~ "five past",
    10 ~ "ten past",
    20 ~ "twenty past",
    25 ~ "twenty-five past",
    55 ~ "five to",
    50 ~ "ten to",
    40 ~ "twenty to",
    35 ~ "twenty-five to"
  ) %>%
  switch_case(
    "o'clock" ~ paste(hours, .),
    default   = paste(., hours)
  )
