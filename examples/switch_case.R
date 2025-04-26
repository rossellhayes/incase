parties <- sample(c("d", "r", "i", "g", "l"), 20, replace = TRUE)

switch_case(
  parties,
  "d" ~ "Democrat",
  "r" ~ "Republican",
  "i" ~ "Independent",
  "g" ~ "Green",
  "l" ~ "Libertarian"
)

parties %>%
  switch_case(
    "d" ~ "Democrat",
    "r" ~ "Republican",
    "i" ~ "Independent",
    "g" ~ "Green",
    "l" ~ "Libertarian"
  )

parties %>%
  switch_case(
    "d" ~ "Democrat",
    "r" ~ "Republican",
    c("i", "g", "l") ~ "Other"
  )

parties %>%
  switch_case(
    "d" ~ "Democrat",
    "r" ~ "Republican",
    .default = "Other"
  )

parties %>%
  switch_case(
    "d" ~ "Democrat",
    "r" ~ "Republican",
    .preserve = FALSE
  )

parties %>%
  switch_case(
    "d" ~ "Democrat",
    "r" ~ "Republican",
    .preserve = TRUE
  )

data <- c(1, 4, 8, 12, 999, 6, 2, 888, 4, 6, 777)

fn_switch_case(
  data,
  function(x) paste(rep(x, 3), collapse = ""),
  7 ~ "Not asked",
  8 ~ "Refused",
  9 ~ "Missing",
  .preserve = TRUE
)
