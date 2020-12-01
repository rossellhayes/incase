test_that("grep_case() works", {
  words <- c("caterpillar", "dogwood", "catastrophe", "dogma")

  expect_equal(
    grep_case(words, "cat" ~ "feline", "dog" ~ "canine"),
    c("feline", "canine", "feline", "canine")
  )
})

test_that("grep_case() with ignore.case", {
  caps_words <- c("caterpillar", "dogwood", "Catastrophe", "DOGMA")

  expect_equal(
    grep_case(
      caps_words, "cat" ~ "feline", "dog" ~ "canine", ignore.case = TRUE
    ),
    c("feline", "canine", "feline", "canine")
  )
})

test_that("grep_case() with preserve", {
  words <- c("caterpillar", "dogwood", "catastrophe", "dogma", "ratatouille")

  expect_equal(
    grep_case(words, "cat" ~ "feline", "dog" ~ "canine"),
    c("feline", "canine", "feline", "canine", NA)
  )

  expect_equal(
    grep_case(words, "cat" ~ "feline", "dog" ~ "canine", preserve = FALSE),
    c("feline", "canine", "feline", "canine", NA)
  )

  expect_equal(
    grep_case(words, "cat" ~ "feline", "dog" ~ "canine", preserve = TRUE),
    c("feline", "canine", "feline", "canine", "ratatouille")
  )

  countries <- c(
    "France", "Ostdeutschland", "Westdeutschland", "Nederland",
    "België (Vlaanderen)", "Belgique (Wallonie)", "Luxembourg", "Italia"
  )

  expect_equal(
    grep_case(
      countries,
      "Deutschland" ~ "Germany",
      "Belg"        ~ "Belgium",
      "Nederland"   ~ "Netherlands",
      "Italia"      ~ "Italy",
      preserve      = TRUE,
      ignore.case   = TRUE
    ),
    c(
      "France", "Germany", "Germany", "Netherlands",
      "Belgium", "Belgium", "Luxembourg", "Italy"
    )
  )

})
