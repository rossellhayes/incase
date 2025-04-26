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
      caps_words,
      "cat" ~ "feline",
      "dog" ~ "canine",
      ignore.case = TRUE
    ),
    c("feline", "canine", "feline", "canine")
  )
})

test_that("grep_case() .exhaustive", {
  words <- c("caterpillar", "dogwood", "catastrophe", "dogma")

  expect_error(
    grep_case(words, "cat" ~ "feline", .exhaustive = TRUE),
    "The following values were not matched: dogwood and dogma.",
  )

  expect_equal(
    grep_case(words, "cat" ~ "feline", "dog" ~ "canine", .exhaustive = TRUE),
    c("feline", "canine", "feline", "canine")
  )
})

test_that("grep_case() with two args", {
  caps_words <- c("caterpillar", "dogwood", "Catastrophe", "DOGMA")

  expect_equal(
    grep_case(
      caps_words,
      "cat" ~ "feline",
      "dog" ~ "canine",
      ignore.case = TRUE,
      perl        = TRUE
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
    grep_case(words, "cat" ~ "feline", "dog" ~ "canine", .preserve = FALSE),
    c("feline", "canine", "feline", "canine", NA)
  )

  expect_equal(
    grep_case(words, "cat" ~ "feline", "dog" ~ "canine", .preserve = TRUE),
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
      .preserve      = TRUE,
      ignore.case   = TRUE
    ),
    c(
      "France", "Germany", "Germany", "Netherlands",
      "Belgium", "Belgium", "Luxembourg", "Italy"
    )
  )
})

test_that("grep_case() with vector LHS", {
  countries <- c(
    "France", "Ostdeutschland", "West Germany", "Nederland",
    "België (Vlaanderen)", "Belgique (Wallonie)", "Luxembourg", "Italia"
  )

  expect_equal(
    grep_case(
      countries,
      c("german", "deutsch") ~ "Germany",
      "belg" ~ "Belgium",
      "nederland" ~ "Netherlands",
      "italia" ~ "Italy",
      ignore.case = TRUE,
      .preserve = TRUE
    ),
    c(
      "France", "Germany", "Germany", "Netherlands",
      "Belgium", "Belgium", "Luxembourg", "Italy"
    )
  )
})

test_that("warning for deprecated argument", {
  words <- c("caterpillar", "dogwood", "catastrophe", "dogma", "ratatouille")

  lifecycle::expect_deprecated(
    preserve <- grep_case(words, "cat" ~ "feline", "dog" ~ "canine", preserve = FALSE),
    "The `preserve` argument of `grep_case()` is deprecated as of incase 0.3.3.",
    fixed = TRUE
  )

  expect_equal(
    preserve,
    grep_case(words, "cat" ~ "feline", "dog" ~ "canine", .preserve = FALSE)
  )
})
