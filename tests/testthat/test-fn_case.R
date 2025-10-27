words  <- c("caterpillar", "dogwood", "catastrophe", "dogma")
result <- c("feline", "canine", "feline", "canine")

test_that("function and lambda equivalent", {
  expect_equal(
    fn_case(
      words,
      fn = function(x, pattern) {grepl(pattern, x)},
      "cat" ~ "feline",
      "dog" ~ "canine"
    ),
    result
  )

  expect_equal(
    fn_case(
      words,
      fn = function(x, pattern, ...) {grepl(pattern, x, ...)},
      "cat" ~ "feline",
      "dog" ~ "canine"
    ),
    result
  )

  expect_equal(
    fn_case(
      words,
      fn = ~ grepl(.y, .x),
      "cat" ~ "feline",
      "dog" ~ "canine"
    ),
    result
  )
})

test_that("fn_case() ignore.case", {
  expect_equal(
    fn_case(
      words,
      fn = function(x, pattern, ...) {grepl(pattern, x, ...)},
      "cat" ~ "feline",
      "dog" ~ "canine",
      ignore.case = TRUE
    ),
    result
  )

  expect_equal(
    fn_case(
      words,
      fn = ~ grepl(.y, .x),
      "cat" ~ "feline",
      "dog" ~ "canine",
      ignore.case = TRUE
    ),
    result
  )
})

test_that("fn_case() exhaustive", {
  expect_error(
    fn_case(
      words,
      fn = function(x, pattern, ...) {grepl(pattern, x, ...)},
      "cat" ~ "feline",
      .exhaustive = TRUE
    ),
    'The following values were not matched: "dogwood" and "dogma".',
  )

  expect_equal(
    fn_case(
      words,
      fn = function(x, pattern, ...) {grepl(pattern, x, ...)},
      "cat" ~ "feline",
      "dog" ~ "canine",
      .exhaustive = TRUE
    ),
    result
  )
})

test_that("errors", {
  expect_error(fn_case(1:10, function(x) x < 5))
  expect_error(fn_case(1:10, function(x, y) {x + y}, 1 ~ TRUE, 2 ~ FALSE))
})
