test_that("in_case_fct", {
  x <- 1:10

  expect_equal(
    in_case_fct(
      x %% 2 == 0 ~ "even",
      x %% 2 == 1 ~ "odd"
    ),
    factor(rep(c("odd", "even"), 5), levels = c("even", "odd"))
  )

  expect_equal(
    x %>%
      in_case_fct(
        . %% 2 == 0 ~ "even",
        . %% 2 == 1 ~ "odd"
      ),
    factor(rep(c("odd", "even"), 5), levels = c("even", "odd"))
  )

  expect_equal(
    x %>%
      in_case_fct(
        . %% 2 == 1 ~ "odd",
        . %% 2 == 0 ~ "even"
      ),
    factor(rep(c("odd", "even"), 5), levels = c("odd", "even"))
  )
})

test_that("switch_case_fct", {
  expect_equal(
    switch_case_fct(
      c("a", "b", "c"),
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple"
    ),
    factor(
      c("apple", "banana", "cantaloupe"),
      levels = c("cantaloupe", "banana", "apple")
    )
  )

  expect_equal(
    switch_case_fct(
      c("a", "b", "c", "d"),
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple"
    ),
    factor(
      c("apple", "banana", "cantaloupe", NA),
      levels = c("cantaloupe", "banana", "apple")
    )
  )

  expect_equal(
    switch_case_fct(
      c("a", "b", "c", "d"),
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple",
      .preserve = TRUE
    ),
    factor(
      factor(
        c("apple", "banana", "cantaloupe", "d"),
        levels = c("cantaloupe", "banana", "apple", "d")
      )
    )
  )

  expect_equal(
    switch_case_fct(
      c("a", "b", "c", "d"),
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple",
      "d" ~ "banana"
    ),
    factor(
      c("apple", "banana", "cantaloupe", "banana"),
      levels = c("cantaloupe", "banana", "apple")
    )
  )
})

test_that("grep_case_fct", {
  expect_equal(
    grep_case_fct(
      c("caterpillar", "dogwood", "catastrophe", "dogma"),
      "cat" ~ "feline",
      "dog" ~ "canine"
    ),
    factor(rep(c("feline", "canine"), 2), levels = c("feline", "canine"))
  )
})

test_that("fn_case_fct", {
  expect_equal(
    fn_case_fct(
      c("a", "b", "c"),
      `%in%`,
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple"
    ),
    factor(
      c("apple", "banana", "cantaloupe"),
      levels = c("cantaloupe", "banana", "apple")
    )
  )
})

test_that("grep_case_fct() with vector LHS", {
  countries <- c(
    "France", "Ostdeutschland", "West Germany", "Nederland",
    "België (Vlaanderen)", "Belgique (Wallonie)", "Luxembourg", "Italia"
  )

  expect_equal(
    grep_case_fct(
      countries,
      c("german", "deutsch") ~ "Germany",
      "belg" ~ "Belgium",
      "nederland" ~ "Netherlands",
      "italia" ~ "Italy",
      ignore.case = TRUE,
      .preserve = TRUE
    ),
    factor(
      c(
        "France", "Germany", "Germany", "Netherlands",
        "Belgium", "Belgium", "Luxembourg", "Italy"
      ),
      levels = c(
        "Germany", "Belgium", "Netherlands", "Italy", "France", "Luxembourg"
      )
    )
  )
})

test_that("ordered", {
  x <- 1:10

  expect_equal(
    in_case_fct(
      x %% 2 == 0 ~ "even",
      x %% 2 == 1 ~ "odd",
      .ordered = TRUE
    ),
    factor(rep(c("odd", "even"), 5), levels = c("even", "odd"), ordered = TRUE)
  )
})

test_that("errors", {
  x <- 1:15

  expect_error(
    in_case(
      x %% 15 == 0 ~ "fizzbuzz",
      x %% 3  == 0 ~ "fizz",
      x %% 5  == 0 ~ "buzz",
      .preserve = TRUE
    )
  )
})

test_that("fn_switch_case_fct()", {
  data <- c(1, 2, 999, 888, 777)

  expect_equal(
    fn_switch_case_fct(
      data,
      function(x) paste(rep(x, 3), collapse = ""),
      7 ~ "Not asked",
      8 ~ "Refused",
      9 ~ "Missing",
      .preserve = TRUE
    ),
    factor(
      c("1", "2", "Missing", "Refused", "Not asked"),
      levels = c("Not asked", "Refused", "Missing", "1", "2")
    )
  )
})

test_that("fn_switch_case_fct() errors", {
  expect_error(fn_switch_case_fct(1:10, function(x) x + 5))
})

test_that("warning for deprecated argument", {
  lifecycle::expect_deprecated(
    preserve <- switch_case_fct(
      c("a", "b", "c", "d"),
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple",
      preserve = TRUE
    ),
    "The `preserve` argument of `switch_case_fct()` is deprecated as of incase 0.3.3.",
    fixed = TRUE
  )

  expect_equal(
    preserve,
    switch_case_fct(
      c("a", "b", "c", "d"),
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple",
      .preserve = TRUE
    )
  )

  x <- 1:10
  lifecycle::expect_deprecated(
    ordered <- in_case_fct(
      x %% 2 == 0 ~ "even",
      x %% 2 == 1 ~ "odd",
      ordered = TRUE
    ),
    "The `ordered` argument of `in_case_fct()` is deprecated as of incase 0.3.3.",
    fixed = TRUE
  )

  expect_equal(
    ordered,
    ordered <- in_case_fct(
      x %% 2 == 0 ~ "even",
      x %% 2 == 1 ~ "odd",
      .ordered = TRUE
    )
  )
})
