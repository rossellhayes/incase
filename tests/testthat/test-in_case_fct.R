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

test_that("in_case_fct with default", {
  x <- 1:10

  expect_equal(
    in_case_fct(
      x %% 2 == 0 ~ "even",
      .default = "odd"
    ),
    factor(rep(c("odd", "even"), 5), levels = c("even", "odd"))
  )

  expect_equal(
    in_case_fct(
      .default = "odd",
      x %% 2 == 0 ~ "even",
    ),
    factor(rep(c("odd", "even"), 5), levels = c("odd", "even"))
  )

  expect_equal(
    x %>%
      in_case_fct(
        . %% 2 == 0 ~ "even",
        .default = "odd"
      ),
    factor(rep(c("odd", "even"), 5), levels = c("even", "odd"))
  )

  expect_equal(
    x %>%
      in_case_fct(
        .default = "odd",
        . %% 2 == 0 ~ "even",
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

test_that("switch_case_fct with default", {
  expect_equal(
    switch_case_fct(
      c("a", "b", "c", "d"),
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple",
      .default = "fruit"
    ),
    factor(
      c("apple", "banana", "cantaloupe", "fruit"),
      levels = c("cantaloupe", "banana", "apple", "fruit")
    )
  )

  expect_equal(
    switch_case_fct(
      c("a", "b", "c", "d"),
      .default = "fruit",
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple",
    ),
    factor(
      c("apple", "banana", "cantaloupe", "fruit"),
      levels = c("fruit", "cantaloupe", "banana", "apple")
    )
  )

  expect_equal(
    switch_case_fct(
      c("a", "b", "c", "d"),
      "c" ~ "cantaloupe",
      .default = "fruit",
      "b" ~ "banana",
      "a" ~ "apple",
    ),
    factor(
      c("apple", "banana", "cantaloupe", "fruit"),
      levels = c("cantaloupe", "fruit", "banana", "apple")
    )
  )
  expect_equal(
    switch_case_fct(
      c("a", "b", "c", "d"),
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      .default = "fruit",
      "a" ~ "apple",
    ),
    factor(
      c("apple", "banana", "cantaloupe", "fruit"),
      levels = c("cantaloupe", "banana", "fruit", "apple")
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

test_that("grep_case_fct with default", {
  expect_equal(
    grep_case_fct(
      c("caterpillar", "dogwood", "catastrophe", "dogma", "ratatouille"),
      "cat" ~ "feline",
      "dog" ~ "canine",
      .default = "other"
    ),
    factor(
      c("feline", "canine", "feline", "canine", "other"),
      levels = c("feline", "canine", "other")
    )
  )

  expect_equal(
    grep_case_fct(
      c("caterpillar", "dogwood", "catastrophe", "dogma", "ratatouille"),
      "cat" ~ "feline",
      .default = "other",
      "dog" ~ "canine",
    ),
    factor(
      c("feline", "canine", "feline", "canine", "other"),
      levels = c("feline", "other", "canine")
    )
  )

  expect_equal(
    grep_case_fct(
      c("caterpillar", "dogwood", "catastrophe", "dogma", "ratatouille"),
      .default = "other",
      "cat" ~ "feline",
      "dog" ~ "canine",
    ),
    factor(
      c("feline", "canine", "feline", "canine", "other"),
      levels = c("other", "feline", "canine")
    )
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

test_that("fn_case_fct with default", {
  expect_equal(
    fn_case_fct(
      c("a", "b", "c", "d"),
      `%in%`,
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple",
      .default = "other"
    ),
    factor(
      c("apple", "banana", "cantaloupe", "other"),
      levels = c("cantaloupe", "banana", "apple", "other")
    )
  )

  expect_equal(
    fn_case_fct(
      c("a", "b", "c", "d"),
      `%in%`,
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      .default = "other",
      "a" ~ "apple",
    ),
    factor(
      c("apple", "banana", "cantaloupe", "other"),
      levels = c("cantaloupe", "banana", "other", "apple")
    )
  )

  expect_equal(
    fn_case_fct(
      c("a", "b", "c", "d"),
      `%in%`,
      "c" ~ "cantaloupe",
      .default = "other",
      "b" ~ "banana",
      "a" ~ "apple",
    ),
    factor(
      c("apple", "banana", "cantaloupe", "other"),
      levels = c("cantaloupe", "other", "banana", "apple")
    )
  )

  expect_equal(
    fn_case_fct(
      c("a", "b", "c", "d"),
      `%in%`,
      .default = "other",
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple",
    ),
    factor(
      c("apple", "banana", "cantaloupe", "other"),
      levels = c("other", "cantaloupe", "banana", "apple")
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

  expect_equal(
    fn_switch_case_fct(
      data,
      7 ~ "Not asked",
      8 ~ "Refused",
      9 ~ "Missing",
      .preserve = TRUE,
      fn = function(x) paste(rep(x, 3), collapse = "")
    ),
    factor(
      c("1", "2", "Missing", "Refused", "Not asked"),
      levels = c("Not asked", "Refused", "Missing", "1", "2")
    )
  )
})

test_that("fn_switch_case_fct() with default", {
  data <- c(1, 2, 999, 888, 777)

  expect_equal(
    fn_switch_case_fct(
      data,
      function(x) paste(rep(x, 3), collapse = ""),
      7 ~ "Not asked",
      8 ~ "Refused",
      9 ~ "Missing",
      .default = "Valid"
    ),
    factor(
      c("Valid", "Valid", "Missing", "Refused", "Not asked"),
      levels = c("Not asked", "Refused", "Missing", "Valid")
    )
  )

  expect_equal(
    fn_switch_case_fct(
      data,
      fn = function(x) paste(rep(x, 3), collapse = ""),
      .default = "Valid",
      7 ~ "Not asked",
      8 ~ "Refused",
      9 ~ "Missing",
    ),
    factor(
      c("Valid", "Valid", "Missing", "Refused", "Not asked"),
      levels = c("Valid", "Not asked", "Refused", "Missing")
    )
  )
})

test_that("fn_switch_case_fct() with arguments", {
  data <- c(1, 2, 999, 888, 777)

  expect_equal(
    fn_switch_case_fct(
      data,
      strrep,
      times = 3,
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

test_that("fn_switch_case_fct() with arguments and default", {
  data <- c(1, 2, 999, 888, 777)

  expect_equal(
    fn_switch_case_fct(
      data,
      strrep,
      times = 3,
      7 ~ "Not asked",
      8 ~ "Refused",
      9 ~ "Missing",
      .default = "Valid"
    ),
    factor(
      c("Valid", "Valid", "Missing", "Refused", "Not asked"),
      levels = c("Not asked", "Refused", "Missing", "Valid")
    )
  )

  expect_equal(
    fn_switch_case_fct(
      data,
      strrep,
      times = 3,
      7 ~ "Not asked",
      8 ~ "Refused",
      .default = "Valid",
      9 ~ "Missing",
    ),
    factor(
      c("Valid", "Valid", "Missing", "Refused", "Not asked"),
      levels = c("Not asked", "Refused", "Valid", "Missing")
    )
  )

  expect_equal(
    fn_switch_case_fct(
      data,
      strrep,
      times = 3,
      7 ~ "Not asked",
      .default = "Valid",
      8 ~ "Refused",
      9 ~ "Missing",
    ),
    factor(
      c("Valid", "Valid", "Missing", "Refused", "Not asked"),
      levels = c("Not asked", "Valid", "Refused", "Missing")
    )
  )

  expect_equal(
    fn_switch_case_fct(
      data,
      strrep,
      7 ~ "Not asked",
      times = 3,
      .default = "Valid",
      8 ~ "Refused",
      9 ~ "Missing",
    ),
    factor(
      c("Valid", "Valid", "Missing", "Refused", "Not asked"),
      levels = c("Not asked", "Valid", "Refused", "Missing")
    )
  )

  expect_equal(
    fn_switch_case_fct(
      data,
      strrep,
      7 ~ "Not asked",
      .default = "Valid",
      times = 3,
      8 ~ "Refused",
      9 ~ "Missing",
    ),
    factor(
      c("Valid", "Valid", "Missing", "Refused", "Not asked"),
      levels = c("Not asked", "Valid", "Refused", "Missing")
    )
  )

  expect_equal(
    fn_switch_case_fct(
      data,
      strrep,
      7 ~ "Not asked",
      .default = "Valid",
      8 ~ "Refused",
      times = 3,
      9 ~ "Missing",
    ),
    factor(
      c("Valid", "Valid", "Missing", "Refused", "Not asked"),
      levels = c("Not asked", "Valid", "Refused", "Missing")
    )
  )

  expect_equal(
    fn_switch_case_fct(
      data,
      strrep,
      7 ~ "Not asked",
      .default = "Valid",
      8 ~ "Refused",
      9 ~ "Missing",
      times = 3,
    ),
    factor(
      c("Valid", "Valid", "Missing", "Refused", "Not asked"),
      levels = c("Not asked", "Valid", "Refused", "Missing")
    )
  )

  expect_equal(
    fn_switch_case_fct(
      data,
      strrep,
      .default = "Valid",
      7 ~ "Not asked",
      8 ~ "Refused",
      9 ~ "Missing",
      times = 3,
    ),
    factor(
      c("Valid", "Valid", "Missing", "Refused", "Not asked"),
      levels = c("Valid", "Not asked", "Refused", "Missing")
    )
  )
})

test_that("exhaustive *_case_fct()", {
  error <- expect_error(
    switch_case_fct(
      c("a", "b", "c"),
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      .exhaustive = TRUE
    ),
    "The following value was not matched: a.",
  )

  expect_equal(error$call[[1]], rlang::sym("switch_case_fct"))

  expect_equal(
    switch_case_fct(
      c("a", "b", "c"),
      "c" ~ "cantaloupe",
      "b" ~ "banana",
      "a" ~ "apple",
      .exhaustive = TRUE
    ),
    factor(
      c("apple", "banana", "cantaloupe"),
      levels = c("cantaloupe", "banana", "apple")
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
