x <- 1:5
y <- c("1", "2", "fizz", "4", "buzz")

test_that("fizz_buzz", {
  expect_equal(x %>% in_case(TRUE ~ x), x)
  expect_equal(x %>% in_case(x %% 3 == 0 ~ x), c(NA, NA, 3, NA, NA))
  expect_equal(
    in_case(
      x %% 3 == 0 ~ "fizz",
      x %% 5 == 0 ~ "buzz",
      TRUE        ~ x
    ),
    y
  )

  expect_equal(
    x %>%
      in_case(
        . %% 3 == 0 ~ "fizz",
        . %% 5 == 0 ~ "buzz",
        TRUE        ~ x
      ),
    y
  )

  expect_equal(
    x %>%
      in_case(
        . %% 3 == 0 ~ "fizz",
        . %% 5 == 0 ~ "buzz",
        preserve    = TRUE
      ),
    y
  )

  expect_equal(
    in_case(
      x %% 3 == 0 ~ "fizz",
      x %% 5 == 0 ~ "buzz",
      default     = "pass"
    ),
    c("pass", "pass", "fizz", "pass", "buzz")
  )
})


test_that("errors", {
  expect_warning(x %>% in_case(. > 3 ~ "f", preserve = TRUE, default = "p"))
  expect_error(in_case(x %% 3 == 0 ~ "fizz", preserve = TRUE))
  expect_error(in_case(3 ~ "fizz", 5 ~ "buzz", TRUE ~ x))
  expect_error(in_case(y ~ "fizz", 5 ~ "buzz", TRUE ~ x), "y")
  expect_error(
    in_case(letters[1:3] ~ "buzz", TRUE ~ x), "letters[1:3]", fixed = TRUE
  )
  expect_error(in_case(x))
  expect_error(in_case(x, 3 ~ "fizz", 5 ~ "buzz", x))
  expect_error(
    in_case(c(TRUE, FALSE, TRUE) ~ 1, c(FALSE, TRUE) ~ 2),
    "c(FALSE, TRUE)", fixed = TRUE
  )
})


test_that("matches values in order", {
  x <- 1:3
  expect_equal(
    in_case(
      x <= 1 ~ 1,
      x <= 2 ~ 2,
      x <= 3 ~ 3
    ),
    c(1, 2, 3)
  )
})

test_that("unmatched gets missing value", {
  x <- 1:3
  expect_equal(
    in_case(
      x <= 1 ~ 1,
      x <= 2 ~ 2
    ),
    c(1, 2, NA)
  )
})

test_that("missing values can be replaced (#1999)", {
  x <- c(1:3, NA)
  expect_equal(
    in_case(
      x <= 1 ~ 1,
      x <= 2 ~ 2,
      is.na(x) ~ 0
    ),
    c(1, 2, NA, 0)
  )
})

test_that("NA conditions (#2927)", {
  expect_equal(
    in_case(
      c(TRUE, FALSE, NA) ~ 1:3,
      TRUE ~ 4L
    ),
    c(1L, 4L, 4L)
  )
})

test_that("atomic conditions (#2909)", {
  expect_equal(
    in_case(
      TRUE  ~ 1:3,
      FALSE ~ 4:6
    ),
    1:3
  )
  expect_equal(
    in_case(
      NA   ~ 1:3,
      TRUE ~ 4:6
    ),
    4:6
  )
})

test_that("zero-length conditions and values (#3041)", {
  expect_equal(
    in_case(
      TRUE  ~ integer(),
      FALSE ~ integer()
    ),
    integer()
  )
  expect_equal(
    in_case(
      logical() ~ 1,
      logical() ~ 2
    ),
    numeric()
  )
})

test_that("in_case can be used in anonymous functions (#3422)", {
  res <- dplyr::tibble(a = 1:3) %>%
    dplyr::mutate(b = (function(x) in_case(x < 2 ~ TRUE, TRUE ~ FALSE))(a)) %>%
    dplyr::pull()
  expect_equal(res, c(TRUE, FALSE, FALSE))
})

test_that("in_case() can be used inside mutate()", {
  out <- mtcars[1:4, ] %>%
    dplyr::mutate(out = in_case(
      cyl == 4            ~ 1,
      .data[["am"]] == 1  ~ 2,
      TRUE                ~ 0
    )) %>%
    dplyr::pull()
  expect_identical(out, c(2, 2, 1, 0))
})

test_that("can pass quosures to in_case()", {
  fs <- local({
    x <- 3:1
    rlang::quos(
      x < 2 ~ TRUE,
      TRUE ~ FALSE
    )
  })
  expect_identical(in_case(!!!fs), c(FALSE, FALSE, TRUE))
})

test_that("can pass nested quosures to in_case()", {
  fs <- local({
    foo <- mtcars[["cyl"]][1:4]
    rlang::quos(
      !!rlang::quo(foo) == 4 ~ 1,
      TRUE            ~ 0
    )
  })
  expect_identical(in_case(!!!fs), c(0, 0, 1, 0))
})

test_that("can pass unevaluated formulas to in_case()", {
  x <- 6:8
  fs <- rlang::exprs(
    x == 7L ~ TRUE,
    TRUE ~ FALSE
  )
  expect_identical(in_case(!!!fs), c(FALSE, TRUE, FALSE))

  out <- local({
    x <- 7:9
    in_case(!!!fs)
  })
  expect_identical(out, c(TRUE, FALSE, FALSE))
})

test_that("unevaluated formulas can refer to data mask", {
  fs <- rlang::exprs(
    cyl == 4 ~ 1,
    am == 1  ~ 2,
    TRUE     ~ 0
  )
  out <- mtcars[1:4, ] %>% dplyr::mutate(out = in_case(!!!fs)) %>% dplyr::pull()
  expect_identical(out, c(2, 2, 1, 0))
})

test_that("unevaluated formulas can contain quosures", {
  quo <- local({
    n <- 4
    rlang::quo(n)
  })
  fs <- rlang::exprs(
    cyl == !!quo ~ 1,
    am == 1      ~ 2,
    TRUE         ~ 0
  )
  out <- mtcars[1:4, ] %>% dplyr::mutate(out = in_case(!!!fs)) %>% dplyr::pull()
  expect_identical(out, c(2, 2, 1, 0))
})

test_that("NULL inputs are compacted", {
  x <- 1:3

  bool <- FALSE
  out <- in_case(
    x == 2           ~ TRUE,
    if (bool) x == 3 ~ NA,
    TRUE             ~ FALSE
  )
  expect_identical(out, c(FALSE, TRUE, FALSE))

  bool <- TRUE
  out <- in_case(
    x == 2           ~ TRUE,
    if (bool) x == 3 ~ NA,
    TRUE             ~ FALSE
  )
  expect_identical(out, c(FALSE, TRUE, NA))
})
