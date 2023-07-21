yn <- c(NA, NA, "fizz", NA, "buzz", "fizz", NA, NA, "fizz", NA)
yp <- c(1, 2, "fizz", 4, "buzz", "fizz", 7, 8, "fizz", 10)
yd <- c(
  "bam", "bam", "fizz", "bam", "buzz", "fizz", "bam", "bam", "fizz", "bam"
)

test_that("unpiped unpreserved switch_case()", {
  x <- switch_case(
    1:10,
    3 ~ "fizz",
    5 ~ "buzz",
    6 ~ "fizz",
    9 ~ "fizz",
    preserve = FALSE
  )

  expect_equal(x, yn)

  x <- switch_case(
    1:10,
    3 ~ "fizz",
    5 ~ "buzz",
    6 ~ "fizz",
    9 ~ "fizz"
  )

  expect_equal(x, yn)
})

test_that("unpiped preserved switch_case()", {
  x <- switch_case(
    1:10,
    3 ~ "fizz",
    5 ~ "buzz",
    6 ~ "fizz",
    9 ~ "fizz",
    preserve = TRUE
  )

  expect_equal(x, yp)
})

test_that("unpiped defaulted switch_case()", {
  x <- switch_case(
    1:10,
    3 ~ "fizz",
    5 ~ "buzz",
    6 ~ "fizz",
    9 ~ "fizz",
    default = "bam"
  )

  expect_equal(x, yd)
})

test_that("unpiped vectored switch_case()", {
  x <- switch_case(
    1:10,
    c(3, 6, 9) ~ "fizz",
    5          ~ "buzz"
  )

  expect_equal(x, yn)
})

test_that("piped unpreserved switch_case()", {
  x <- 1:10 %>%
    switch_case(
      3 ~ "fizz",
      5 ~ "buzz",
      6 ~ "fizz",
      9 ~ "fizz",
      preserve = FALSE
    )

  expect_equal(x, yn)

  x <- 1:10 %>%
    switch_case(
      3 ~ "fizz",
      5 ~ "buzz",
      6 ~ "fizz",
      9 ~ "fizz"
    )

  expect_equal(x, yn)
})

test_that("piped preserved switch_case()", {
  x <- 1:10 %>%
    switch_case(
      3 ~ "fizz",
      5 ~ "buzz",
      6 ~ "fizz",
      9 ~ "fizz",
      preserve = TRUE
    )

  expect_equal(x, yp)
})

test_that("piped defaulted switch_case()", {
  x <- 1:10 %>%
    switch_case(
      3 ~ "fizz",
      5 ~ "buzz",
      6 ~ "fizz",
      9 ~ "fizz",
      default = "bam"
    )

  expect_equal(x, yd)
})

test_that("unpiped vectored switch_case()", {
  x <- 1:10 %>%
    switch_case(
      c(3, 6, 9) ~ "fizz",
      5          ~ "buzz"
    )

  expect_equal(x, yn)
})

test_that("default from other variable switch_case()", {
  input  <- dplyr::tibble(a = letters[1:5], b = 1:5)
  output <- dplyr::tibble(a = letters[1:5], b = 1:5, c = c(10, 2:5))

  x <- dplyr::tibble(a = letters[1:5], b = 1:5) %>%
    dplyr::mutate(
      c = switch_case(
        a,
        "a" ~ 10,
        default = b
      )
    )

  expect_equal(x, output)

  output <- dplyr::tibble(a = letters[1:5], b = 1:5, c = 1:5)

  x <- dplyr::tibble(a = letters[1:5], b = 1:5) %>%
    dplyr::mutate(
      c = switch_case(
        a,
        "z" ~ 10,
        default = b
      )
    )

  expect_equal(x, output)
})

test_that("preserve and default", {
  expect_warning(
    switch_case(1:10, 5 ~ "buzz", preserve = TRUE, default = "bam")
  )
})

test_that("arguments can be passed with or without dots", {
  x <- switch_case(
    1:10,
    3 ~ "fizz",
    5 ~ "buzz",
    6 ~ "fizz",
    9 ~ "fizz",
    .preserve = TRUE
  )

  expect_equal(x, yp)

  x <- switch_case(
    1:10,
    3 ~ "fizz",
    5 ~ "buzz",
    6 ~ "fizz",
    9 ~ "fizz",
    .default = "bam"
  )

  expect_equal(x, yd)

  x <- switch_case(
    1:10,
    3 ~ "fizz",
    5 ~ "buzz",
    6 ~ "fizz",
    9 ~ "fizz",
    .preserve = TRUE
  )

  expect_equal(x, yp)

  x <- switch_case(
    3 ~ "fizz",
    5 ~ "buzz",
    6 ~ "fizz",
    9 ~ "fizz",
    .x = 1:10
  )

  expect_equal(x, yn)
})

test_that("fn_switch_case()", {
  data <- c(1, 2, 999, 888, 777)

  expect_equal(
    fn_switch_case(
      data,
      function(x) paste(rep(x, 3), collapse = ""),
      7 ~ "Not asked",
      8 ~ "Refused",
      9 ~ "Missing",
      preserve = TRUE
    ),
    c("1", "2", "Missing", "Refused", "Not asked")
  )
})

test_that("fn_switch_case() errors", {
  expect_error(fn_switch_case(1:10, function(x) x + 5))
})
