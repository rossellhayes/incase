test_that("in_case_list", {
  expect_equal(
    1:3 %>% in_case_list(. < 2 ~ mtcars, TRUE ~ letters),
    list(mtcars, letters, letters)
  )

  expect_equal(
    dplyr::tibble(x = 1:3) %>%
      dplyr::mutate(y = x %>% in_case_list(. < 2 ~ mtcars, TRUE ~ letters)),
    dplyr::tibble(x = 1:3, y = list(mtcars, letters, letters))
  )

  expect_equal(
    1:3 %>% in_case_list(. < 2 ~ mtcars, .default = letters),
    list(mtcars, letters, letters)
  )

  expect_equal(
    dplyr::tibble(x = 1:3) %>%
      dplyr::mutate(y = x %>% in_case_list(. < 2 ~ mtcars, .default = letters)),
    dplyr::tibble(x = 1:3, y = list(mtcars, letters, letters))
  )

  expect_equal(
    1:3 %>% in_case_list(. < 2 ~ mtcars, .preserve = TRUE),
    list(mtcars, 2, 3)
  )

  expect_equal(
    dplyr::tibble(x = 1:3) %>%
      dplyr::mutate(y = x %>% in_case_list(. < 2 ~ mtcars, .preserve = TRUE)),
    dplyr::tibble(x = 1:3, y = list(mtcars, 2, 3))
  )
})

test_that("switch_case_list", {
  expect_equal(
    1:3 %>% switch_case_list(2 ~ mtcars, .default = letters),
    list(letters, mtcars, letters)
  )

  expect_equal(
    dplyr::tibble(x = 1:3) %>%
      dplyr::mutate(y = switch_case_list(x, 2 ~ mtcars, .default = letters)),
    dplyr::tibble(x = 1:3, y = list(letters, mtcars, letters))
  )
})

test_that("grep_case_list", {
  names <- c("bat", "cat", "dog")

  expect_equal(
    names %>% grep_case_list("a" ~ mtcars, .default = letters),
    list(mtcars, mtcars, letters)
  )

  expect_equal(
    dplyr::tibble(x = names) %>%
      dplyr::mutate(y = grep_case_list(x, "a" ~ mtcars, .default = letters)),
    dplyr::tibble(x = names, y = list(mtcars, mtcars, letters))
  )
})

test_that("fn_case_list", {
  in_herits <- function(x) {
    fn_case_list(
      x,
      fn = base::inherits,
      "character"  ~ data.frame(a = seq_along(x), x = x),
      "numeric"    ~ list(char = as.character(x), x = x, comp = x + 0.5i),
      "integer"    ~ matrix(c(x, x + 100, x * 100), length(x)),
      "matrix"     ~ x[, 1],
      "list"       ~ unlist(x),
      "data.frame" ~ list(colnames(x), lengths(x))
    )
  }

  x <- c("a", "b", "cdef")
  expect_equal(in_herits(x), list(data.frame(a = seq_along(x), x = x)))

  x <- seq(2.5, 9.5)
  expect_equal(
    in_herits(x), list(list(char = as.character(x), x = x, comp = x + 0.5i))
  )

  x <- as.integer(1:3)
  expect_equal(in_herits(x), list(matrix(c(x, x + 100, x * 100), length(x))))

  x <- matrix(1:10, 5)
  expect_equal(in_herits(x), list(x[, 1]))

  x <- data.frame(A = 1:2, B = 3:4)
  expect_equal(in_herits(x), list(list(colnames(x), lengths(x))))

  x <- list(U = 1:2, V = 3:4)
  expect_equal(in_herits(x), list(unlist(x)))
})

test_that("fn_switch_case_list()", {
  data <- c(1, 2, 999, 888, 777)

  expect_equal(
    fn_switch_case_list(
      data,
      function(x) paste(rep(x, 3), collapse = ""),
      7 ~ mtcars,
      8 ~ letters,
      9 ~ mtcars,
      .preserve = TRUE
    ),
    list(1, 2, mtcars, letters, mtcars)
  )
})

test_that("warning for deprecated argument", {
  lifecycle::expect_deprecated(
    default <- 1:3 %>% in_case_list(. < 2 ~ mtcars, default = letters),
    "The `default` argument of `in_case_list()` is deprecated as of incase 0.3.3.",
    fixed = TRUE
  )

  expect_equal(
    default,
    1:3 %>% in_case_list(. < 2 ~ mtcars, .default = letters)
  )

  lifecycle::expect_deprecated(
    preserve <- 1:3 %>% in_case_list(. < 2 ~ mtcars, preserve = TRUE),
    "The `preserve` argument of `in_case_list()` is deprecated as of incase 0.3.3.",
    fixed = TRUE
  )

  expect_equal(
    preserve,
    1:3 %>% in_case_list(. < 2 ~ mtcars, .preserve = TRUE)
  )
})
