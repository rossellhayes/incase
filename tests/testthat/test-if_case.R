x <- c(2, NA, 1)
y <- c("h", "m", "l")

test_that("if_case()", {
  expect_equal(if_case(x < 2, "l", "h", "m"), y)
  expect_equal({x < 2} %>% if_case("l", "h", "m"), y)
  expect_equal(x %>% if_case(. < 2, "l", "h", "m"), y)
  expect_equal(
    x %>% if_case(condition = . < 2, true = "l", false = "h", missing = "m"), y
  )
  expect_equal(
    x %>% if_case(true = "l", false = "h", missing = "m", condition = . < 2), y
  )
  expect_equal(x %>% if_case(condition = . < 2, true = "l", "h", "m"), y)
  expect_equal(x %>% if_case(true = "l", "h", "m", condition = . < 2), y)
})

test_that("lazy-ish eval of if_case()", {
  expect_equal(if_case(TRUE,  TRUE,   stop(), stop()), TRUE)
  expect_equal(if_case(FALSE, stop(), FALSE,  stop()), FALSE)
  expect_equal(if_case(NA,    stop(), stop(), NA),     NA)

  expect_equal(TRUE  %>% if_case(TRUE,   stop(), stop()), TRUE)
  expect_equal(FALSE %>% if_case(stop(), FALSE,  stop()), FALSE)
  expect_equal(NA    %>% if_case(stop(), stop(), NA),     NA)
})

test_that("if_case() errors", {
  expect_error(if_case(x < 2, "l", "h", "m", "z"), "z")
  expect_error(
    if_case(x < 2, rep("l", 2), rep("h", 3), rep("m", 4)),
    "true.*missing"
  )
  expect_error(
    if_case(c(TRUE, FALSE, NA), rep("t", 3), rep("f", 4), rep("m", 5)),
    "false.*missing"
  )
  expect_error(if_case(x, "l", "h", "m"))
})

test_that("scalar true and false are vectorised", {
  x <- c(TRUE, TRUE, FALSE, FALSE)
  expect_equal(if_case(x, 1, 2), c(1, 1, 2, 2))
})

test_that("vector true and false are ok", {
  x <- c(-1, 0, 1)

  expect_equal(if_case(x < 0, x, 0), c(-1, 0, 0))
  expect_equal(if_case(x > 0, x, 0), c(0, 0, 1))
})

test_that("missing values are missing", {
  expect_equal(if_case(c(TRUE, NA, FALSE), -1, 1), c(-1, NA, 1))
})

test_that("works with lists", {
  x <- list(1, 2, 3)

  expect_equal(
    if_case(c(TRUE, TRUE, FALSE), x, list(NULL)),
    list(1, 2, NULL)
  )
})
