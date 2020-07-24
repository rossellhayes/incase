test_that("formatting without {crayon}", {
  local_mock(requireNamespace = function(...) {FALSE})
  expect_equal(code("code"), "`code`")
  expect_equal(cross_bullet(), paste0("\n", "x"))
})
