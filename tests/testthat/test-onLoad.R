test_that("lengths is backported", {
  expect_silent(incase:::.onLoad())

  if (getRversion() >= numeric_version("3.2.0")) {
    expect_equal(environment(lengths), asNamespace("base"))
  } else {
    expect_equal(environment(lengths), asNamespace("backports"))
  }
})
