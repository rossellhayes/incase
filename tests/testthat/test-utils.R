test_that("first_incase_frame()", {
  expect_equal(
    first_incase_frame(),
    rlang::current_env()
  )

  expect_equal(
    first_incase_frame_parent(),
    rlang::global_env()
  )
})
