test_that("switch_case_fct without package loaded", {
  expect_equal(
    incase::switch_case_fct(
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
})

test_that("grep_case_fct without package loaded", {
  expect_equal(
    incase::grep_case_fct(
      c("caterpillar", "dogwood", "catastrophe", "dogma"),
      "cat" ~ "feline",
      "dog" ~ "canine"
    ),
    factor(rep(c("feline", "canine"), 2), levels = c("feline", "canine"))
  )
})
