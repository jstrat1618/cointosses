library(cointosses)
context("Testing Tosses")

test_that("Testing the toss function", {
  should_be_H <- toss(prob = 1)
  expect_equal(should_be_H, "H")
})
