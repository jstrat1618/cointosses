context("Testing Coin Analytics")

test_that("Testing streaks", {
  x <- streaks(c("H", "H", "T", "H", "T", "T", "T"))
  expect_equal(x, c(2,3))
})
