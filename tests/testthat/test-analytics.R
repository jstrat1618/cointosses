context("Testing Coin Analytics")

test_that("Testing streaks", {
  x <- streaks(c("H", "H", "T", "H", "T", "T", "T"))
  expect_equal(x, c(2,3))

  set.seed(2018)
  x <- coin()
  y <- toss(x, 50)
  out <- streaks(y)

  expect_equal(max(out$runs),6)
})


test_that("Testing summary statistics ", {
  set.seed(2018)
  x <- coin()
  y <- toss(x, 10)


  expect_equal(sum(y),3)
  mean(y, 0.3)

})
