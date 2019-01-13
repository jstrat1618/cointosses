library(cointosses)
context("Testing Tosses")

test_that("Testing the toss function", {
  should_be_H <- toss(prob = 1)
  expect_equal(should_be_H, "H")
})

test_that("Testing Autocorrelated Coins",{
  x <-
    autoCorrCoin(initial_prob = 1, p_given1 = 0,
                    p_given0 = 1) %>%
    toss(x, n=4)

  expect_equal(x$trials, factor(c("H", "T", "H", "T"), levels = c("H", "T")))
})
