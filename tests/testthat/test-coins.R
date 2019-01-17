library(cointosses)
context("Testing Coins")

test_that("Testing coins",{
 mycoin <- coin()

 expect_equal(0.5, mycoin$prob)
 expect_equal("H", mycoin$success)
 expect_equal("T", mycoin$failure)

 x <- print(mycoin)
 expect_equal(x,"A Bernoulli trial with success denoted by 'H' failure denoted by 'T', and probability of success 0.5.")

 expect_error(coin(prob = 1.1))

 expect_error(coin(prob = "1.1"))


})


test_that("Testing tossed coins",{
  expect_error(tossed_coin(c('a', 'b', 'c')))

  x <- tossed_coin(c('a', 'b'), success = 'a')

  expect_equal(x$failure, 'b')
  expect_null(x$prob)

  y <- tossed_coin(c('b', 'a'))

  expect_equal(y$success, 'a')
  expect_equal(print(y), c('b', 'a'))
  expect_null(y$prob)


})


test_that("Testing Autocorrelated Coins",{
 x <- autoCorrCoin()

 expect_equal(x$success, "H")

 expect_error(autoCorrCoin(p_given1 = 1.1))
 expect_error(autoCorrCoin(p_given0 = -1.1))
 expect_error(autoCorrCoin(initial_prob  = -1.1))

})
