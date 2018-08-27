library(cointosses)
context("Testing Coins")

test_that("Testing coins",{
 mycoin <- coin()

 expect_equal(0.5, mycoin$prob)
 expect_equal("H", mycoin$success)
 expect_equal("T", mycoin$failure)


 expect_error(tossed_coin(c('a', 'b', 'c')))

})
