context("check_params")
library(TEfits)

d_01_mono <- data.frame(timeVar = 1:50, respVar = c(seq(.3,.9,length=25),seq(.9,.91,length=25))+rep(c(0,.01),25),covar1=rep(c(1,2),25),covar2 = rep(c(-3,-1,0,2,5),10))
m <- TEfit(d_01_mono[,c('respVar','timeVar')])

test_that("3-parameter expo model params", {
  expect_equal(as.numeric(round(m$model$par['pAsym'],3)), 0.978)
  expect_equal(as.numeric(round(m$model$par['pRate'],3)), 3.379)
  expect_equal(as.numeric(round(m$model$par['pStart'],3)), 0.240)
})
