context("check_params")
library(TEfits)

d_01_mono <- data.frame(timeVar = 1:50,
                        respVar = c(seq(.3,.9,length=25), seq(.9,.91,length=25))+rep(c(0,.01),25),
                        covar1=rep(c(1,2),25),
                        covar2 = rep(c(-3,-1,0,2,5),10))
m <- TEfit(d_01_mono[,c('respVar','timeVar')])

test_that("3-parameter expo model params", {
  expect_equal(as.numeric(round(m$model$par['pAsym'],3)), 0.978) # entered 20200502
  expect_equal(as.numeric(round(m$model$par['pRate'],3)), 3.379) # entered 20200502
  expect_equal(as.numeric(round(m$model$par['pStart'],3)), 0.240) # entered 20200502
  expect_equal(as.numeric(round(unique(m$modList$nullYhat),2)), 0.76) # entered 20200502
})



test_that('model specification is correct',{
  expect_equivalent(paste(m$modList$evalFun,collapse=''),'((pAsym) + ((pStart) - (pAsym)) * 2^((1 - timeVar)/(2^(pRate)))') # entered 20200502
  expect_equivalent(paste(m$modList$null_pNames,collapse=''),'Intercept') # entered 20200502
})

## ##
## boundaries
## ##
test_that('model boundaries are correct',{
  expect_equivalent(as.numeric(round(m$modList$y_lim[1],3)),-1E7) # entered 20200502
  expect_equivalent(as.numeric(round(m$modList$y_lim[2],3)), 1E7) # entered 20200502
  expect_equivalent(as.numeric(round(m$modList$rate_lim[1],2)),1.02) # entered 20200502
  expect_equivalent(as.numeric(round(m$modList$rate_lim[2],2)), 4.61) # entered 20200502
})

m_bounds <- tef_getBounds(m$modList)
test_that('tef_getBounds output is correct',{
  expect_equivalent(length(m_bounds$parLims$parMax),3) # entered 20200502
  expect_equivalent(length(m_bounds$parLims$parMin),3) # entered 20200502
  expect_equivalent(length(m_bounds$parGuessBounds$parMax),3) # entered 20200502
  expect_equivalent(length(m_bounds$parGuessBounds$parMin),3) # entered 20200502
  expect_equivalent(round(m_bounds$parGuessBounds$parMax,2),c(.92, 0.92, 4.61)) # entered 20200502
  expect_equivalent(round(m_bounds$parGuessBounds$parMin,2),c(.30, 0.30,1.02)) # entered 20200502
})

