context("check_TEfit")
library(TEfits)

d_01_mono <- data.frame(timeVar = 1:50,
                        respVar = c(seq(.3,.9,length=25), seq(.9,.91,length=25))+rep(c(0,.01),25),
                        covar1=rep(c(1,0),25),
                        covar2 = rep(c(-3,-1,0,2,5),10))
m <- TEfit(d_01_mono[,c('respVar','timeVar')])

## ##
test_that("3-parameter expo model params", {
  expect_equal(as.numeric(round(m$model$par['pAsym'],3)), 0.978) # entered 20200502
  expect_equal(as.numeric(round(m$model$par['pRate'],3)), 3.379) # entered 20200502
  expect_equal(as.numeric(round(m$model$par['pStart'],3)), 0.240) # entered 20200502
  expect_equal(as.numeric(round(unique(m$modList$nullYhat),2)), 0.76) # entered 20200502
})


## ## boundaries
test_that('model boundaries are correct',{
  expect_equivalent(as.numeric(round(m$modList$y_lim[1],3)),-1E7) # entered 20200502
  expect_equivalent(as.numeric(round(m$modList$y_lim[2],3)), 1E7) # entered 20200502
  expect_equivalent(as.numeric(round(m$modList$rate_lim[1],2)),1.02) # entered 20200502
  expect_equivalent(as.numeric(round(m$modList$rate_lim[2],2)), 4.61) # entered 20200502
})

## ##
m_bounds <- tef_getBounds(m$modList)
test_that('tef_getBounds output is correct',{
  expect_equivalent(length(m_bounds$parLims$parMax),3) # entered 20200502
  expect_equivalent(length(m_bounds$parLims$parMin),3) # entered 20200502
  expect_equivalent(length(m_bounds$parGuessBounds$parMax),3) # entered 20200502
  expect_equivalent(length(m_bounds$parGuessBounds$parMin),3) # entered 20200502
  expect_equivalent(round(m_bounds$parGuessBounds$parMax,2),c(.92, 0.92, 4.61)) # entered 20200502
  expect_equivalent(round(m_bounds$parGuessBounds$parMin,2),c(.30, 0.30,1.02)) # entered 20200502
})

## ##
m_dPrime <- TEfit(d_01_mono[,c('respVar','timeVar','covar1')],
                  linkFun = list(link='d_prime',presence='covar1'),
                  changeFun = 'weibull',
                  errFun='logcosh')
test_that("weibull change in d-prime with logcosh error: model params", {
  expect_equal(as.numeric(round(m_dPrime$model$par['pAsym'],2)), 2.68) # entered 20200504
  expect_equal(as.numeric(round(m_dPrime$model$par['pRate'],2)), 3.93) # entered 20200504
  expect_equal(as.numeric(round(m_dPrime$model$par['pStart'],2)), -0.59) # entered 20200504
  expect_equal(as.numeric(round(m_dPrime$model$par['pShape'],2)), 1.16) # entered 20200504
  expect_equal(as.numeric(round(unique(m_dPrime$modList$nullYhat),2)), 1.85) # entered 20200504
})

## ##
m_logit <- TEfit(anstrain[,c('resp','trialNum','ratio')],
                 linkFun = list(link='logit',logistX='ratio'),
                 changeFun = 'power')
test_that("power change in logistic threshold: model params", {
  expect_equal(as.numeric(round(m_logit$model$par['threshAsym'],2)), 0.15) # entered 20200504
  expect_equal(as.numeric(round(m_logit$model$par['threshRate'],1)), 4.8) # entered 20200504
  expect_equal(as.numeric(round(m_logit$model$par['threshStart'],2)), 0.47) # entered 20200504
  expect_equal(as.numeric(round(m_logit$model$par['bias'],2)), 0.09) # entered 20200504
  expect_equal(as.numeric(round(mean(m_logit$modList$nullYhat),2)), 0.42) # entered 20200504
})

## ##
anstrain$abs_ratio <- abs(anstrain$ratio)
m_weibull<- TEfit(anstrain[,c('acc','trialNum','abs_ratio')],
                 linkFun = list(link='weibull',weibullX='abs_ratio'),
                 errFun='bernoulli')
test_that("3-parameter exponential change in weibull threshold and bernoulli error: model params", {
  expect_equal(as.numeric(round(m_weibull$model$par['threshAsym'],2)), 0.19) # entered 20200504
  expect_equal(as.numeric(round(m_weibull$model$par['threshRate'],1)), 4.3) # entered 20200504
  expect_equal(as.numeric(round(m_weibull$model$par['threshStart'],2)), 0.37) # entered 20200504
  expect_equal(as.numeric(round(m_weibull$model$par['weibull_shape'],2)), 1.28) # entered 20200504
  expect_equal(as.numeric(round(mean(m_weibull$modList$nullYhat),2)), 0.78) # entered 20200504
})

## ##
test_that('model specification is correct',{
  expect_equivalent(paste(m$modList$evalFun,collapse=''),'((pAsym) + ((pStart) - (pAsym)) * 2^((1 - timeVar)/(2^(pRate)))') # entered 20200502
  expect_equivalent(paste(m$modList$null_pNames,collapse=''),'Intercept') # entered 20200502
  expect_equivalent(paste(m_dPrime$modList$evalFun,collapse=''),'((pAsym) + ((pStart) - (pAsym)) * 2^(-((timeVar - 1)/(2^(pRate)))^(2^pShape))') # entered 20200502
  expect_equivalent(paste(m_logit$modList$evalFun,collapse=''),'+0.005(1 - 2 * 0.005)/(1 + 3^(((bias) - ratio)/(((threshAsym) + ((threshStart) - (threshAsym)) * (trialNum - 0)^(log(0.25)/log(2^threshRate))))))') # entered 20200502
  expect_equivalent(paste(m_weibull$modList$evalFun,collapse=''),'+0.5((1 - 0.5) - 0.005) * (1 - 2.0204^(-(abs_ratio/(((threshAsym) + ((threshStart) - (threshAsym)) * 2^((1 - trialNum)/(2^(threshRate))))))^weibull_shape))') # entered 20200502
})

## ##
test_that('errors occur after improper specification',{
  expect_error(TEfit(data.frame(yVar = letters[1:11],tVar = 1:11))) # string response fails
  expect_error(TEfit(data.frame(yVar = 1:11,tVar = letters[1:11]))) # string time fails

  expect_error(TEfit(data.frame(yVar = factor(letters[1:11]),tVar = 1:11))) # factor response fails
  expect_error(TEfit(data.frame(yVar = 1:11,tVar = factor(letters[1:11])))) # factor time fails
  })
