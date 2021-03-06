

context("check_TEfitAll")

library(TEfits)

d <- anstrain

d$absRat <- abs(d$ratio)
d$moreCatA <- as.numeric(d$ratio > 0)

m <- list()

### ###
test_that('TEfitAll runs with identity link and OLS error function',{
  expect_is({
    m$ident_ols <- TEfitAll(d[,c('acc','trialNum')],
                            bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                            control = tef_control(suppressWarnings = T,nTries = 50))
    m$ident_ols},
    'TEfitAll')
})


### ###
test_that('TEfitAll runs with identity link and logcosh error function',{
  skip_on_travis() # times out
  expect_is({
    m$ident_logcosh <- TEfitAll(d[,c('acc','trialNum')],
                                bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                                errFun = 'logcosh',
                                control = tef_control(suppressWarnings = T,nTries = 50))
    m$ident_logcosh},
    'TEfitAll')
})


### ###
test_that('TEfitAll runs with identity link and bernoulli error function',{
  skip_on_travis() # times out
  expect_is({
    m$ident_bern <- TEfitAll(d[,c('acc','trialNum')],
                             bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                             errFun = 'bernoulli',
                             control = tef_control(suppressWarnings = T,nTries = 50))
    m$ident_bern},
    'TEfitAll')
})


### ###
test_that('TEfitAll runs with Weibull link and OLS error function',{
  skip_on_travis() # times out
  expect_is({
    m$weib_ols <- TEfitAll(d[,c('acc','trialNum','absRat')],
                           bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                           linkFun = list(link='weibull',weibullX = 'absRat'),
                           control = tef_control(suppressWarnings = T,nTries = 50)
    )
    m$weib_ols},
    'TEfitAll')
})

### ###
test_that('TEfitAll runs with Weibull link and bernoulli error function',{
  skip_on_travis() # times out
  expect_is({
    m$weib_bern <- TEfitAll(d[,c('acc','trialNum','absRat')],
                            bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                            linkFun = list(link='weibull',weibullX = 'absRat'),
                            errFun = 'bernoulli',
                            control = tef_control(suppressWarnings = T,nTries = 50))
    m$weib_bern},
    'TEfitAll')
})

### ###
test_that('TEfitAll runs with logistic link and OLS error function',{
  skip_on_travis() # times out
  expect_is({
    m$logist_ols <- TEfitAll(d[,c('acc','trialNum','ratio')],
                             bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                             linkFun = list(link='logit',logistX = 'ratio'),
                             control = tef_control(suppressWarnings = T,nTries = 50)
    )
    m$logist_ols},
    'TEfitAll')
})

### ###
test_that('TEfitAll runs with logistic link and bernoulli error function',{
  skip_on_travis() # times out
  expect_is({
    m$logist_bern <- TEfitAll(d[,c('acc','trialNum','ratio')],
                              bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                              linkFun = list(link='logit',logistX = 'ratio'),
                              errFun = 'bernoulli',
                              control = tef_control(suppressWarnings = T,nTries = 50))
    m$logist_bern},
    'TEfitAll')
})

### ###
test_that('TEfitAll runs with d prime link function',{
  skip_on_travis() # times out
  expect_is({
    m$dPrime <- TEfitAll(d[,c('acc','trialNum','moreCatA')],
                         bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                         linkFun = list(link='d_prime',presence = 'moreCatA'),
                         control = tef_control(suppressWarnings = T,nTries = 50)
    )
    m$dPrime},
    'TEfitAll')
})




