

context("check_TEfitAll")

library(TEfits)
d <- anstrain

d$absRat <- abs(d$ratio)
d$moreCatA <- as.numeric(d$ratio > 0)

m <- list()
test_that('TEfitAll runs with various link and error functions',{

  expect_success({
m$ident_ols <- TEfitAll(d[,c('acc','trialNum')],
                        bootPars = tef_bootList(resamples = 20),groupingVar = d$subID)
  })
  expect_success({
    m$ident_logcosh <- TEfitAll(d[,c('acc','trialNum')],
                                bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                             errFun = 'logcosh')
  })
  expect_success({
m$ident_bern <- TEfitAll(d[,c('acc','trialNum')],
                         bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                         errFun = 'bernoulli')
})
expect_success({
m$weib_ols <- TEfitAll(d[,c('acc','trialNum','absRat')],
                       bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                        linkFun = list(link='weibull',weibullX = 'absRat')
                        )
})
expect_success({
m$weib_bern <- TEfitAll(d[,c('acc','trialNum','absRat')],
                        bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                        linkFun = list(link='weibull',weibullX = 'absRat'),
                        errFun = 'bernoulli')
})
expect_success({
m$logist_ols <- TEfitAll(d[,c('acc','trialNum','ratio')],
                         bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                       linkFun = list(link='logit',logistX = 'ratio')
)
})
expect_success({
m$logist_bern <- TEfitAll(d[,c('acc','trialNum','ratio')],
                          bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                        linkFun = list(link='logit',logistX = 'absRat'),
                        errFun = 'bernoulli')
})
expect_success({
m$dPrime <- TEfitAll(d[,c('acc','trialNum','moreCatA')],
                     bootPars = tef_bootList(resamples = 20),groupingVar = d$subID,
                     linkFun = list(link='d_prime',presence = 'moreCatA')
                     )
})
})
