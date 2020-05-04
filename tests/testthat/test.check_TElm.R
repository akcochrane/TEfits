context("check_TElm")
library(TEfits)

d_01_mono <- data.frame(timeVar = 1:50,
                        respVar = c(seq(.3,.9,length=25), seq(.9,.91,length=25))+rep(c(0,.01),25),
                        covar1=rep(c(1,0),25),
                        covar2 = rep(c(-3,-1,0,2,5),10))

m <- TElm(respVar ~ timeVar+covar2,d_01_mono,timeVar = 'timeVar',nBoot=1000)

test_that("Check all aspects of the fit to the time variable", {
  expect_equivalent(round(m$bootSummary['timeVar','Estimate'],2),-.74) # entered 20200504
  expect_equivalent(round(m$bootSummary['timeVar','Std..Error'],2),0.02) # entered 20200504
  expect_equivalent(round(m$bootSummary['timeVar','t.value'],1),-39.2) # entered 20200504
  expect_equivalent(round(m$bootSummary['timeVar','ci025'],2),-0.79) # entered 20200504
  expect_equivalent(round(m$bootSummary['timeVar','ci975'],3),-0.71) # entered 20200504
  expect_equivalent(round(m$bootSummary['timeVar','bootP'],3),0) # entered 20200504
  expect_equivalent(round(m$bootSummary['timeVar','dRsq_oos'],2),0.97) # entered 20200504
})
