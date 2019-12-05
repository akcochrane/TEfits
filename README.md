<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview to Time-Evolving fits
------------------------------

The **TEfits** package streamlines nonlinear regression, such as that encountered in analysis of learning. The **TEfits** package is intended to assist in the implementation and interpretation of nonlinear regression with a heavy emphasis on interpretability of parameters. Parameters fit by **TEfits** are meant to reflect human-interpretable representations of time-evolving processes such as starting values, number of trials until 50% of change, or asymptotic values. Error functions, nonlinear ("change") functions linking parameters and time to predicted values, parameter and prediction boundaries, and goodness-of-fit indices are intended to be clear and adjustable. An equal emphasis is on ease of use: minimal arguments are necessary to begin using the primary function, `TEfit()`, and many common tasks are fully automated (e.g., optimization starting points, bootstrapping).

``` r
dat <- data.frame(resp=log(1:30),trialNum=1:30)
mod <- TEfit(dat[,c('resp','trialNum')])

plot(mod,plot_title='Time-evolving fit of artificial data')
```

![](README_files/figure-markdown_github/simple_model-1.png)

``` r
summary(mod)
```

    ## 
    ## >> Call: resp~(pAsym) + ((pStart) - (pAsym)) * 2^((1 - trialNum)/(2^(pRate)))
    ## 
    ## >> Converged: TRUE 
    ## 
    ## >> Fit Values:
    ##        Estimate
    ## pAsym     3.372
    ## pStart    0.316
    ## pRate     2.543
    ## 
    ## >> Goodness-of-fit:
    ##           err nullErr nPars nObs     Fval Pval  Rsquared       BIC   nullBIC
    ## ols 0.2760858  20.974     3   30 1012.083    0 0.9868368 -130.4436 -7.336215
    ##      deltaBIC
    ## ols -123.1074
    ## 
    ## >> Test of change in nonindependence:
    ##                                            rawSpearman modelConditionalSpearman
    ## Nonindependence between resp and trialNum:           1             0.0002224694
    ##                                            proportionalSpearmanChange
    ## Nonindependence between resp and trialNum:               0.0002224694
    ##                                            pValSpearmanChange
    ## Nonindependence between resp and trialNum:                  0

An example of a double-exponential fit using a Bernoulli response distribution, with 50 bootstrapped fits.

``` r
dat <- data.frame(resp=log(1:30)/log(31),trialNum=1:30)
mod <- TEfit(dat[,c('resp','trialNum')], 
             errFun='bernoulli',
             changeFun='expo_double',
             bootPars=list(nBoots=50))
```

    ## 
    ## Warning: model did not converge at tol = 0.05 . Consider respecifying, allowing more runs, or increasing the convergence tolerance.

``` r
plot(mod,plot_title='Time-evolving fit of artificial data')
```

![](README_files/figure-markdown_github/model_a-1.png)

``` r
summary(mod)
```

    ## 
    ## >> Call: resp~(pAsym) + ((pStart) - (pAsym)) * 0.5 * 2^((1 - trialNum)/(2^(pRateA))) + ((pStart) - (pAsym)) * 0.5 * 2^((1 - trialNum)/(2^(pRateB)))
    ## 
    ## >> Converged: FALSE 
    ## >> Max runs: 500  -- Tolerance: 0.05 
    ## 
    ## >> Fit Values:
    ##        Estimate   Q025  Q975 pseudoSE
    ## pAsym     0.083  0.066 0.923    0.219
    ## pRateA    2.280  1.122 3.790    0.681
    ## pRateB   -0.240 -0.431 0.412    0.215
    ## pStart    0.717  0.049 0.978    0.237
    ## 
    ## >> Goodness-of-fit:
    ##             err nullErr nPars nObs   BIC nullBIC deltaBIC
    ## bernoulli 1e+15 17.6538     4   30 2e+15 38.7088    2e+15
    ## 
    ## >> Test of change in nonindependence:
    ##                                            rawSpearman modelConditionalSpearman
    ## Nonindependence between resp and trialNum:           1                       -1
    ##                                            proportionalSpearmanChange
    ## Nonindependence between resp and trialNum:                          1
    ##                                            pValSpearmanChange
    ## Nonindependence between resp and trialNum:                NaN
    ## 
    ## >> Percent of resamples predicting an increase in values: 54 
    ## 
    ## >> Timepoint at which resampled estimates diverge from timepoint 1, with Cohen's d>1: NA 
    ## 
    ## >> Bootstrapped parameter correlations:
    ##         pAsym pStart pRateA pRateB err
    ## pAsym   1.000 -0.220 -0.254  0.009  NA
    ## pStart -0.220  1.000  0.089 -0.024  NA
    ## pRateA -0.254  0.089  1.000 -0.142  NA
    ## pRateB  0.009 -0.024 -0.142  1.000  NA
    ## err        NA     NA     NA     NA   1
