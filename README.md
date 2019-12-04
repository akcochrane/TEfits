<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview to Time-Evolving fits
------------------------------

The **TEfits** package streamlines nonlinear regression, such as that encountered in analysis of learning. The **TEfits** package is intended to assist in the implementation and interpretation of nonlinear regression with a heavy emphasis on interpretability of parameters. Parameters fit by **TEfits** are meant to reflect human-interpretable representations of time-evolving processes. Error functions, nonlinear ("change") functions linking parameters and time to predicted values, parameter and prediction boundaries, and goodness-of-fit indices are intended to be clear and adjustable. An equal emphasis is on ease of use: minimal arguments are necessary to begin using the primary function, `TEfit()`, and many common tasks are fully automated (e.g., optimization starting points, bootstrapping).

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
