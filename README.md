<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview to Time-Evolving fits
------------------------------

The **TEfits** package streamlines nonlinear regression, such as that encountered in analysis of learning. The **TEfits** package is intended to assist in the implementation and interpretation of nonlinear regression with a heavy emphasis on interpretability of parameters. Parameters fit by **TEfits** are meant to reflect human-interpretable representations of time-evolving processes. Error functions, nonlinear ("change") functions linking parameters and time to predicted values, parameter and prediction boundaries, and goodness-of-fit indices are intended to be clear and adjustable. An equal emphasis is on ease of use: minimal arguments are necessary to begin using the primary function, `TEfit()`, and many common tasks are fully automated (e.g., optimization starting points, bootstrapping).

``` r
dat <- data.frame(resp=c(seq(0,2,.1),rep(2,9)),trialNum=1:30)
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
    ## pAsym     2.800
    ## pStart   -0.124
    ## pRate     3.704
    ## 
    ## >> Goodness-of-fit:
    ##           err nullErr nPars nObs     Fval Pval Rsquared       BIC nullBIC
    ## ols 0.2730278      14     3   30 678.7373    0 0.980498 -130.7778 -19.463
    ##      deltaBIC
    ## ols -111.3148
    ## 
    ## >> Test of change in nonindependence:
    ##                                            rawSpearman modelConditionalSpearman
    ## Nonindependence between resp and trialNum:   0.9814747               0.04916574
    ##                                            proportionalSpearmanChange
    ## Nonindependence between resp and trialNum:                 0.05009374
    ##                                            pValSpearmanChange
    ## Nonindependence between resp and trialNum:                  0
