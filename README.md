<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview to Time-Evolving fits
------------------------------

The **TEfits** package streamlines nonlinear regression such as that encountered in analysis of learning. The **TEfits** package is intended to assist in the implementation and interpretation of nonlinear regression with a heavy emphasis on interpretability of parameters. Parameters fit by **TEfits** are meant to reflect human-interpretable representations of time-evolving processes such as starting values, number of trials until 50% of change, or asymptotic values. Error functions, nonlinear ("change") functions linking parameters and time to predicted values, parameter and prediction boundaries, and goodness-of-fit indices are intended to be clear and adjustable. An equal emphasis is on ease of use: minimal arguments are necessary to begin using the primary function, `TEfit()`, and many common tasks are fully automated (e.g., optimization starting points, bootstrapping).

``` r
dat <- data.frame(resp=log(6:35),trialNum=1:30)
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
    ## pAsym     3.876
    ## pStart    1.840
    ## pRate     3.509
    ## 
    ## >> Goodness-of-fit:
    ##             err nullErr nPars nObs     Fval Pval  Rsquared       BIC   nullBIC
    ## ols 0.008682719 7.34803     3   30 11411.31    0 0.9988184 -234.2249 -38.80176
    ##      deltaBIC
    ## ols -195.4232
    ## 
    ## >> Test of change in nonindependence:
    ##                                            rawSpearman modelConditionalSpearman
    ## Nonindependence between resp and trialNum:           1               0.05494994
    ##                                            proportionalSpearmanChange
    ## Nonindependence between resp and trialNum:                 0.05494994
    ##                                            pValSpearmanChange
    ## Nonindependence between resp and trialNum:                  0

An example of a learning fit using a Bernoulli response distribution, with 40 bootstrapped fits.

``` r
dat <- data.frame(resp=log(6:35)/log(36),trialNum=1:30)
mod <- TEfit(dat[,c('resp','trialNum')], 
             errFun='bernoulli',
             bootPars=list(nBoots=40))
plot(mod,plot_title='Time-evolving fit of artificial data with 95% CI from 40 bootstrapped fits')
```

![](README_files/figure-markdown_github/model_a-1.png)

``` r
summary(mod)
```

    ## 
    ## >> Call: resp~(pAsym) + ((pStart) - (pAsym)) * 2^((1 - trialNum)/(2^(pRate)))
    ## 
    ## >> Converged: TRUE 
    ## 
    ## >> Fit Values:
    ##        Estimate  Q025  Q975 pseudoSE
    ## pAsym     1.000 1.000 1.000    0.000
    ## pRate     2.922 2.855 3.012    0.040
    ## pStart    0.477 0.462 0.494    0.008
    ## 
    ## >> Goodness-of-fit:
    ##                err  nullErr nPars nObs      BIC  nullBIC deltaBIC
    ## bernoulli 12.57908 14.47716     3   30 35.36175 32.35552  3.00623
    ## 
    ## >> Test of change in nonindependence:
    ##                                            rawSpearman modelConditionalSpearman
    ## Nonindependence between resp and trialNum:           1               -0.1586207
    ##                                            proportionalSpearmanChange
    ## Nonindependence between resp and trialNum:                  0.1586207
    ##                                            pValSpearmanChange
    ## Nonindependence between resp and trialNum:                  0
    ## 
    ## >> Percent of resamples predicting an increase in values: 100 
    ## 
    ## >> Timepoint at which resampled estimates diverge from timepoint 1, with Cohen's d>1: 2 
    ## 
    ## >> Bootstrapped parameter correlations:
    ##         pAsym pStart  pRate    err
    ## pAsym   1.000 -0.322 -0.362 -0.448
    ## pStart -0.322  1.000  0.788  0.640
    ## pRate  -0.362  0.788  1.000  0.792
    ## err    -0.448  0.640  0.792  1.000
