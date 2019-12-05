<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview to Time-Evolving fits
------------------------------

Data is described, interpreted, and tested using indices such as d prime, mean, or psychometric function threshold. The **TEfits** package serves to allow the same questions to be asked about time-evolving aspects of these indices, such as the starting level, the amount of time that the index takes to change, and the asymptotic level of that index. Nonlinear regression applied to time-evolving functions is made as intuitive and painless as is feasible, with many extensions if desired.

The **TEfits** package has a heavy emphasis on interpretability of parameters. As far as possible, parameters fit by **TEfits** are meant to reflect human-interpretable representations of time-evolving processes. Error functions, nonlinear ("change") functions linking predicted values to parameters and time, parameter and prediction boundaries, and goodness-of-fit indices are intended to be clear and adjustable. An equal emphasis is on ease of use: minimal arguments are necessary to begin using the primary function, `TEfit()`, and many common tasks are fully automated (e.g., optimization starting points, bootstrapping).

``` r
dat <- data.frame(response=log(2:31),trial_number=1:30)
mod <- TEfit(dat[,c('response','trial_number')])

plot(mod,plot_title='Time-evolving fit of artificial data')
```

![](README_files/figure-markdown_github/simple_model-1.png)

``` r
summary(mod)
```

    ## 
    ## >> Call: response~(pAsym) + ((pStart) - (pAsym)) * 2^((1 - trial_number)/(2^(pRate)))
    ## 
    ## >> Converged: TRUE 
    ## 
    ## >> Fit Values:
    ##        Estimate
    ## pAsym     3.522
    ## pStart    0.869
    ## pRate     2.866
    ## 
    ## >> Goodness-of-fit:
    ##            err nullErr nPars nObs     Fval Pval  Rsquared       BIC   nullBIC
    ## ols 0.09355702 15.2815     3   30 2191.575    0 0.9938778 -162.9079 -16.83544
    ##      deltaBIC
    ## ols -146.0724
    ## 
    ## >> Test of change in nonindependence:
    ##                                                    rawSpearman
    ## Nonindependence between response and trial_number:           1
    ##                                                    modelConditionalSpearman
    ## Nonindependence between response and trial_number:               0.03537264
    ##                                                    proportionalSpearmanChange
    ## Nonindependence between response and trial_number:                 0.03537264
    ##                                                    pValSpearmanChange
    ## Nonindependence between response and trial_number:                  0

An example of a learning fit using a Bernoulli response distribution, with 40 bootstrapped fits.

``` r
dat <- data.frame(response=log(2:31)/log(32),trial_number=1:30)
mod <- TEfit(dat[,c('response','trial_number')], 
             errFun='bernoulli',
             bootPars=list(nBoots=40))
plot(mod,plot_title='Time-evolving fit of artificial data with 95% CI from 40 bootstrapped fits')
```

![](README_files/figure-markdown_github/model_boot-1.png)

``` r
summary(mod)
```

    ## 
    ## >> Call: response~(pAsym) + ((pStart) - (pAsym)) * 2^((1 - trial_number)/(2^(pRate)))
    ## 
    ## >> Converged: TRUE 
    ## 
    ## >> Fit Values:
    ##        Estimate  Q025  Q975 pseudoSE
    ## pAsym     1.000 1.000 1.000    0.000
    ## pRate     2.740 2.658 2.841    0.047
    ## pStart    0.231 0.209 0.278    0.018
    ## 
    ## >> Goodness-of-fit:
    ##                err  nullErr nPars nObs      BIC  nullBIC    deltaBIC
    ## bernoulli 13.42284 16.83409     3   30 37.04928 37.06937 -0.02009175
    ## 
    ## >> Test of change in nonindependence:
    ##                                                    rawSpearman
    ## Nonindependence between response and trial_number:           1
    ##                                                    modelConditionalSpearman
    ## Nonindependence between response and trial_number:              -0.03581758
    ##                                                    proportionalSpearmanChange
    ## Nonindependence between response and trial_number:                 0.03581758
    ##                                                    pValSpearmanChange
    ## Nonindependence between response and trial_number:                  0
    ## 
    ## >> Percent of resamples predicting an increase in values: 100 
    ## 
    ## >> Timepoint at which resampled estimates diverge from timepoint 1, with Cohen's d>1: 2 
    ## 
    ## >> Bootstrapped parameter correlations:
    ##         pAsym pStart pRate    err
    ## pAsym   1.000 -0.115 0.087 -0.474
    ## pStart -0.115  1.000 0.884  0.433
    ## pRate   0.087  0.884 1.000  0.447
    ## err    -0.474  0.433 0.447  1.000

An example of fitting a given model to subsets of data (e.g., individual participants within a behavioral study).

``` r
dat <- data.frame(response=c(dat$response,dat$response*1.1,dat$response*1.2,dat$response*1.3),trial_number=rep(1:30,4),group=rep(letters[1:4],each=30))

mod <- TEfitAll(dat[,c('response','trial_number')], 
             groupingVar = dat$group)
```

    ## . . . .

``` r
plot(mod)
```

![](README_files/figure-markdown_github/model_groups-1.png)

``` r
summary(mod)
```

    ## 
    ## >> Call: response ~ (pAsym) + ((pStart) - (pAsym)) * 2^((1 - trial_number)/(2^(pRate)))
    ## 
    ## >> Overall effects:
    ##             pAsym     pStart        pRate
    ## mean   1.16873035 0.28830871 2.866463e+00
    ## stdErr 0.06559772 0.01618762 4.534049e-05
    ## 
    ##                err   nullErr nPars nObs         Fval Pval     Rsquared
    ## mean   0.010398404 1.6984638     3   30 2.191575e+03    0 9.938778e-01
    ## stdErr 0.001157272 0.1890277     0    0 2.613288e-04    0 7.255617e-10
    ##                BIC    nullBIC      deltaBIC  linkFun errFun changeFun converged
    ## mean   -229.383415 -83.310998 -1.460724e+02 identity    ols      expo         1
    ## stdErr    3.387974   3.387976  3.555373e-06 identity    ols      expo         0
    ##        pValSpearmanChange
    ## mean                    0
    ## stdErr                  0
    ## 
    ## 
    ## >> Max runs: 500  -- Tolerance: 0.05 
    ## 
    ## >> Parameter Pearson product-moment correlations:

    ##         pAsym pStart  pRate
    ## pAsym   1.000  1.000 -0.258
    ## pStart  1.000  1.000 -0.258
    ## pRate  -0.258 -0.258  1.000
