<!-- README.md is generated from README.Rmd. Please edit that file -->
TEfits
======

[![DOI](https://zenodo.org/badge/225967950.svg)](https://zenodo.org/badge/latestdoi/225967950) [![Build Status](https://travis-ci.com/akcochrane/TEfits.svg?branch=master)](https://travis-ci.com/akcochrane/TEfits)

Overview to Time-Evolving fits
------------------------------

Behavioral data is described, interpreted, and tested using indices such as d prime, mean, or psychometric function threshold. The **TEfits** package serves to allow the same questions to be asked about time-evolving aspects of these indices, such as the starting level, the amount of time that the index takes to change, and the asymptotic level of that index. Nonlinear regression applied to time-evolving functions is made as intuitive and painless as is feasible, with many extensions if desired.

The **TEfits** package has a heavy emphasis on interpretability of parameters. As far as possible, parameters fit by **TEfits** are meant to reflect human-interpretable representations of time-evolving processes. Error functions, nonlinear ("change") functions linking predicted values to parameters and time, parameter and prediction boundaries, and goodness-of-fit indices are intended to be clear and adjustable. An equal emphasis is on ease of use: minimal arguments are necessary to begin using the primary function, `TEfit()`, and many common tasks are fully automated (e.g., optimization starting points, bootstrapping).

Simple model of exponential change
----------------------------------

A basic model nonlinearly relating time to an outcome variable.

``` r
# generate artificial data:
dat_simple <- data.frame(response=log(2:31),trial_number=1:30)

# fit a `TEfit` model
mod_simple <- TEfit(dat_simple[,c('response','trial_number')])

plot(mod_simple,plot_title='Time-evolving fit of artificial data')
```

![](README_files/figure-markdown_github/simple_model-1.png)

``` r
summary(mod_simple)
```

    ## 
    ## >> Formula: response~((pAsym) + ((pStart) - (pAsym)) * 2^((1 - trial_number)/(2^(pRate))))
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
    ##          err nullErr nPars nObs     Fval Pval  Rsquared       BIC   nullBIC
    ## ols 0.093557 15.2815     3   30 2191.575    0 0.9938778 -162.9079 -16.83544
    ##      deltaBIC
    ## ols -146.0724
    ## 
    ## >> Test of change in nonindependence:
    ##                          rawSpearman modelConditionalSpearman
    ## response ~ trial_number:          -1               0.03537264
    ##                          proportionalSpearmanChange pValSpearmanChange
    ## response ~ trial_number:                 0.03537264                  0
    ##                          pval_KPSS_null pval_KPSS_model
    ## response ~ trial_number:          < .01            > .1

Bootstrapped model with Bernoulli error function
------------------------------------------------

An example of a learning fit using a Bernoulli response distribution, with 40 bootstrapped fits.

``` r
# generate artificial data:
dat <- data.frame(response=log(2:31)/log(32),trial_number=1:30)

# fit a `TEfit` model
mod_boot <- TEfit(dat[,c('response','trial_number')], 
             errFun='bernoulli',
             bootPars=list(nBoots=40))
plot(mod_boot,plot_title='Time-evolving fit of artificial data with 95% CI from 40 bootstrapped fits')
```

![](README_files/figure-markdown_github/model_boot-1.png)

``` r
summary(mod_boot)
```

    ## 
    ## >> Formula: response~((pAsym) + ((pStart) - (pAsym)) * 2^((1 - trial_number)/(2^(pRate))))
    ## 
    ## >> Converged: TRUE 
    ## 
    ## >> Fit Values:
    ##        Estimate  Q025  Q975 pseudoSE
    ## pAsym     0.999 0.999 1.000    0.000
    ## pRate     2.735 2.672 2.801    0.033
    ## pStart    0.231 0.210 0.257    0.012
    ## 
    ## >> Goodness-of-fit:
    ##                err  nullErr nPars nObs      BIC  nullBIC    deltaBIC
    ## bernoulli 13.42358 16.83409     3   30 37.05076 37.06937 -0.01861105
    ## 
    ## >> Test of change in nonindependence:
    ##                          rawSpearman modelConditionalSpearman
    ## response ~ trial_number:          -1              -0.04605117
    ##                          proportionalSpearmanChange pValSpearmanChange
    ## response ~ trial_number:                 0.04605117                  0
    ##                          pval_KPSS_null pval_KPSS_model
    ## response ~ trial_number:          < .01            > .1
    ## 
    ## >> Percent of resamples predicting an increase in values: 100 
    ## 
    ## >> Timepoint at which resampled estimates diverge from timepoint 1, with Cohen's d>1: 2 
    ## 
    ## >> Bootstrapped parameter correlations:
    ##         pAsym pStart  pRate    err
    ## pAsym   1.000  0.009  0.213 -0.060
    ## pStart  0.009  1.000  0.477  0.510
    ## pRate   0.213  0.477  1.000 -0.022
    ## err    -0.060  0.510 -0.022  1.000

Fitting multiple models
-----------------------

An example of fitting a given model to subsets of data (e.g., individual participants within a behavioral study).

``` r
# generate artificial data:
dat <- data.frame(response=rep(dat$response,4)*seq(0,.2,length=120),trial_number=rep(1:30,4),group=rep(letters[1:4],each=30))

# fit a `TEfitAll` model
mod_4group <- TEfitAll(dat[,c('response','trial_number')], 
             groupingVar = dat$group,
             groupingVarName = 'Participant')
```

    ## 
    ## Your rate is very close to the boundary. Consider penalizing the likelihood.. 
    ## Your rate is very close to the boundary. Consider penalizing the likelihood.. 
    ## Your rate is very close to the boundary. Consider penalizing the likelihood.. .

Note the warnings regarding rate parameters; identifiability is a major concern in nonlinear models, and `TEfits` attempts to notify the user of potentially problematic situations.

``` r
plot(mod_4group)
```

![](README_files/figure-markdown_github/plot_model_groups-1.png)

``` r
summary(mod_4group)
```

    ## 
    ## >> Formula: response ~ ((pAsym) + ((pStart) - (pAsym)) * 2^((1 - trial_number)/(2^(pRate))))
    ## 
    ## >> Overall effects:
    ##             pAsym     pStart      pRate
    ## mean   0.14923089 0.01639689 3.83340520
    ## stdErr 0.03934536 0.01058507 0.02423948
    ## 
    ##                 err    nullErr nPars nObs      Fval         Pval  Rsquared
    ## mean   3.006328e-04 0.03071614     3   30 1692.0117 1.110223e-16 0.9759788
    ## stdErr 6.865709e-05 0.01187769     0    0  653.4459 1.110223e-16 0.0166171
    ##                BIC    nullBIC   deltaBIC  linkFun errFun changeFun converged
    ## mean   -337.324861 -211.91820 -125.40666 identity    ols      expo         1
    ## stdErr    6.546535   14.35328   19.26293 identity    ols      expo         0
    ##        pValSpearmanChange
    ## mean                    0
    ## stdErr                  0
    ## 
    ## 
    ## >> Max runs: 200  -- Tolerance: 0.05 
    ## 
    ## >> Parameter Pearson product-moment correlations:

    ##         pAsym pStart  pRate
    ## pAsym   1.000  1.000 -0.756
    ## pStart  1.000  1.000 -0.764
    ## pRate  -0.756 -0.764  1.000

Using a more common linear regression framework
-----------------------------------------------

In some cases (such as `mod_simple` above), similar performance can be attained using a nonlinear transformation of time as a predictor in a linear model. This method is plotted in green on top of the `mod_simple` results, with clearly near-identical fits.

``` r
# Fit a `lm` model, first computing the best nonlinear transformation for time:
mod_lm <- TElm(response~trial_number,dat_simple,timeVar = 'trial_number')

plot(mod_simple)

lines(dat_simple$trial_number,fitted(mod_lm),col='green',lty=2,lwd=2)
```

![](README_files/figure-markdown_github/TElm-1.png)

TElm parameter estimates:

|  X.Intercept.|  trial\_number|  log2\_rate|
|-------------:|--------------:|-----------:|
|         3.531|         -2.657|       2.883|

TEfit parameter estimates:

|          |  pAsym|  pStart|  pRate|
|----------|------:|-------:|------:|
| Estimate |  3.522|   0.869|  2.866|

Note that `TEfit` provides start and asymptote parameters directly, while `TElm` provides start as an offset from asymptote (ie., `Intercept`).
