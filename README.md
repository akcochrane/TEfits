<!-- README.md is generated from README.Rmd. Please edit that file -->
TEfits
======

[![Build Status](https://travis-ci.com/akcochrane/TEfits.svg?branch=master)](https://travis-ci.com/akcochrane/TEfits)

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
    ## >> Call: response~((pAsym) + ((pStart) - (pAsym)) * 2^((1 - trial_number)/(2^(pRate))))
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
    ## >> Call: response~((pAsym) + ((pStart) - (pAsym)) * 2^((1 - trial_number)/(2^(pRate))))
    ## 
    ## >> Converged: TRUE 
    ## 
    ## >> Fit Values:
    ##        Estimate  Q025  Q975 pseudoSE
    ## pAsym     1.000 0.999 1.000    0.000
    ## pRate     2.740 2.660 2.810    0.038
    ## pStart    0.231 0.194 0.261    0.017
    ## 
    ## >> Goodness-of-fit:
    ##                err  nullErr nPars nObs      BIC  nullBIC    deltaBIC
    ## bernoulli 13.42306 16.83409     3   30 37.04971 37.06937 -0.01965737
    ## 
    ## >> Test of change in nonindependence:
    ##                          rawSpearman modelConditionalSpearman
    ## response ~ trial_number:          -1              -0.03581758
    ##                          proportionalSpearmanChange pValSpearmanChange
    ## response ~ trial_number:                 0.03581758                  0
    ##                          pval_KPSS_null pval_KPSS_model
    ## response ~ trial_number:          < .01            > .1
    ## 
    ## >> Percent of resamples predicting an increase in values: 100 
    ## 
    ## >> Timepoint at which resampled estimates diverge from timepoint 1, with Cohen's d>1: 2 
    ## 
    ## >> Bootstrapped parameter correlations:
    ##         pAsym pStart pRate    err
    ## pAsym   1.000 -0.171 0.136 -0.315
    ## pStart -0.171  1.000 0.406  0.702
    ## pRate   0.136  0.406 1.000  0.052
    ## err    -0.315  0.702 0.052  1.000

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
    ## >> Call: response ~ ((pAsym) + ((pStart) - (pAsym)) * 2^((1 - trial_number)/(2^(pRate))))
    ## 
    ## >> Overall effects:
    ##             pAsym     pStart      pRate
    ## mean   0.14925356 0.01634720 3.83357741
    ## stdErr 0.03931318 0.01061323 0.02429374
    ## 
    ##                 err    nullErr nPars nObs      Fval         Pval   Rsquared
    ## mean   3.005669e-04 0.03071614     3   30 1692.2688 1.110223e-16 0.97598393
    ## stdErr 6.865735e-05 0.01187769     0    0  653.3993 1.110223e-16 0.01661518
    ##                BIC    nullBIC   deltaBIC  linkFun errFun changeFun converged
    ## mean   -337.332229 -211.91820 -125.41403 identity    ols      expo         1
    ## stdErr    6.547467   14.35328   19.26204 identity    ols      expo         0
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
    ## pStart  1.000  1.000 -0.763
    ## pRate  -0.756 -0.763  1.000

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
|         3.516|         -2.651|       2.856|

TEfit parameter estimates:

|          |  pAsym|  pStart|  pRate|
|----------|------:|-------:|------:|
| Estimate |  3.522|   0.869|  2.866|

Note that `TEfit` provides start and asymptote parameters directly, while `TElm` provides start as an offset from asymptote (ie., `Intercept`).
