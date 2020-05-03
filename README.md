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
    ## >> Call: response~((pAsym) + ((pStart) - (pAsym)) * 2^((1 - trial_number)/(2^(pRate))))
    ## 
    ## >> Converged: TRUE 
    ## 
    ## >> Fit Values:
    ##        Estimate  Q025  Q975 pseudoSE
    ## pAsym     1.000 0.999 1.000    0.000
    ## pRate     2.733 2.659 2.811    0.039
    ## pStart    0.230 0.175 0.254    0.020
    ## 
    ## >> Goodness-of-fit:
    ##                err  nullErr nPars nObs      BIC  nullBIC    deltaBIC
    ## bernoulli 13.42328 16.83409     3   30 37.05016 37.06937 -0.01921487
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
    ## pAsym   1.000  0.110 -0.011  0.061
    ## pStart  0.110  1.000  0.586  0.290
    ## pRate  -0.011  0.586  1.000 -0.021
    ## err     0.061  0.290 -0.021  1.000

An example of fitting a given model to subsets of data (e.g., individual participants within a behavioral study).

    ## 
    ## Your rate is very close to the boundary. Consider penalizing the likelihood.. 
    ## Your rate is very close to the boundary. Consider penalizing the likelihood.. 
    ## Your rate is very close to the boundary. Consider penalizing the likelihood.. .

``` r
plot(mod)
```

![](README_files/figure-markdown_github/print_model_groups-1.png)

``` r
summary(mod)
```

    ## 
    ## >> Call: response ~ ((pAsym) + ((pStart) - (pAsym)) * 2^((1 - trial_number)/(2^(pRate))))
    ## 
    ## >> Overall effects:
    ##             pAsym     pStart      pRate
    ## mean   0.14920954 0.01639722 3.83349020
    ## stdErr 0.03935343 0.01059216 0.02425303
    ## 
    ##                 err    nullErr nPars nObs     Fval         Pval   Rsquared
    ## mean   3.005902e-04 0.03071614     3   30 1691.907 1.110223e-16 0.97598289
    ## stdErr 6.864833e-05 0.01187769     0    0  653.177 1.110223e-16 0.01661562
    ##                BIC    nullBIC  deltaBIC  linkFun errFun changeFun converged
    ## mean   -337.328495 -211.91820 -125.4103 identity    ols      expo         1
    ## stdErr    6.544946   14.35328   19.2603 identity    ols      expo         0
    ##        pValSpearmanChange
    ## mean                    0
    ## stdErr                  0
    ## 
    ## 
    ## >> Max runs: 200  -- Tolerance: 0.05 
    ## 
    ## >> Parameter Pearson product-moment correlations:

    ##         pAsym pStart  pRate
    ## pAsym   1.000  1.000 -0.759
    ## pStart  1.000  1.000 -0.766
    ## pRate  -0.759 -0.766  1.000

Additional principles guiding the development of **TEfits**:

-   Reliance only on base R (dependencies are few and optional: **psych**, **MASS**, **brms**)
-   Good things come to those who wait: Speed is nice, but robustness is better.
