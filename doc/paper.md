---
title: 'TEfits: Nonlinear regression for time-evolving indices'
authors:
- affiliation: 1
  name: Aaron Cochrane
  orcid: 0000-0001-6691-9149
tags:
- R
- Psychology
- Nonlinear regression
- Generalized nonlinear model
- Perceptul learning
affiliations:
- index: 1
  name: University of Wisconsin - Madison
output: html_document
date: 29 June 2020
bibliography: references.bib
---

# Summary

Within behavioral science, it is common for data to be paradigmatically collected through repeated measurement of behavior (e.g., on each of 400 trials a human presses one of two buttons to indicate which of two possible stimuli they saw). Typical analytic tools used alongside such designs, such as ANOVA, linear regression, or T tests, implicitly assume that the data arises from distributions that are stationary across the repeated individual measurements (i.e., that every trial is independently and identically sampled from the same distribution, or _iid_, conditional on experimentally manipulated or observed variables).  Interestingly, the use of such analytic tools is common even in those areas of behavioral science that are inherently concerned with time-evolving changes in behavior (e.g., learning, memory, priming, adaptation, vigilance, cognitive control). For instance, in learning research it is common for researchers to first divide the repeated measurements into temporal bins (e.g., trials 1-100; 101-200; 201-300; etc.). They then calculate means within those bins, before applying the analysis tools above. Such an analytic method is explicitly modelling a process where performance can change between, but not within, bins. That is, conditional stationarity is assumed within the temporal bins. Beyond these fields, research methods in many others (e.g., attention, development, neuroscience, perception) attempt to by-pass the problem of non-stationarity by utilizing practice trials prior to collecting behavioral data. Practice trials are intended to give participants enough practice with the task that they reach a stable level of performance. However, whether this assumption is true is rarely tested. Our previous work has demonstrated, across several different experimental contexts, that by-trial modeling of performance provides estimates of the full timecourse of behavioral change. In doing so, these models of nonlinear monotonic *trend* stationarity provide both better estimates of behavior, as well as allows for deeper inferences regarding the underlying processes at work, than statistical methods that assume that behavioral data remains conditionally stationary over the course of a set of measurements [@kattner_trial-dependent_2017].

`TEfits` is a `R` package for fitting and assessing time-evolving models to data common in behavioral science. `TEfits` is designed with behavioral science researchers with a range of interests and expertise in models of time-dependent changes in behavior. Although many excellent nonlinear regression methods exist in `R`, most notably using the powerful and flexible Bayesian package `brms` [@burkner_brms_2017], but also including functions such as `nls` from the core `R` `stats` package, these methods can be difficult to learn and integrate into the workflow of researchers not familiar with nonlinear regression. The user-oriented functions of `TEfits` are designed to be friendly to `R` users with minimal experience implementing nonlinear models. Extensions of this base functionality allow for simple use of various time-evolving indices (e.g., psychometric function threshold or d prime), objective functions, and/or functional forms of time-related change. Default constraints are applied to models for stability and reproducibility, but boundaries on parameters or predicted values are fully user-defineable. `TEfits` is designed to operate with minimal dependencies on other `R` packages. However, certain functions allow for optional simulation from model fits using `MASS` [@venables_ripley], fitting of hierarchical models using `lme4` [@bates_fitting_2015], or re-fitting a model using Bayesian methods using `brms` [@burkner_brms_2017].

`TEfits` is being actively used in learning and memory research, with several manuscripts in preparation or under review, as well as having results using `TEfits` presented at academic conferences. Primary areas of use to-date include assessments of the most appropriate learning functional form of visual perceptual improvements, testing for learning and generalization in the field of radiological diagnosis, and modeling rapid shifts of attentional control in response to environmental statistics. However, the use of `TEfits` could be appropriate in any domain where individuals repeatedly engage with the same task (i.e., most psychological tasks). The ease of use and wide applicability of `TEfits` models should remove barriers from many behavioral scientists' assessment of the assumption of stationarity. Instead of this assumption users are provded a framework for understanding the changes that occur in nonstationary (i.e., _iid_ conditioned on a time-evolving trend) distributions of behavioral data.

# Funding and Support

This work has been supported in part by US Office of Naval Research Grant ONR-N000141712049.

# References
