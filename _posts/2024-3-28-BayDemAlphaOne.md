---
layout: post
title: Bayesian Deming regression, Type I (alpha) error
date: 2024-03-28 13:30:00
---

![pplots]({{ site.baseurl }}/images/BayDemAlphaOne_files/figure-gfm/ppplots-1.png)

## *Mahalanobis* distance test

In order to apply the *Mahalanobis* distance (*MD*) method when carrying
out a *method comparison* (*bdpPlotBE()* in
[*{rstanbdp}*](https://cran.r-project.org/package=rstanbdp) *R*
package), it is necessary to set a probability threshold *P-value*.
Through simulation in [2021](https://arxiv.org/pdf/2105.04628.pdf), it
was reported that a *P \< 1%* level is a good choice, as *5%* is not
enough conservative. The same approach is now repeated here with the
*Bayesian Deming* regression. The experiment is performed with 2000
randomly generated data sets. For comparison, classical frequentist
*Deming* and *PaBa* were performed with *bootstrap / BCa CI* method to
obtain the necessary intercept and slope pairs.