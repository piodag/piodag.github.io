---
layout: post
title: "Bayesian Deming regression - Small sample example"
date: 2024-02-25 19:00:00
---

Imagine a data set of only 7 pairs of observation for two methods ( *X*
and *Y* ). Imagine that the target is to reject the null Hypothesis of
*slope = 1* and *intercept = 0*.


|     |      |      |      |      |      |      |      |
|:----|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
| X   | 38.0 | 39.8 | 38.0 | 26.9 | 37.5 | 33.2 | 36.9 |
| Y   | 30.8 | 33.7 | 26.1 | 21.5 | 33.9 | 26.9 | 29.7 |


With Bayesian Deming regression and the MD test it is possible. The
simulation is run with *df = 1* to provide maximal robustness.

![bdpPlotBE]({{ site.baseurl }}/images/small_size_example/unnamed-chunk-2-1.png)