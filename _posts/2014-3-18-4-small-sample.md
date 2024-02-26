---
layout: post
title: "Bayesian Deming regression - Small sample example"
date: 2024-02-25 18:40:00
---

Imagine a data set of only 7 pairs of observation for two methods *X*
and *Y* ( courtesy of SSSMT - Locarno ). Imagine that the target is to reject the null Hypothesis of
*slope = 1* and *intercept = 0*.


|   Meth.   |  Sample 1   |   Sample 2  |   Sample 3  |   Sample 4  |   Sample 5  |  Sample 6   |  Sample 7   |
| :-------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: |
|     X     |     38.0    |     39.8    |     38.0    |     26.9    |     37.5    |     33.2    |     36.9    |
|     Y     |     30.8    |     33.7    |     26.1    |     21.5    |     33.9    |     26.9    |     29.7    |



With Bayesian Deming regression paired with a MD test it is possible. The simulation is run with *df = 1* to provide maximal robustness.

![Small sample BE plot]({{ site.baseurl }}/images/small_size_example/unnamed-chunk-2-1.png)