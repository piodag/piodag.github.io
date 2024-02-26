---
layout: post
title: "Bayesian Deming regression - Small sample example"
date: 2024-02-25 18:40:00
---

Imagine a data set of only 7 pairs of observation for two methods *X*
and *Y*, courtesy of [SSSMT - Locarno](https://www.cpslocarno.ti.ch/index.php/home/sss/). Imagine that the target is to reject the null Hypothesis of
*slope = 1* and *intercept = 0*. The *rstan* sampling was performed with *set.seed(20240225)* on Debian Trixie with amd64 architecture.



|   Method   |  Sample 1   |   Sample 2  |   Sample 3  |   Sample 4  |   Sample 5  |  Sample 6   |  Sample 7   |
| :--------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: | :---------: |
|     X      |     38.0    |     39.8    |     38.0    |     26.9    |     37.5    |     33.2    |     36.9    |
|     Y      |     30.8    |     33.7    |     26.1    |     21.5    |     33.9    |     26.9    |     29.7    |

Table of the data set

With Bayesian Deming regression paired with a MD test it is possible. The simulation is run with *df = 1* to provide maximal robustness.

![Small sample BE plot]({{ site.baseurl }}/images/small_size_example/unnamed-chunk-2-1.png)

The classical CI approach has no hope, see the purple HDI-CI box. The data set is too small, even for the
Bayesian Deming regression. But the result of a Bayesian Deming regression can be tested with the Mahalanobis distance MD method. The power of the MD testing method is so much higher than it is still possible to reject the null hypothesis, even with such a reduced data set. The probability of the MD test (the Chi-sq. p-value with df=2 
is printed in the figure above) is extremely low and highly significant.

Here below the regression plot drawn with the *{rstanbdp}* package

![Small sample regression plot]({{ site.baseurl }}/images/small_size_example/unnamed-chunk-3-1.png)

For matter of comparison here the results of Deming and PBequi
analytical regressions obtained with the package *{mcr}*. With both
methods a MD approach via bootstrap is impossible because of the small
size of the sample.

Here below the table for the classical (frequentist) Deming results



|           |       EST |         SE |        LCI |       UCI |
|:----------|----------:|-----------:|-----------:|----------:|
| Intercept | \-7.393668 | 10.8756262 | \-35.350355 | 20.563020 |
| Slope     |  1.016203 |  0.3022048 |   0.239361 |  1.793046 |


The CI are slightly wider than with the Bayesian Deming method and also
a little shifted compared to the HDI intervals. This is not surprising
since slope and intercept are not normally distributed. HDI for the CI
can mitigate the *right side excess* previously reported.

Here below the table for the PBequi alternative results.



|           |       EST |         SE |         LCI |        UCI |
|:----------|----------:|-----------:|------------:|-----------:|
| Intercept | \-7.306061 | 111.345365 | \-293.528433 | 278.916312 |
| Slope     |  1.030303 |   3.017419 |   \-6.726219 |   8.786825 |



Apparently *PBequi* is not able to provide meaningful CI for such a small
data set.


