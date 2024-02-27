---
layout: post
title: "Bayesian Deming regression - Small sample example"
date: 2024-02-25 18:40:00
---

Imagine a data set of only 7 pairs of observations[^dsource] for two methods *X*
and *Y*. Imagine that the target is to reject the null Hypothesis of
*slope = 1* and *intercept = 0*. The *rstan* sampling was performed with *set.seed(20240225)* on Debian Trixie with amd64 architecture.

[^dsource]: Courtesy of [SSSMT - Locarno](https://www.cpslocarno.ti.ch/index.php/home/sss/).



|   Method&emsp;   |   Sample 1&emsp;  |   Sample 2&emsp;  |   Sample 3&emsp;  |   Sample 4&emsp;  |   Sample 5&emsp;  |  Sample 6&emsp;   |  Sample 7   |
| :--------------: | :---------------: | :---------------: | :---------------: | :---------------: | :---------------: | :---------------: | :---------: |
|     X&emsp;      |     38.0&emsp;    |     39.8&emsp;    |     38.0&emsp;    |     26.9&emsp;    |     37.5&emsp;    |     33.2&emsp;    |     36.9    |
|     Y&emsp;      |     30.8&emsp;    |     33.7&emsp;    |     26.1&emsp;    |     21.5&emsp;    |     33.9&emsp;    |     26.9&emsp;    |     29.7    |


With Bayesian Deming regression paired with a MD test this is possible. The simulation is run with *df = 1* to provide maximal robustness.

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



|                 |       EST&emsp;   |         SE&emsp;  |        LCI&emsp;  |       UCI  |
|:----------------|----------------:  |-----------------: |-----------------: |----------: |
| Intercept&emsp; | \-7.393668&emsp;  | 10.8756262&emsp;  | \-35.350355&emsp; | 20.563020  |
| Slope&emsp;     |  1.016203&emsp;   |  0.3022048&emsp;  |   0.239361&emsp;  |  1.793046  |


The CI are slightly wider than with the Bayesian Deming method and also
a little shifted compared to the HDI intervals. This is not surprising
since slope and intercept are not normally distributed. HDI for the CI
can mitigate the *right side excess* previously reported.

Here below the table for the PBequi alternative results.



|                 |       EST&emsp;  |         SE&emsp; |         LCI&emsp;  |  UCI&emsp; |
|:----------------|-----------------:|-----------------:|-------------------:|-----------:|
| Intercept&emsp; | \-7.306061&emsp; | 111.345365&emsp; | \-293.528433&emsp; | 278.916312 |
| Slope&emsp;     |  1.030303&emsp;  |   3.017419&emsp; |   \-6.726219&emsp; |   8.786825 |



Apparently *PBequi* is not able to provide meaningful CIs for such a small
data set.


