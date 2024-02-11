---
layout: post
title: "Jackknife pairs collinearity for Passing Bablok methods"
---

![Jackknife collinearity]({{ site.baseurl }}/images/jack3x3summary.png)

Using 3 significant digits instead of two, it is spontaneous to try to reintroduce the jackknife method for PBequi. However, stability problems are again encountered in high-resolution experiments.

The jackknife pairs generated with Deming, MDeming and PaBa variants are observed in the graph. The Deming and MDeming methods show a distribution that still has a vaguely multivariate Gaussian appearance with a fair correlation. The nonparametric methods, on the other hand, show a very strong correlation between slope and intercept. The points are found almost on a straight line. One would think that in long experiments this correlation would sporadically lead to the generation of errors that block experimentation. This data set has been generated again with set.seed(202402) and the [published data generator function (chap 6.1.4)](https://arxiv.org/pdf/2105.04628.pdf). The plots are easily reproduced at will.

It was possible for now to perform one power experiment with n=250. A report will follow shortly.

