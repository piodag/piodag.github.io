---
layout: post
title: "Right side effect and bias in MethComp at 2x2 digit precision"
---

![Second simulation one]({{ site.baseurl }}/images/2024-03-10-right-side-effect/run2im1-1.png)

A new experiment (at 2x2 digit precision) was carried out to reproduce
and confirm the bias of the various Passing Bablok methods. It was
chosen to increase the resolution in the tails: one point for every
0.002 units in the interval \[0.9;1.1\] for a total of 109 points. Each
point consists of aproportion over 1000 regressions. The Deming -
jackknife method was added as additional reference to the list.

The Deming bootstrap BCa method is again shown to be particularly
symmetric. The Deming analytic and jackknife methods are very similar to
each other, with a slight **right-side effect (r.s.e)**. The power
curves of the three Deming methods look anyway very similar. A very
light asymmetry can be understood as the effect of the fact that the
slopes distribution is not perfectly normal. Only testing for the angles
(tangent methods) should provide perfectly symmetric power curves.
Anyway each model is tested with the very same data set. Thus the
comparison between methods is definitely meaningful, and Deming methods
are a good standard to start with.

Unfortunately, the asymmetry of the Passing Bablok methods is confirmed.
The classical algorithm is completely unacceptable. For the two PBequi
variants the situation is much better, but cannot be called good. In
fact, the r.s.e stands at 3.8% for the methodlarge=F variant and 4.8%
for the default variant. Basically, there is a bias and an increased
probability of not rejecting slopes greater than 1 compared to slopes
less than 1.


|                              | r. s.  vol. ratio |  r.s.e %   |
|:-----------------------------|:-----------------:|:----------:|
| Deming - anal.               |     0.5076242     | 1.5248311  |
| Deming - jack.               |     0.5072365     | 1.4473065  |
| Deming - boot BCa            |     0.4991186     | -0.1762712 |
| PBequi - anal.               |     0.5239478     | 4.7895647  |
| PBequi methodlarge=F - anal. |     0.5190940     | 3.8188037  |
| PaBa - anal.                 |     0.7123302     | 42.4660319 |
| MDeming - jack.              |     0.5094370     | 1.8874076  |
| MMDeming - jack.             |     0.5138581     | 2.7716215  |
| Deming - JE MD 1%            |     0.5078349     | 1.5669720  |

Table of r.s.e.


