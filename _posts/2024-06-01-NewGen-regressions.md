---
layout: post
title: mcrPioda-1.3.3 release. Rewritten M- and MM-Deming regressions
date: 2024-06-01 18:40:00
---

I would like to report that the recently released
[mcrPioda-1.3.3](https://github.com/piodag/mcrPioda/releases/tag/1.3.3)
package has refurbished versions of the M-Deming and MM-Deming
regression. The new functions are written in C and are much faster that
the previous ones. The **new release is on the devel branch** because on
the master branch the legacy version of 2021 is still available.

In detail:

- M-Deming gives exact identical results as the previous one and is
  approximately 10 times faster. The converging algorithm has a new
  gradient descending mechanism that reduces resonance phenomena and
  leads to lower cycles count with the very same numeric result as the
  2021 version.

- MM-Deming regression with bisquare redescending weights is available
  in two new versions. Both have a new feature in the algorithm compared
  to the legacy MM-Deming published in 2021 (still available in the new
  package). In the new methods the **mad()** parameter for scaling the
  euclidean residuals gets estimated at **every cycle** instead as only
  once at the beginning of the iterative process. This leads to a much
  more stable algorithm. The previous iterative method was rather
  strongly dependent on the provided starting values, indicating that a
  plethora of local minima could be present. With the new algorithm the
  minimum is coherently found with almost no influence given by the
  starting values (slope and intercept) of the iterative process. For
  this reason two different versions have been proposed, both of them
  not anymore using the *covSest()* functions:

  - NgMMDeming has an initial step for the determination of the starting
    values. In this step the M-Deming is used twice. The first time just
    like in the M-Deming regression and in the second part only an
    adjustable fraction the points (‘bdPoint = 0.5’ as parameter
    default), those with highest M-weight, are used so as to obtain a
    robust estimate with an high breakdown point. These starting values
    are subsequently used to start the bisquare MM-Deming regression.

  - PiMMDeming has no starting values determination step. The starting
    slope and intercept are “user provided” in a Bayesian style. The
    default is slope = 1 and intercept = 0. The convergence of the
    bisquare weighted method is so strong that these values are in most
    cases a reasonable choice.

All these new methods are under scrutiny for power and power symmetry.
