---
layout: post
title: "Deming vs Passing Bablok at 3 x 3 digits precision"
---

![Three digits one]({{ site.baseurl }}/images/threedigits/threedigits1-1.png)


Here the result of a successful simulation with 400 replicas per point at 3x3
digits precision. Classical PaBa still shows a pretty high asymmetry with
r.s.e of over 4%. PBequi is performing better, but shows some bias compared to
classical Deming methods with an r.s.e over 2%. Quite surprisingly PBequi with
jackknife CI performs better that the analytical version with 
approx. 1.36% r.s.e.



| Methods                       | r. s. vol. ratio  |   r.s.e%     |
| :---------------------------- | :---------------: | :----------: |
| Deming \- anal.               |     0.5075293     |  1.5058635   |
| Deming \- jack.               |     0.5059993     |  1.1998674   |
| Deming \- boot BCa            |    0.4998623     |  \-0.0275387 |
| PBequi \- anal.               |    0.5105528     |  2.1105656   |
| PBequi methodlarge=F \- anal. |    0.5112202     |  2.2440393   |
| PaBa \- anal.                 |    0.5220235     |  4.4047071   |
| PBequi \- jack.               |    0.5067896     |  1.3579233   |
| MDeming \- jack.              |    0.5075982     |  1.5196464   |
| MMDeming \- jack.             |    0.5113902     |  2.2780404   |
| Deming \- JE MD 1%            |    0.5030620     |  0.6123919   |

Table of r.s.e. at 3x3 digits precision 

Below is also reported a plot for the M- and MM-Deming methods. M-Deming is
very well performing, in line with other classical Deming regressions with
a r.s.e% of approx. 1.52%. MM-Deming is confirming its bias with an r.s.e%
of 2.28%.

Worth noting that the Deming JE/MD method has an r.s.e% of only 0.62%. This
method seems much more powerful that the traditional ones and also shows a
good symmetry.

![Three digits M-Deming]({{ site.baseurl }}/images/threedigits/threedigits2-1.png)