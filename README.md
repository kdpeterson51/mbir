[![version](http://www.r-pkg.org/badges/version/mbir)](https://cran.r-project.org/package=mbir)
[![CRAN](https://cranlogs.r-pkg.org/badges/mbir)](https://cran.r-project.org/web/packages/mbir/index.html)
[![Build Status](https://travis-ci.org/kdpeterson51/mbir.svg?branch=master)](https://travis-ci.org/kdpeterson51/mbir)

# mbir: Magnitude-Based Inferences
Kyle D Peterson and Aaron R Caldwell

# Overview
Allows practitioners and researchers a wholesale approach for deriving magnitude-based inferences from raw data. A major goal of `mbir` is to programmatically detect appropriate statistical tests to run in lieu of relying on practitioners to determine correct stepwise procedures independently.

# Installation
Assuming users have R downloaded and installed, `mbir` can be installed right from [CRAN](https://cran.r-project.org/web/packages/mbir/index.html) by typing: 

```
install.packages("mbir")
```
This package depends on the packages `graphics`, `stats`, and `utils`, which are imported upon installation.

# A brief walk through `mbir`
Below is an example of how a user would perform a *t*-test with the classic Sleep data set.
```
data('sleep')
mbir::smd_test(sleep$extra[sleep$group==1],sleep$extra[sleep$group==2],paired = F)
```
By default, `smd_test` function tested for normality and homogeneity prior to performing the respectgive *t*-test. The output first states the results of the preliminary assumption tests, followed by the *t*-test parameters, which are then used to calculate the appropriate effect size estimate (in this case Cohen's *d*). Lastly, `smd_test` prints the magnitude-based inference about the effect size estimate by providing the partitioned probabilities and associated qualitative label. 

Below is an example of how a user would perform a correlation with the classic mtcars data set, which provide the same theme as above.
```
data('mtcars')
mbir::corr_test(mtcars$mpg,mtcars$qsec)
```
For a detailed exporlation of `mbir`, please visit our [vignette](http://www.mbir-project.us/mbir.pdf).
# Feedback
Feedback from users is welcome, and would be sincerely appreciated, to help improve functionality of `mbir` where warranted. Please reach out to petersonkdon@gmail.com for support, reporting issues, or contributions. Thank you very much.
