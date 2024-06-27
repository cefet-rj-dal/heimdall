
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src='https://raw.githubusercontent.com/cefet-rj-dal/heimdall/master/inst/logo.png' align='centre' height='150' width='129'/> heimdall

<!-- badges: start -->

![GitHub Repo
stars](https://img.shields.io/github/stars/cefet-rj-dal/heimdall?logo=Github)
![GitHub Repo stars](https://cranlogs.r-pkg.org/badges/heimdall)
<!-- badges: end -->

## Heimdall

By analyzing streaming datasets, it is possible to observe significant
changes in the data distribution or models’ accuracy during their
prediction (concept drift). The goal of Heimdall is to measure when
concept drift occurs. The package makes available several
state-of-the-art methods. It also tackles how to adapt models in a
nonstationary context.

## Installation

The latest version of heimdall at CRAN is available at:
<https://CRAN.R-project.org/package=heimdall>

You can install the stable version of heimdall from CRAN with:

``` r
install.packages("heimdall")
```

You can install the development version of heimdall from GitHub
<https://github.com/cefet-rj-dal/heimdall> with:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("cefet-rj-dal/heimdall", force=TRUE, upgrade="never")
```

## Examples

Examples of heimdall are organized according to drift detection.

General: <https://nbviewer.org/github/cefet-rj-dal/heimdall>

``` r
library(heimdall)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
## basic example code
```

## Bugs and new features request

<https://github.com/cefet-rj-dal/heimdall/issues>
