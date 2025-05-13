
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src='https://raw.githubusercontent.com/cefet-rj-dal/heimdall/master/inst/logo.png' alt='Logo do pacote heimdall' align='centre' height='150' width='129'/> Heimdall

<!-- badges: start -->

![GitHub
Stars](https://img.shields.io/github/stars/cefet-rj-dal/heimdall?logo=Github)
![CRAN Downloads](https://cranlogs.r-pkg.org/badges/heimdall)
<!-- badges: end -->

Heimdall is a tool designed for concept drift detection in continuous
data streams. When analyzing time series data, it is possible to observe
significant changes in the data distribution or in the accuracy of
predictive models during the forecasting process. These changes are
known as concept drift.

The goal of the Heimdall package is to detect when concept drift occurs,
enabling the detection and adaptation of models in non-stationary
contexts. The package offers several state-of-the-art methods to
identify and handle concept drift, as well as techniques for efficient
model adaptation.

------------------------------------------------------------------------

## Installation

The latest version of Heimdall is available on CRAN:

``` r
install.packages("heimdall")
```

You can install the development version directly from GitHub:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("cefet-rj-dal/heimdall", force = TRUE, upgrade = "never")
```

------------------------------------------------------------------------

## Exemplos

Heimdall usage examples are organized according to drift detection:

- [Examples](https://github.com/cefet-rj-dal/heimdall/tree/main/examples)

``` r
library(heimdall)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
#> Registered S3 methods overwritten by 'forecast':
#>   method  from 
#>   head.ts stats
#>   tail.ts stats
#> 
#> Attaching package: 'heimdall'
#> The following object is masked from 'package:base':
#> 
#>     norm
```

------------------------------------------------------------------------

## Bugs and Feature Requests

If you encounter any issues or would like to suggest new features,
please open an issue:

- [GitHub Issues](https://github.com/cefet-rj-dal/heimdall/issues)
