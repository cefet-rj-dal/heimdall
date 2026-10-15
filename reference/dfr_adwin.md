# ADWIN method

ADWIN (Adaptive Windowing) is a sequential change detector that
maintains a variable-length window and tests whether the means of two
subwindows differ significantly. In this package, the implementation is
primarily used for **virtual concept drift** when it monitors a numeric
feature stream, although the same mechanism can also detect **real
concept drift** if applied to an error or loss stream. The theoretical
basis follows Bifet and Gavalda (2007) <doi:10.1137/1.9781611972771.42>.

## Usage

``` r
dfr_adwin(target_feat = NULL, delta = 2e-05)
```

## Arguments

- target_feat:

  Feature to be monitored.

- delta:

  The significance parameter for the ADWIN algorithm.

## Value

`dfr_adwin` object

## Details

The implementation is written in pure R and follows the
exponential-histogram version of ADWIN distributed with
scikit-multiflow. The window is compressed into buckets of exponentially
growing size (at most five buckets per size), and the cut points are
checked every 32 observations, once the window holds more than 10
observations. All the state lives in the object itself, so a `dfr_adwin`
object can be serialized with
[`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html).

## References

Bifet, A., and Gavalda, R. (2007). Learning from time-changing data with
adaptive windowing. In *Proceedings of the 2007 SIAM International
Conference on Data Mining*, 443-448. <doi:10.1137/1.9781611972771.42>

## Examples

``` r
library(daltoolbox)
#> 
#> Attaching package: ‘daltoolbox’
#> The following object is masked from ‘package:base’:
#> 
#>     transform
library(heimdall)

data(st_drift_examples)
data <- st_drift_examples$univariate
data$event <- NULL

model <- dfr_adwin(target_feat='serie')

detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in seq_along(data$serie)){
 output <- update_state(output$obj, data$serie[i])
 if (output$drift){
   type <- 'drift'
   output$obj <- reset_state(output$obj)
 }else{
   type <- ''
 }
 detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
}

print(detection[detection$type == 'drift',])
#>     idx event  type
#> 224 224  TRUE drift
#> 352 352  TRUE drift
#> 448 448  TRUE drift
```
