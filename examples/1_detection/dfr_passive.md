---
title: "dfr_passive Example"
output:
  html_document:
    self_contained: true
---
# Passive Baseline Example

`dfr_passive()` is a dummy detector that reports a drift on every observation. It does not monitor anything; it exists as an upper baseline for adaptation: a model wrapped with it is retrained on every batch, which is what a passive (blind) adaptation strategy does.

## Learning goal

This example shows that the passive baseline follows the same streaming loop as every other detector, and that it signals a drift at every step.



``` r
# Load Heimdall and the synthetic stream example.
library(heimdall)
```


``` r
# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)
```


``` r
# Load the univariate numeric stream used in this example.
data(st_drift_examples)
serie <- st_drift_examples$uv_virtual_drift
```


``` r
# Plot the stream; it has four known change points.
plot(x=seq_len(nrow(serie)), y=serie$serie)
```

![plot of chunk unnamed-chunk-4](fig/dfr_passive/unnamed-chunk-4-1.png)


``` r
# Instantiate the passive baseline.
model <- dfr_passive()
```


``` r
# Run the usual streaming loop.
detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in seq_len(nrow(serie))){
  output <- update_state(output$obj, serie$serie[i])
  if (output$drift){
    type <- 'drift'
    output$obj <- reset_state(output$obj)
  } else {
    type <- ''
  }
  detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
}
```


``` r
# The baseline reports a drift on every observation.
sum(detection$event)
```

```
## [1] 500
```

``` r
nrow(serie)
```

```
## [1] 500
```


``` r
print(paste('Successfull run at', Sys.time()))
```

```
## [1] "Successfull run at 2026-10-16 10:25:29.596129"
```
