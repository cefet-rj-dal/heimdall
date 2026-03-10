
``` r
# Installing heimdall
install.packages("heimdall")
```

```
## Installing package into '/home/gpca/R/x86_64-pc-linux-gnu-library/4.5'
## (as 'lib' is unspecified)
```


``` r
# Loading heimdall
library(daltoolbox)
```

```
## 
## Attaching package: 'daltoolbox'
```

```
## The following object is masked from 'package:base':
## 
##     transform
```

``` r
library(heimdall)
```


``` r
# ADWIN example
# ADWIN is shown here as a virtual concept drift detector over a numeric stream.
seed <- 1
set.seed(seed)
```


``` r
# Load data

data(st_drift_examples)
serie <- st_drift_examples$univariate
```


``` r
# Plot series

plot(x=seq_len(nrow(serie)), y=serie$serie)
```

![plot of chunk unnamed-chunk-5](fig/dfr_adwin/unnamed-chunk-5-1.png)


``` r
# Instantiate model

model <- dfr_adwin(target_feat='serie')
```


``` r
# Detection

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
# Detected drifts

detection[detection$type == 'drift',]
```

```
##     idx event  type
## 224 224  TRUE drift
## 352 352  TRUE drift
## 448 448  TRUE drift
```


``` r
# Plot drifts

plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
```

![plot of chunk unnamed-chunk-9](fig/dfr_adwin/unnamed-chunk-9-1.png)
