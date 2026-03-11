
``` r
# Loading heimdall
library(heimdall)
```


``` r
# KLDIST example
# KLDIST is shown here as a virtual concept drift detector over a numeric stream.
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

![plot of chunk unnamed-chunk-4](fig/dfr_kldist/unnamed-chunk-4-1.png)


``` r
# Instantiate model

model <- dfr_kldist(target_feat='serie', window_size=100)
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

```
## Warning in update_state.dfr_kldist(output$obj, serie$serie[i]): NaNs produced
## Warning in update_state.dfr_kldist(output$obj, serie$serie[i]): NaNs produced
## Warning in update_state.dfr_kldist(output$obj, serie$serie[i]): NaNs produced
## Warning in update_state.dfr_kldist(output$obj, serie$serie[i]): NaNs produced
## Warning in update_state.dfr_kldist(output$obj, serie$serie[i]): NaNs produced
## Warning in update_state.dfr_kldist(output$obj, serie$serie[i]): NaNs produced
## Warning in update_state.dfr_kldist(output$obj, serie$serie[i]): NaNs produced
## Warning in update_state.dfr_kldist(output$obj, serie$serie[i]): NaNs produced
## Warning in update_state.dfr_kldist(output$obj, serie$serie[i]): NaNs produced
## Warning in update_state.dfr_kldist(output$obj, serie$serie[i]): NaNs produced
## Warning in update_state.dfr_kldist(output$obj, serie$serie[i]): NaNs produced
```


``` r
# Detected drifts

detection[detection$type == 'drift',]
```

```
##     idx event  type
## 105 105  TRUE drift
## 208 208  TRUE drift
## 384 384  TRUE drift
## 484 484  TRUE drift
```


``` r
# Plot drifts

plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
```

![plot of chunk unnamed-chunk-8](fig/dfr_kldist/unnamed-chunk-8-1.png)
