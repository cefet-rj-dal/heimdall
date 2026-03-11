
``` r
# Loading heimdall
library(heimdall)
```


``` r
# LBDD example
# LBDD is shown here as a virtual concept drift detector over a numeric stream.
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

![plot of chunk unnamed-chunk-4](fig/dfr_lbdd/unnamed-chunk-4-1.png)


``` r
# Instantiate model

model <- dfr_lbdd(target_feat='serie', window_size=100)
```

```
## Error in `dfr_lbdd()`:
## ! could not find function "dfr_lbdd"
```


``` r
# Detection

detection <- NULL
output <- list(obj=model, drift=FALSE)
```

```
## Error:
## ! object 'model' not found
```

``` r
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
## Error:
## ! object 'output' not found
```


``` r
# Detected drifts

detection[detection$type == 'drift',]
```

```
## NULL
```


``` r
# Plot drifts

plot(x=seq_len(nrow(serie)), y=serie$serie)
```

![plot of chunk unnamed-chunk-8](fig/dfr_lbdd/unnamed-chunk-8-1.png)

``` r
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
```
