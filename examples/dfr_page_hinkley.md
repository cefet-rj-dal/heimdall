# Page Hinkley Drifter Example

This example uses a distribuition-based drift detector with a synthetic variable.


``` r
library(daltoolbox)
library(heimdall)
seed <- 1
set.seed(seed)
```

## Load Data


``` r
data(st_drift_examples)
data <- st_drift_examples$univariate
data$event <- NULL
data$prediction <- st_drift_examples$univariate$serie > 4
```

### Plot Serie


``` r
plot(x=1:length(data$serie), y=data$serie)
```

![plot of chunk unnamed-chunk-3](fig/dfr_page_hinkley/unnamed-chunk-3-1.png)

### Instantiate Model


``` r
model <- dfr_page_hinkley(target_feat='serie')
```

## Detection


``` r
detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in 1:length(data$prediction)){
 output <- update_state(output$obj, data$prediction[i])
 if (output$drift){
   type <- 'drift'
   output$obj <- reset_state(output$obj)
 }else{
   type <- ''
 }
 detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
}
```

## Plot Drifts


``` r
detection[detection$type == 'drift',]
```

```
##     idx event  type
## 258 258  TRUE drift
```


``` r
plot(x=1:length(data$serie), y=data$serie)
for(drift_index in detection[detection$type == 'drift', 'idx']){
  abline(v=drift_index, col='red', lty=2)
}
```

![plot of chunk unnamed-chunk-7](fig/dfr_page_hinkley/unnamed-chunk-7-1.png)
