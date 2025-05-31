
``` r
# Installing heimdall
install.packages("heimdall")
```

```

```


``` r
# Loading heimdall
library(daltoolbox)
library(heimdall) 
```


``` r
# ADWIN Drifter Example
# This example uses an distribuition-based drift detector with a synthetic variable.
seed <- 1
set.seed(seed)
```


``` r
# Load Data

data(st_drift_examples)
serie <- st_drift_examples$univariate
```


``` r
# Plot Serie

plot(x=rownames(serie), y=serie[['serie']])
```

![plot of chunk unnamed-chunk-5](fig/dfr_adwin/unnamed-chunk-5-1.png)


``` r
# Instantiate Model

model <- dfr_adwin(target_feat='serie')
```


``` r
# Detection

detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in 1:length(serie$serie)){
 output <- update_state(output$obj, serie$serie[i])
 if (output$drift){
   type <- 'drift'
   output$obj <- reset_state(output$obj)
 }else{
   type <- ''
 }
 detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
}
```


``` r
# Plot Drifts

detection[detection$type == 'drift',]
```

```
##     idx event  type
## 224 224  TRUE drift
## 352 352  TRUE drift
## 448 448  TRUE drift
```


``` r
plot(x=rownames(serie), y=serie[['serie']])
for(drift_index in detection[detection$type == 'drift', 'idx']){
  abline(v=drift_index, col='red', lty=2)
}
```

![plot of chunk unnamed-chunk-9](fig/dfr_adwin/unnamed-chunk-9-1.png)
