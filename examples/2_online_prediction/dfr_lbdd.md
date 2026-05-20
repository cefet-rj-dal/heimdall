---
title: "dfr_lbdd Example"
output:
  html_document: 
    self_contained: true
---
# LBDD Example

LBDD compares the variability of historical and recent windows using Levene's test. It is useful when the most relevant change is not the average level of the signal, but its dispersion.

In this example, LBDD is used for **virtual concept drift** detection.

Reference: Giusti, L., Carvalho, L., Gomes, A. T., Coutinho, R., Soares, J., and Ogasawara, E. (2021). *Analysing flight delay under concept drift*. Evolving Systems. <doi:10.1007/s12530-021-09415-z>

## Learning goal

This example is a compact template for online prediction with distribution-based dispersion drift monitoring with Heimdall.



``` r
# Load Heimdall and the built-in synthetic data stream.
library(heimdall)
```



``` r
# Fix the seed to make the example reproducible.
seed <- 1
set.seed(seed)
```



``` r
# Load the univariate stream and derive the binary monitored signal.
data(st_drift_examples)
df <- st_drift_examples$uv_vdp_real_drift
```



``` r
# Plot the binary stream monitored by ADWIN.
plot(x=seq_len(nrow(df)), y=df$serie, col=df$target)
```

![plot of chunk unnamed-chunk-4](fig/dfr_lbdd/unnamed-chunk-4-1.png)


``` r
# Define features
features <- c('serie')

## Force Target as Factor
target <- 'target'
df[paste0(target)] <- factor(df[[target]])
slevels <- levels(df[[target]])

# Objects used to aggregate explainability data
results <- c()
norm_df <- c()
drift_input_df <- c()
hist_proj <- c()
recent_proj <- c()

# Model threshold
th <- 0.5

# Define batches
batch_size <- 50
batch_index <- c()
for(c in (1:(as.integer(nrow(df)/batch_size)))){
  batch_index <- cbind(batch_index, as.vector(rep(c, batch_size)))
}
df <- head(df, length(as.vector(batch_index)))
df['batch_index'] <- as.vector(batch_index)
ordered_batches <- sort(unique(df$batch_index))
old_start_batch <- ordered_batches[1]
# Simulation parameters
warmup_size <- batch_size * 2 # 
monitoring_step <- batch_size # Process data in batch_size frequency
# Define the stealthy model
model <- stealthy(
  model=daltoolbox::cla_dtree(target, slevels), # Naive Bayes Classifier
  drift_method=dfr_lbdd(target_feat='serie', alpha=0.05, window_size=100), # Using LBDD
  norm_class=nrm_memory(norm_class = daltoolbox::zscore()), # Normalization with Z-Score
  warmup_size=warmup_size, # Warmup size used to train models
  incremental_memory=FALSE, # Do not force retrain in each batch
  class_balance='inactive', # No class balancing
  target_uni_drifter=FALSE, # Uses each features individually
  active_warmup = FALSE, # Only trains when warmup_size rows are available
  verbose=TRUE # Shows status messages
)
```



``` r
 # Simulation
start_time <- Sys.time()
for (batch in ordered_batches[2:length(ordered_batches)]){
  start_batch_time <- Sys.time()
  
  print(paste0('Time Step:', batch))
  
  # Define batches
  new_batch <- df[df$batch_index == batch,]
  last_batch <- df[(df$batch_index < batch) & (df$batch_index >= old_start_batch),]
  old_start_batch <- batch
  
  # Define Training Data
  x_train <- last_batch[, features, drop=FALSE]
  y_train <- last_batch[, target, drop=FALSE]
  
  # Define Test Data
  x_test <- new_batch[, features, drop=FALSE]
  y_test <- new_batch[, target]
  
  model <- daltoolbox::fit(model, x_train, y_train)
  
  # If there is a model, predict and calculate metrics
  if(model$fitted){
    test_predictions <- predict(model, x_test)
    # If only one class
    if(length(names(test_predictions)) == 1){
      if('TRUE' %in% names(test_predictions)){
        y_pred <- factor(test_predictions[, 1] >= th)
      }else if('FALSE' %in% names(test_predictions)){
        y_pred <- factor(test_predictions[, 1] < th)
      }
    # If two classes
    }else{
      y_pred <- factor(test_predictions[, 2] >= th)
    }
    
    levels(y_pred) <- slevels
    
    # Metrics calculation
    y_pred <- as.numeric(y_pred)
    y_test <- as.numeric(y_test)

    true_positives <- sum((y_pred == 2) & (y_test == 2))
    false_positives <- sum((y_pred == 2) & (y_test == 1))
    true_negatives <- sum((y_pred == 1) & (y_test == 1))
    false_negatives <- sum((y_pred == 1) & (y_test == 2))
  }else{
    true_positives <- 0
    false_positives <- 0
    true_negatives <- 0
    false_negatives <- 0
  }
  
  elap_batch_time <- Sys.time() - start_batch_time
  
  # Aggregate results
  results <- rbind(results, 
                   c(
                     batch,
                     true_positives,
                     false_positives,
                     true_negatives,
                     false_negatives,
                     model$drifted,
                     elap_batch_time
                   )
  )
  
}
```

```
## [1] "Time Step:2"
## [1] "Time Step:3"
## [1] "Time Step:4"
## [1] "Time Step:5"
## [1] "Time Step:6"
## [1] "Time Step:7"
```

```
## Stealthy detected a drift, discarding old data
```

```
## [1] "Time Step:8"
## [1] "Time Step:9"
## [1] "Time Step:10"
```

``` r
results<- as.data.frame(results)
names(results) <- c('Batch Index', 'tp', 'fp', 'tn', 'fn', 'drifted', 'elap')
```



``` r
# Print the positions where DDM detected drift.
results
```

```
##   Batch Index tp fp tn fn drifted         elap
## 1           2  0  0  0  0       0 0.0007345676
## 2           3 17  0 33  0       0 0.0083389282
## 3           4 23  0 27  0       0 0.1025068760
## 4           5 23  1 26  0       0 0.1001939774
## 5           6  0 26  0 24       0 0.0843198299
## 6           7  0  0  0  0       1 0.0896637440
## 7           8 31  0 19  0       0 0.0073482990
## 8           9 26  0 24  0       0 0.0899446011
## 9          10 27  0 23  0       0 0.0963771343
```


``` r
# Calculate metrics
beta <- 1
tp_sum <- sum(results['tp'])
fp_sum <- sum(results['fp'])
tn_sum <- sum(results['tn'])
fn_sum <- sum(results['fn'])
precision <- tp_sum/(tp_sum + fp_sum)
recall <- tp_sum/(tp_sum + fn_sum)
accuracy <- (tp_sum + tn_sum)/(tp_sum + fp_sum + tn_sum + fn_sum)
f1 <- 2 * (precision * recall) / (((beta^2)*precision) + recall)

print(paste('Precision', precision, 'Recall', recall, 'F1', f1, 'Accuracy', accuracy))
```

```
## [1] "Precision 0.844827586206897 Recall 0.859649122807018 F1 0.852173913043478 Accuracy 0.854285714285714"
```

``` r
plot(x=seq_len(nrow(df)), y=df$serie, col=as.numeric(df[[target]]))
abline(v=250, col='blue', lty=2)
for (drift_index in results[results['drifted'] == 1, 'Batch Index']) {
  abline(v=drift_index*batch_size, col='red', lty=2)
}
```

![plot of chunk unnamed-chunk-9](fig/dfr_lbdd/unnamed-chunk-9-1.png)
