# Load Heimdall and the synthetic univariate stream.
library(heimdall)

# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)

# Load the numeric stream used by the detector.
data(st_drift_examples)
serie <- st_drift_examples$univariate

# Plot the monitored variable before detection.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Instantiate KSWIN with explicit window parameters.
model <- dfr_kswin(target_feat='serie', window_size=100, stat_size=50)

# Update the detector sequentially and store the alarm positions.
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

# Print the points where KSWIN raised drift alarms.
detection[detection$type == 'drift',]

# Overlay the detected drifts on the original stream.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
