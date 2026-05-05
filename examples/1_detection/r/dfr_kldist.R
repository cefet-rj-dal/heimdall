# Load Heimdall and the example data stream.
library(heimdall)

# Fix the seed to keep the example reproducible.
seed <- 1
set.seed(seed)

# Load the univariate stream monitored in this walkthrough.
data(st_drift_examples)
serie <- st_drift_examples$univariate

# Plot the monitored numeric signal before detection.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Instantiate the KL-divergence-based detector.
model <- dfr_kldist(target_feat='serie', window_size=100)

# Update the detector sequentially and store drift alarms.
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

# Print the points where KLDIST detected drift.
detection[detection$type == 'drift',]

# Overlay the detected drifts on the original numeric stream.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
