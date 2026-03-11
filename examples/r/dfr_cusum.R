# Load Heimdall and the synthetic benchmark stream.
library(heimdall)

# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)

# Load the stream and derive a simple binary error-like signal.
data(st_drift_examples)
data <- st_drift_examples$univariate
data$prediction <- st_drift_examples$univariate$serie > 4

# Plot the binary monitored stream used by CUSUM.
plot(x=seq_len(nrow(data)), y=data$prediction)

# Instantiate the CUSUM detector.
model <- dfr_cusum()

# Update the detector sequentially over the binary stream.
detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in seq_len(nrow(data))){
  output <- update_state(output$obj, data$prediction[i])
  if (output$drift){
    type <- 'drift'
    output$obj <- reset_state(output$obj)
  } else {
    type <- ''
  }
  detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
}

# Print the drift alarms produced by the detector.
detection[detection$type == 'drift',]

# Map the drift alarms back onto the original numeric series.
plot(x=seq_len(nrow(data)), y=data$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
