# Load Heimdall and the built-in synthetic data stream.
library(heimdall)

# Fix the seed to make the example reproducible.
seed <- 1
set.seed(seed)

# Load the univariate stream and derive the binary monitored signal.
data(st_drift_examples)
data <- st_drift_examples$univariate
data$prediction <- st_drift_examples$univariate$serie > 4

# Plot the binary stream monitored by DDM.
plot(x=seq_len(nrow(data)), y=data$prediction)

# Instantiate the DDM detector.
model <- dfr_ddm()

# Process the stream sequentially and store the drift alarms.
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

# Print the positions where DDM detected drift.
detection[detection$type == 'drift',]

# Show the detected drifts over the original numeric signal.
plot(x=seq_len(nrow(data)), y=data$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
