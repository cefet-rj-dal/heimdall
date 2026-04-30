# Load Heimdall and the synthetic example stream.
library(heimdall)

# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)

# Load the stream and derive the binary monitored signal.
data(st_drift_examples)
data <- st_drift_examples$univariate
data$prediction <- st_drift_examples$univariate$serie > 4

# Plot the binary stream that ECDD will monitor.
plot(x=seq_len(nrow(data)), y=data$prediction)

# Instantiate the ECDD detector.
model <- dfr_ecdd()

# Update the detector sequentially and store the detected alarms.
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

# Print the drift points signaled by ECDD.
detection[detection$type == 'drift',]

# Overlay the alarms on the original numeric signal.
plot(x=seq_len(nrow(data)), y=data$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
