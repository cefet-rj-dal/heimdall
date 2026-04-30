# Load Heimdall and the built-in synthetic stream.
library(heimdall)

# Fix the seed so the results are reproducible.
seed <- 1
set.seed(seed)

# Load the stream and derive the binary monitored signal.
data(st_drift_examples)
data <- st_drift_examples$univariate
data$prediction <- st_drift_examples$univariate$serie > 4

# Visualize the binary stream that EDDM will monitor.
plot(x=seq_len(nrow(data)), y=data$prediction)

# Instantiate the EDDM detector.
model <- dfr_eddm()

# Process the stream sequentially and store each detected alarm.
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

# Print the drift alarms returned by EDDM.
detection[detection$type == 'drift',]

# Show those alarms on top of the original numeric series.
plot(x=seq_len(nrow(data)), y=data$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
