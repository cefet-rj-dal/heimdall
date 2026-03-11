# Load Heimdall and the synthetic univariate stream.
library(heimdall)

# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)

# Load the numeric stream monitored by the detector.
data(st_drift_examples)
serie <- st_drift_examples$univariate

# Plot the series before starting drift detection.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Instantiate the Levene-based detector.
model <- dfr_lbdd(target_feat='serie', window_size=100)

# Update the detector over the stream and record each drift alarm.
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

# Print the positions where LBDD detected drift.
detection[detection$type == 'drift',]

# Overlay those drift points on the original numeric stream.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
