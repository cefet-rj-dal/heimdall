# Load Heimdall so the detector and built-in datasets are available.
library(heimdall)

# Fix the seed to keep the example reproducible.
seed <- 1
set.seed(seed)

# Load the synthetic univariate stream used in the walkthrough.
data(st_drift_examples)
serie <- st_drift_examples$univariate

# Plot the monitored variable before starting the sequential updates.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Instantiate ADWIN to monitor the numeric stream directly.
model <- dfr_adwin(target_feat='serie')

# Run the sequential detection loop and record every drift alarm.
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

# Print the drift positions detected by ADWIN.
detection[detection$type == 'drift',]

# Overlay the detected drifts on the original numeric stream.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
