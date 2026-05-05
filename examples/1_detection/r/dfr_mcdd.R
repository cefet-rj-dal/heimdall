# Load Heimdall and the synthetic stream.
library(heimdall)

# Fix the seed to keep the walkthrough reproducible.
seed <- 1
set.seed(seed)

# Load the univariate series monitored in this example.
data(st_drift_examples)
serie <- st_drift_examples$univariate

# Plot the monitored signal before running the detector.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Instantiate the mean-comparison detector.
model <- dfr_mcdd(target_feat='serie', window_size=100)

# Update the detector sequentially and collect the alarm positions.
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

# Print the detected drift points.
detection[detection$type == 'drift',]

# Overlay the detected drifts on the original series.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
