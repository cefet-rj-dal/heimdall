# Load Heimdall and the synthetic stream example.
library(heimdall)

# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)

# Load the univariate numeric stream monitored in this example.
data(st_drift_examples)
serie <- st_drift_examples$univariate

# Plot the monitored signal before detection.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Instantiate the Page-Hinkley detector.
model <- dfr_page_hinkley(target_feat='serie')

# Update the detector sequentially and record every drift alarm.
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

# Overlay those alarms on the original numeric stream.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
