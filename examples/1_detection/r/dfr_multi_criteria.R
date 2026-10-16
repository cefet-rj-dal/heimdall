# Load Heimdall and the synthetic stream example.
library(heimdall)

# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)

# Load the univariate numeric stream monitored in this example.
data(st_drift_examples)
serie <- st_drift_examples$uv_virtual_drift

# Plot the monitored signal before detection.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Combine a detector of mean shifts with a detector of spread changes.
model <- dfr_multi_criteria(
  drifter_list=list(
    ph=dfr_page_hinkley(target_feat='serie'),
    lbdd=dfr_lbdd(target_feat='serie', window_size=100, alpha=0.001)
  ),
  combination='or'
)

# Update the combined detector sequentially and record every drift alarm.
detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in seq_len(nrow(serie))){
  output <- update_state(output$obj, serie[i, 'serie', drop=FALSE])
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

# Fit the combined detector on the whole stream to inspect the decision of each detector.
model <- fit(model, serie[, 'serie', drop=FALSE])
head(model$drifter_output)
colSums(model$drifter_output)

# Overlay the combined alarms on the original numeric stream.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}

print(paste('Successfull run at', Sys.time()))
