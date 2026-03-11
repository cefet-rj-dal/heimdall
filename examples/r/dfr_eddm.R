# Loading heimdall
library(heimdall)

# EDDM example
# EDDM is shown here as a real concept drift detector over a binary error stream.
seed <- 1
set.seed(seed)

# Load data

data(st_drift_examples)
data <- st_drift_examples$univariate
data$prediction <- st_drift_examples$univariate$serie > 4

# Plot binary error stream

plot(x=seq_len(nrow(data)), y=data$prediction)

# Instantiate model

model <- dfr_eddm()

# Detection

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

# Detected drifts

detection[detection$type == 'drift',]

# Plot drifts over the original signal

plot(x=seq_len(nrow(data)), y=data$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
