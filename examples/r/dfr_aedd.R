# Load the autoencoder backend and Heimdall.
library(daltoolboxdp)
library(heimdall)

# Fix the seed so the example can be reproduced consistently.
seed <- 1
set.seed(seed)

# Load the multivariate synthetic stream.
data(st_drift_examples)
serie <- st_drift_examples$dataset2

# Plot the monitored variables before running the detector.
plot(x=serie$i, y=serie$serie1)
plot(x=serie$i, y=serie$serie2)

# Instantiate AEDD with a compact latent representation.
model <- dfr_aedd(
  encoding_size=1,
  ae_class=autoenc_ed,
  batch_size=64,
  monitoring_step=10,
  window_size=256
)
monitored_features <- c('serie1', 'serie2')

# Update AEDD row by row using only the selected monitored features.
detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in seq_len(nrow(serie))){
  output <- update_state(output$obj, serie[i, monitored_features])
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

# Display the alarms on top of one monitored variable.
plot(x=serie$i, y=serie$serie2)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
