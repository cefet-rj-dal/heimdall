# Load the autoencoder backend and Heimdall.
library(daltoolbox, warn.conflicts = FALSE)
library(daltoolboxdp)
library(heimdall)

# Ask reticulate for the Python modules used by the daltoolboxdp autoencoders.
reticulate::py_require(c("numpy", "pandas", "scipy", "torch"))

# Fix the seed so the example can be reproduced consistently.
seed <- 1
set.seed(seed)

# Load the multivariate synthetic stream.
data(st_drift_examples)
serie <- st_drift_examples$mv_vct_real_drift

# Plot the monitored variables before running the detector.
plot(x=serie$i, y=serie$serie1)

plot(x=serie$i, y=serie$serie2)

# The autoencoder expects values in [0, 1], so rescale the monitored features.
# For simplicity the scale is learned on the whole stream; in production, learn
# it on a reference period and apply it to the incoming data.
monitored_features <- c('serie1', 'serie2')
normalizer <- fit(minmax(), serie[, monitored_features])
serie[, monitored_features] <- transform(normalizer, serie[, monitored_features])

# Instantiate AEDD with a compact latent representation.
model <- dfr_aedd(
  encoding_size=1,
  ae_class=autoenc_ed,
  batch_size=64,
  monitoring_step=10,
  window_size=256
)

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

print(paste('Successfull run at', Sys.time()))
