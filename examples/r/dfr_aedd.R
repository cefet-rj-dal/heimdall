# Loading heimdall
library(daltoolboxdp)
library(heimdall)

# AEDD example
# AEDD is an unsupervised detector for virtual concept drift in multivariate data.
seed <- 1
set.seed(seed)

# Load data

data(st_drift_examples)
serie <- st_drift_examples$dataset2

# Plot monitored variables

plot(x=serie$i, y=serie$serie1)
plot(x=serie$i, y=serie$serie2)

# Instantiate model

model <- dfr_aedd(
  encoding_size=1,
  ae_class=autoenc_ed,
  batch_size=64,
  monitoring_step=10,
  window_size=256
)
monitored_features <- c('serie1', 'serie2')

# Detection

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

# Detected drifts

detection[detection$type == 'drift',]

# Plot drifts

plot(x=serie$i, y=serie$serie2)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
