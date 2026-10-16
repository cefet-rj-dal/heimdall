# Load Heimdall and the synthetic stream example.
library(heimdall)

# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)

# Load the univariate numeric stream used in this example.
data(st_drift_examples)
serie <- st_drift_examples$uv_virtual_drift

# Plot the stream; it has four known change points.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Instantiate the inactive baseline.
model <- dfr_inactive()

# Run the usual streaming loop.
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

# The baseline never reports a drift, whatever the stream.
detection[detection$type == 'drift',]
sum(detection$event)

# The known change points are still there; the baseline simply ignores them.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in which(serie$event)) {
  abline(v=drift_index, col='blue', lty=2)
}

print(paste('Successfull run at', Sys.time()))
