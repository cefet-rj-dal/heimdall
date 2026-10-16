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

# Instantiate the passive baseline.
model <- dfr_passive()

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

# The baseline reports a drift on every observation.
sum(detection$event)
nrow(serie)

print(paste('Successfull run at', Sys.time()))
