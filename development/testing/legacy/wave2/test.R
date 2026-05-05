library(daltoolbox)
library(heimdall)

data(st_drift_examples)
data <- st_drift_examples$univariate
data$event <- NULL
data$prediction <- st_drift_examples$univariate$serie > 4

model <- dfr_ecdd()

detection <- c()
output <- list(obj=model, pred=FALSE)
for (i in 1:length(data$serie)){
  output <- update_state(output$obj, data$serie[i])
  if (output$pred){
    type <- 'drift'
    output$obj <- reset_state(output$obj)
  }else{
    type <- ''
  }
  detection <- rbind(detection, list(idx=i, event=output$pred, type=type))
}

detection <- as.data.frame(detection)
detection[detection$type == 'drift',]
