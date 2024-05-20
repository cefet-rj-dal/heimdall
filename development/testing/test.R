library("daltoolbox")
source("/home/lucas/lucas/cdr/stealthy.R")
source("/home/lucas/lucas/cdr/drifter.R")
source("/home/lucas/lucas/cdr/drift_techniques/dd_page_hinkley.R")
source("/home/lucas/lucas/cdr/iterator.R")

n <- 100  # Number of time points
example_type='univariate'
data <- as.data.frame(c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi), 10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2))
names(data) <- c('serie1')
if (example_type == 'multivariate'){
  data['serie2'] <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi), 10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2) + runif(length(data), 0, 1)
}


dt <- dd_page_hinkley()

detect(dt, data)

#model <- fit(hcd_hddm(drift_confidence = 0.0000000001, warning_confidence=0.6), data)
#detection <- detect(model, data)
#grf <- har_plot(model, data$serie1, detection)
#grf <- grf + ylab("value")
#grf <- grf
#plot(grf)