gen_data <- function() {
  require(tseries)
  require(forecast)

  st_drift_examples <- list()

  { # dataset 1
    
    require(ggplot2)
    set.seed(1)
    n <- 500  # Number of time points
    example_type='multivariate'
    # Multivariate Example
    data <- as.data.frame(rnorm(n))
    names(data) <- c('serie1')
    data['serie2'] <- rnorm(n)
    
    data$drift <- ((data$serie1 > 0) & (data$serie2 > 0)) | ( (data$serie1 < 0) & (data$serie2 < 0))
    
    tsantes <- data[data$drift==FALSE,]
    posdrift <- nrow(tsantes) + 1
    tsdepois <- data[data$drift==TRUE,]
    
    s_data <- rbind(tsantes, tsdepois)
    s_data$i <- 1:nrow(s_data)
    
    st_drift_examples$dataset1 <- s_data
  }
  
  {
    set.seed(1)
    n <- 100
    x <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi), 10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2)
    event <- rep(FALSE, length(x))
    event[c(100,200,300,400)] <- TRUE
    st_drift_examples$univariate <- data.frame(serie = x, event = event)
  }

  return(st_drift_examples)
}


if (FALSE) {
  #plot_examples <- function(st_drift_examples) {
  #  for (i in 1:length(st_drift_examples)) {
  #    data <- st__drift_examples[[i]]
  #    y <- data$serie
  #    x <- 1:length(y)
  #    plot(x = x, y = y)
  #    lines(x = x, y = y)
  #  }
  #}


  save_examples <- function(st_drift_examples) {
    save(st_drift_examples, file="data/st_drift_examples.RData", compress = 'xz', compression_level=6)
  }

  st_drift_examples <- gen_data()
  #plot_examples(st_drift_examples)
  save_examples(st_drift_examples)
}

