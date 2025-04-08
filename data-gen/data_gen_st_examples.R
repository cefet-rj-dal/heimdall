gen_data <- function() {
  require(tseries)
  require(forecast)

  st_drift_examples <- list()

  { # dataset 1
    
    require(ggplot2)
    set.seed(1)
    n <- 500  # Number of time points
    example_type='multivariate_real_drift'
    # Multivariate Real Drift Example
    data <- as.data.frame(rnorm(n))
    names(data) <- c('serie1')
    data['serie2'] <- rnorm(n)
    
    tsantes <- data[1:(n/2),]
    posdrift <- nrow(tsantes) + 1
    tsdepois <- data[posdrift:nrow(data),]
    
    tsantes$target <- ((tsantes$serie1 > 0) & (tsantes$serie2 > 0)) | ( (tsantes$serie1 < 0) & (tsantes$serie2 < 0))
    tsdepois$target <- ((tsdepois$serie1 < 0) & (tsdepois$serie2 > 0)) | ( (tsdepois$serie1 > 0) & (tsdepois$serie2 < 0))
    
    s_data <- rbind(tsantes, tsdepois)
    s_data$i <- 1:nrow(s_data)
    
    s_data$drift <- 0
    s_data$drift[n/2] <- 1
    
    st_drift_examples$dataset1 <- s_data
  }
  
  { # dataset 2
    
    require(ggplot2)
    set.seed(1)
    n <- 500  # Number of time points
    example_type='multivariate_virtual_drift'
    # Multivariate Virtual Drift Example
    data <- as.data.frame(rnorm(n))
    names(data) <- c('serie1')
    data['serie2'] <- rnorm(n)
    
    tsantes <- data[1:(n/2),]
    posdrift <- nrow(tsantes) + 1
    tsdepois <- data[posdrift:nrow(data),]
    
    tsdepois['serie1'] <- tsdepois['serie1'] + (3*sd(tsantes$serie1))
    tsdepois['serie2'] <- tsdepois['serie2'] + (3*sd(tsantes$serie2))
    
    s_data <- rbind(tsantes, tsdepois)
    s_data$i <- 1:nrow(s_data)
    
    s_data$drift <- 0
    s_data$drift[n/2] <- 1
    
    st_drift_examples$dataset2 <- s_data
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


if (TRUE) {
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
    save(st_drift_examples, file="/home/lucas/heimdall/data/st_drift_examples.RData", compress = TRUE, version = 2)
  }

  st_drift_examples <- gen_data()
  #plot_examples(st_drift_examples)
  save_examples(st_drift_examples)
}

