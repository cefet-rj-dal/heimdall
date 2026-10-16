gen_data <- function() {
  require(tseries)
  require(forecast)

  st_drift_examples <- list()
  
  { # dataset uv_virtual_drift
    set.seed(1)
    n <- 100
    x <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi), 10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2)
    event <- rep(FALSE, length(x))
    event[c(100,200,300,400)] <- TRUE
    st_drift_examples$uv_virtual_drift <- data.frame(serie = x, event = event)
  }

  { # dataset uv_virtual-ct_real_drift
    
    require(ggplot2)
    set.seed(1)
    n <- 500  # Number of time points
    example_type='univariate_virtual-ct_real_drift'
    # Univariate Virtual Central Tendency Real Drift Example
    data <- as.data.frame(rnorm(n))
    names(data) <- c('serie')
    
    tsantes <- data[1:(n/2), 'serie', drop=FALSE]
    posdrift <- nrow(tsantes) + 1
    tsdepois <- data[posdrift:nrow(data), 'serie', drop=FALSE]
    
    tsdepois$serie <- tsdepois$serie + 10
    tsantes$target <- (tsantes$serie < 0)
    tsdepois$target <- (tsdepois$serie > 10)
    
    s_data <- rbind(tsantes, tsdepois)
    s_data$i <- 1:nrow(s_data)
    
    s_data$drift <- 0
    s_data$drift[n/2] <- 1
    
    st_drift_examples$uv_vct_real_drift <- s_data
  }
  
  { # dataset uv_virtual-dp_real_drift
    
    require(ggplot2)
    set.seed(1)
    n <- 500  # Number of time points
    example_type='univariate_virtual-dp_real_drift'
    # Univariate Virtual Dispersion Real Drift Example
    data <- as.data.frame(rnorm(n))
    names(data) <- c('serie')
    
    tsantes <- data[1:(n/2), 'serie', drop=FALSE]
    posdrift <- nrow(tsantes) + 1
    tsdepois <- data[posdrift:nrow(data), 'serie', drop=FALSE]
    
    tsantes$target <- (tsantes$serie > 0)
    tsdepois$serie <- rnorm(n/2, sd=10)
    tsdepois$target <- (tsdepois$serie < 0)
    
    
    s_data <- rbind(tsantes, tsdepois)
    s_data$i <- 1:nrow(s_data)
    
    s_data$drift <- 0
    s_data$drift[n/2] <- 1
    
    st_drift_examples$uv_vdp_real_drift <- s_data
  }
  
  { # dataset mv_real_drift
    
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
    
    st_drift_examples$mv_real_drift <- s_data
  }
  
  { # dataset mv_virtual-ct_real_drift
    
    require(ggplot2)
    set.seed(1)
    n <- 500  # Number of time points
    example_type='multivariate_virtual-ct_real_drift'
    # Multivariate Virtual Central Tendency Real Drift Example
    data <- as.data.frame(rnorm(n))
    names(data) <- c('serie1')
    data['serie2'] <- rnorm(n)
    
    tsantes <- data[1:(n/2),]
    posdrift <- nrow(tsantes) + 1
    tsdepois <- data[posdrift:nrow(data),]
    
    tsantes$target <- ((tsantes$serie1 > 0) & (tsantes$serie2 > 0)) | ( (tsantes$serie1 < 0) & (tsantes$serie2 < 0))
    tsdepois$target <- ((tsdepois$serie1 < 0) & (tsdepois$serie2 > 0)) | ( (tsdepois$serie1 > 0) & (tsdepois$serie2 < 0))
    tsdepois$serie1 <- tsdepois$serie1 + 15
    tsdepois$serie2 <- tsdepois$serie2 + 15
    
    s_data <- rbind(tsantes, tsdepois)
    s_data$i <- 1:nrow(s_data)
    
    s_data$drift <- 0
    s_data$drift[n/2] <- 1
    
    st_drift_examples$mv_vct_real_drift <- s_data
  }
  
  { # dataset mv_virtual-dp_real_drift
    
    require(ggplot2)
    set.seed(1)
    n <- 500  # Number of time points
    example_type='multivariate_virtual-dp_real_drift'
    # Multivariate Virtual Dispersion Real Drift Example
    data <- as.data.frame(rnorm(n))
    names(data) <- c('serie1')
    data['serie2'] <- rnorm(n)
    
    tsantes <- data[1:(n/2),]
    posdrift <- nrow(tsantes) + 1
    tsdepois <- data[posdrift:nrow(data),]
    
    tsantes$target <- ((tsantes$serie1 > 0) & (tsantes$serie2 > 0)) | ( (tsantes$serie1 < 0) & (tsantes$serie2 < 0))
    tsdepois$target <- ((tsdepois$serie1 < 0) & (tsdepois$serie2 > 0)) | ( (tsdepois$serie1 > 0) & (tsdepois$serie2 < 0))
    tsdepois$serie1 <- rnorm(n/2, sd=2)
    tsdepois$serie2 <- rnorm(n/2, sd=2)
    
    s_data <- rbind(tsantes, tsdepois)
    s_data$i <- 1:nrow(s_data)
    
    s_data$drift <- 0
    s_data$drift[n/2] <- 1
    
    st_drift_examples$mv_vdp_real_drift <- s_data
  }

  return(st_drift_examples)
}


if (TRUE) {
  save_examples <- function(st_drift_examples) {
    save(st_drift_examples, file="/home/lucas/heimdall/data/st_drift_examples.RData", compress = TRUE, version = 2)
  }

  st_drift_examples <- gen_data()
  save_examples(st_drift_examples)
}

