gen_data <- function() {
  
  st_real_examples <- list()
  
  { # Brazilian Flight Data 1
    
    load('../stealthy/data-real/bfd_2023.rdata')
    
    bfd['batch_index'] <- format(bfd['expected_depart'], '%Y-%m-%d')
    ordered_batches <- sort(unique(bfd$batch_index))
    
    checkin <- function(X){
      return(X %in% ordered_batches[1:10])
    }
    
    selection <- sapply(bfd['batch_index'], checkin)
    
    st_real_examples$bfd1 <- bfd[as.vector(selection),]
  }
  
  

  return(st_real_examples)
}


if (FALSE) {
  #plot_examples <- function(st_real_examples) {
  #  for (i in 1:length(st_real_examples)) {
  #    data <- st__drift_examples[[i]]
  #    y <- data$serie
  #    x <- 1:length(y)
  #    plot(x = x, y = y)
  #    lines(x = x, y = y)
  #  }
  #}


  save_examples <- function(st_real_examples) {
    save(st_real_examples, file="data/st_real_examples.RData", compress = 'xz', compression_level = 9)
  }

  st_real_examples <- gen_data()
  #plot_examples(st_real_examples)
  save_examples(st_real_examples)
}

