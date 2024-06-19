#'@title ADWIN method
#'@description Adaptive Windowing method for concept drift detection <doi:10.1137/1.9781611972771.42>.
#'@param target_feat Feature to be monitored.
#'@param delta The delta parameter for the ADWIN algorithm.
#ADWIN detection: Bifet, Albert, and Ricard Gavalda. “Learning from time-changing data with adaptive windowing.” In Proceedings of the 2007 SIAM international conference on data mining, pp. 443-448. Society for Industrial and Applied Mathematics, 2007.
#ADWIN detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/adwin.py
#'@return `dfr_adwin` object
#'@examples
#'require("daltoolbox")
#'require('ggplot2')
#'require('darch')
#'
#'data("st_real_examples")
#'
#'bfd <- st_real_examples$bfd1
#'
#'bfd['batch_index'] <- format(bfd['expected_depart'], '%Y-%m-%d')
#'bfd <- bfd[bfd['depart'] == 'SBSP',]
#'
#'# Model features
#'features <- c(
#'  'depart_elevation', 'depart_wind_direction_cat', 'depart_visibility', 
#'    'depart_day_period', 'depart_pressure', 'depart_relative_humidity', 
#'    'depart_dew_point', 'depart_sky_coverage', 'depart_wind_speed_scale'
#')
#'
#'## Target
#'bfd$delay_depart_bin <- bfd$delay_depart > 0
#'target = 'delay_depart_bin'
#'slevels <- c(TRUE, FALSE)
#'
#'# Evaluation
#'th=0.5
#'
#'results <- c()
#'ordered_batches <- sort(unique(bfd$batch_index))
#'old_start_batch <- ordered_batches[1]
#'
#'# Classification Algorithm
#'model <- stealthy(cla_nb(target, slevels), dfr_adwin(target_feat='depart_visibility'))
#'
#'for (batch in ordered_batches[2:length(ordered_batches)]){
#'  print(batch)
#'  print(old_start_batch)
#'  
#'  new_batch <- bfd[bfd$batch_index == batch,]
#'  last_batch <- bfd[(bfd$batch_index < batch) & (bfd$batch_index >= old_start_batch),]
#'  
#'  old_start_batch <- batch
#'  
#'  x_train <- last_batch[, features]
#'  y_train <- last_batch[, target, drop=FALSE]
#'  
#'  x_test <- new_batch[, features]
#'  y_test <- new_batch[, target]
#'  
#'  model <- fit(model, x_train, y_train)
#'  
#'  test_predictions <- predict(model, x_test)
#'  y_pred <- test_predictions[, 2] > th
#'  
#'  # Evaluation
#'  precision <- evaluate(mt_precision(), y_pred, y_test)
#'  recall <- evaluate(mt_recall(), y_pred, y_test)
#'  f1 <- evaluate(mt_fscore(), y_pred, y_test)
#'  
#'  results <- rbind(results, 
#'                   c(
#'                     batch,
#'                     precision,
#'                     recall,
#'                     f1,
#'                     model$drifted
#'                   )
#'  )
#'  
#'  print(nrow(new_batch))
#'  print(nrow(last_batch))
#'}
#'results <- as.data.frame(results)
#'results['index'] <- as.Date(results$index)
#'names(results) <- c('index', 'precision', 'recall', 'f1', 'drift')
#'
#'ggplot(data=results, aes(x=as.Date(index), y=as.numeric(f1), group=1)) + 
#'  geom_line() +
#'  xlab('') +
#'  ylab('F1') +
#'  theme_classic()
#'
#'@export
dfr_adwin <- function(target_feat, delta=0.002) {
  obj <- dist_based(target_feat=target_feat)
  
  # Methods
  obj$init_buckets <- function(obj){

    state <- obj$state
    
    state$list_row_bucket <- LinkedList()
    state$last_bucket_row <- 0
    state$total <- 0
    state$variance <- 0
    state$width <- 0
    state$bucket_number <- 0
    
    obj$state <- state
    
    return(obj)
  }
  
  obj$insert_element_bucket <- function(obj, value, variance){
    
    state <- obj$state
    
    state$list_row_bucket$first <- state$list_row_bucket$first$insert_bucket(state$list_row_bucket$first, value, variance)

    state$bucket_number <- state$bucket_number + 1
    
    if (state$bucket_number > state$bucket_num_max){
      state$bucket_num_max <- state$bucket_number
    }
    
    obj$state <- state
    
    return(obj)
  }
  
  obj$get_change <- function(obj){
    return(obj$state$bln_bucket_deleted)
  }
  
  obj$delete_element <- function(obj){
    
    state <- obj$state
    
    node <- state$list_row_bucket$last
    n1 <- 2^state$last_bucket_row
    state$width <- state$width - n1
    state$total <- state$total - node$bucket_total[1]
    u1 <- node$bucket_total[1] / n1
    incremental_variance <- node$bucket_variance[1] + n1 * state$width * (u1 - state$total / state$width) * (u1 - state$total / state$width) / (n1 + state$width)
    state$variance <- state$variance - incremental_variance
    node <- node$remove_bucket(node)
    state$bucket_number <- state$bucket_number - 1
    
    if (node$bucket_size_row == 0){
      state$list_row_bucket <- state$list_row_bucket$remove_from_tail(state$list_row_bucket)
      state$last_bucket_row <- state$last_bucket_row - 1
    }
    
    obj$state <- state
    return(obj, n1)
  }
  
  obj$compress_buckets <- function(obj){
    
    state <- obj$state
    
    cursor <- state$list_row_bucket_first
    i = 0
    
    while(!is.null(cursor)){
      k <- cursor$bucket_size_row
      if (k == (max_buckets + 1)){
        next_node <- cursor$next_item
        if (is.null(next_node)){
          state$list_row_bucket <- state$list_row_bucket$add_to_tail(state$list_row_bucket)
          next_node <- cursor$next_item
          state$last_bucket_row <- state$last_bucket_row + 1
        }
        n1 <- 2^i
        n2 <- 2^i
        u1 <- cursor$bucket_total[1]/n1
        u2 <- cursor$bucket_total[2]/n2
        
        incremental_variance <- n1 * n2 * ((u1 - u2) * (u1 - u2)) / (n1 + n2)
        
        next_node <- next_node$insert_bucket(next_node, cursor$bucket_total[1] + cursor$bucket_total[2], cursor$bucket_variance[2] + incremental_variance)
        
        state$bucket_number <- state$bucket_number + 1
        
        cursor <- cursor$compress_bucket_row(cursor, 2)
        
        if (next_node$bucket_size_row <= max_buckets){
          break
        }
      }else{
        break
      }
      cursor <- cursor$next_item
      i <- i + 1
    }
    
    obj$state <- state
    return(obj)
  }
  
  obj$bln_cut_expression <- function(obj, n0, n1, u0, u1, v0, v1, abs_value, delta){
    print('Cut Expression')
    
    #print(abs_value)
    
    state <- obj$state
    
    n <- state$width
    dd <- log(2*log(n)/delta)
    v <- state$variance
    m <- (1 / (n0 - state$mint_min_window_length + 1)) + (1 / (n1 - state$mint_min_window_length + 1))
    epsilon <- sqrt(2 * m * v * dd) + 1 * 2 / 3 * dd * m
    
    print(epsilon)
    print(2 * m * v * dd)
    print(m)
    print(v)
    print(dd)
    
    return(abs(abs_value) > epsilon)
  }
  
  obj$detected_change <- function(obj){
    
    state <- obj$state
    
    bln_change <- FALSE
    bln_exit <- FALSE
    bln_bucket_deleted <- FALSE
    state$mint_time <- state$mint_time + 1
    
    n0 = 0
    if (((state$mint_time %% state$mint_clock) == 0) & (state$width > state$mint_min_window_longitude)){
      bln_reduce_width <- TRUE
      while (bln_reduce_width){
        bln_reduce_width <- !bln_reduce_width
        bln_exit <- FALSE
        n0 <- 0
        n1 <- state$width
        u0 <- 0
        u1 <- state$total
        v0 <- 0
        v1 <- state$variance
        n2 <- 0
        u2 <- 0
        cursor <- state$list_row_bucket$last
        i <- state$last_bucket_row
        
        while ((!bln_exit) & (!is.null(cursor))){
          for (k in 1:cursor$bucket_size_row){
            n2 <- 2^i
            u2 <- cursor$bucket_total[k]
            
            if (n0 > 0){
              v0 <- v0 + cursor$bucket_variance[k] + 1 * n0 * n2 * (u0/n0 - u2/n2) * (u0/n0 - u2/n2) / (n0 + n2)
            }
            
            if (n1 > 0){
              v1 <- v1 - cursor$bucket_variance[k] + 1 * n1 * n2 * (u1/n1 - u2/n2) * (u1/n1 - u2/n2) / (n1 + n2)
            }
            
            n0 <- n0 + (2^i)
            n1 <- n1 - (2^i)
            u0 <- u0 + cursor$bucket_total[k]
            u1 <- u1 - cursor$bucket_total[k]
            
            if ((i==0) & (k == (cursor$bucket_size_row))){
              bln_exit <- TRUE
              break
            }
          
            #print(n0 < state$mint_min_window_length)
            if ((n0 < state$mint_min_window_length) & (cursor$bucket_size_row)) stop("n0 is too low", call = FALSE)
            
            
            abs_value <- 1 * ((u0/n0) - (u1/n1))
            
            print(n0)
            print(n1)
            print(state$mint_min_window_length)
            print(obj$bln_cut_expression(obj, n0, n1, u0, u1, v0, v1, abs_value, state$delta))
            if ((n1 >= state$mint_min_window_length) & (n0 >= state$mint_min_window_length) & (obj$bln_cut_expression(obj, n0, n1, u0, u1, v0, v1, abs_value, state$delta))){
              bln_bucket_deleted <-TRUE
              state$detect <- state$mint_time
              if (state$detect == 0){
                state$detect <- state$mint_time
              }else if(state$detect_twice == 0){
                state$detect_twice <- state$mint_time
              }
              
              bln_reduce_width <- TRUE
              bln_change <- TRUE
              
              if (state$width > 0){
                obj$state <- state
                delete_return <- state$delete_element(state)
                obj <- delete_return[1]
                state <- obj$state
                n0 <- n0 - delete_return[2]
              }
            }
          }
          cursor <- cursor$previous
          i <- i - 1
        }
      }
    }
    state$mdbl_width <- state$mdbl_width + state$width
    if (bln_change){
      state$n_detections <- state$n_detections + 1
    }
    state$in_concept_change <- bln_change
    
    obj$drifted <- bln_change
    obj$state <- state
    return(obj)
  }
  
  # Attributes
  state <- list()
  
  state$delta <- delta
  state$last_bucket_row <- 0
  state$list_row_bucket <- NULL
  state$total <- 0
  state$variance <- 0
  state$width = 0
  state$bucket_number <- 0
  
  # Init buckets
  obj$state <- state
  obj <- obj$init_buckets(obj)
  state <- obj$state
  
  state$mint_min_window_longitude <- 10
  
  state$mdbl_delta <- 0.002
  state$mint_time <- 0
  state$mdbl_width <- 0
  
  state$detect <- 0
  state$n_detections <- 0
  state$detect_twice <- 0
  state$mint_clock <- 32
  
  state$bln_bucket_deleted <- FALSE
  state$bucket_num_max <- 0
  state$mint_min_window_length <- 5
  
  print('INIT')
  print(state$variance)
  
  # Reset
  
  obj$drifted <- FALSE
  obj$state <- state
  class(obj) <- append("dfr_adwin", class(obj))
  return(obj)
}

max_buckets <- 5

#'@export
update_state.dfr_adwin <- function(obj, value){
  #print('Update State')
  
  state <- obj$state
  
  state$width <- state$width + 1
  
  obj$state <- state
  obj <- obj$insert_element_bucket(obj,value, 0)
  state <- obj$state
  incremental_variance <- 0
  
  if (state$width > 1){
    incremental_variance <- (state$width - 1) * (value - state$total / (state$width - 1)) * (value - state$total / (state$width - 1)) / state$width
    
  }
  
  state$variance <- state$variance + incremental_variance
  state$total <- state$total + value
  
  obj$state <- state
  obj <- obj$compress_buckets(obj)
  obj <- obj$detected_change(obj)
  return(list(obj=obj, pred=obj$drifted))
}

#'@export
fit.dfr_adwin <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_adwin <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_adwin(
    delta = obj$state$delta
  )$state
  return(obj) 
}

#'@title Item class
#'@description Implements Item class from ADWIN
#'@param next_item Reference to the next Item in the list
#'@param previous_item Reference to the previous Item in the list
#'@return Item object
#'@export
Item <- function(next_item=NULL, previous_item=NULL) {
  obj <- list()
  
  # Methods
  obj$clear_buckets <- function(obj, index){
    obj$bucket_total[index] <- 0
    obj$bucket_variance[index] <- 0
    
    return(obj)
  }
  
  obj$reset <- function(obj){
    obj$bucket_size_row <- 1
    for (i in 1:(max_buckets + 1)){
      obj$clear_buckets(obj, i)
    }
    return(obj)
  }
  
  obj$insert_bucket <- function(obj, value, variance){
    new_item <- obj$bucket_size_row
    obj$bucket_size_row <- obj$bucket_size_row + 1
    obj$bucket_total[new_item] <- value
    obj$bucket_variance[new_item] <- variance
    
    return(obj)
  }
  
  obj$compress_bucket_row <- function(obj, num_deleted=1){
    for (i in num_deleted:(max_buckets + 1)){
      obj$bucket_total[i-num_deleted] <- obj$bucket_total[i]
      obj$bucket_variance[i-num_deleted] <- obj$bucket_variance[i]
    }
    
    for (i in 1:(num_deleted+1)){
      obj <- obj$clear_buckets(obj, max_buckets - i + 1)
    }
    
    obj$bucket_size_row <- obj$bucket_size_row - num_deleted
    return(obj)
  }
  
  obj$remove_bucket <- function(obj){
    obj <- obj$compress_bucket_row(obj, 1)
    return(obj)
  }
  
  # Attributes
  obj$next_item <- next_item
  obj$previous <- previous_item
  if (!missing(next_item)){
    next_item.previous <- obj
  }
  if (!missing(previous_item)){
    previous_item.next_item <- obj
  }
  
  obj$bucket_size_row <- NULL
  obj$max_buckets <- max_buckets
  obj$bucket_total <- numeric(max_buckets+1)
  obj$bucket_variance <- numeric(max_buckets+1)
  
  obj <-obj$reset(obj)
  
  class(obj) <- append("hcd_adwin", class(obj))
  return(obj)
}

#'@title LinkedList class
#'@description Implements LinkedList class from ADWIN
#'@return LinkedList object
#'@export
LinkedList <- function() {
  obj <- list()
  
  # Methods
  obj$reset <- function(obj){
    obj$count <- 0
    obj$first <- NULL
    obj$last <- NULL
    
    return(obj)
  }
  
  obj$add_to_head <- function(obj){
    obj$first <- Item(obj$first, NULL)
    if (is.null(obj$last)){
      obj$last <- obj$first
    }
    
    return(obj)
  }
  
  obj$remove_from_head <- function(obj){
    obj$first <- obj$first$next_item
    if (!is.null(obj$first)){
      obj$previous <- NULL
    }else{
      obj$last <- NULL
    }
    obj$count <- obj$count - 1
    
    return(obj)
  }
  
  obj$add_to_tail <- function(obj){
    obj$last <- Item(NULL, obj$last)
    if (is.null(obj$first)){
      obj$first <- obj$last
    }
    obj$count <- obj$count + 1
    
    return(obj)
  }
  
  obj$remove_from_tail <- function(obj){
    obj$last <- obj$last$previous
    if (!is.null(obj$last)){
      obj$last$next_item <- NULL
    }else{
      obj$first <- NULL
    }
    obj$count <- obj$count - 1
  }
  
  # Attributes
  obj$count <- NULL
  obj$first <- NULL
  obj$last <- NULL
  
  obj <- obj$reset(obj)
  obj <- obj$add_to_head(obj)
  
  class(obj) <- append("LinkedList", class(obj))
  return(obj)
}