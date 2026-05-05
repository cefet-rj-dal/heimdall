# install.packages('Metrics')
# Simulation
simulate <- function(df, df_name, features, params, batch_size, simulation_seed){
  
  set.seed(params[['simulation_seed']])
  
  results <- c()
  norm_df <- c()
  drift_input_df <- c()
  hist_proj <- c()
  recent_proj <- c()
  ordered_batches <- sort(unique(df$batch_index))
  old_start_batch <- ordered_batches[1]
  ## Train
  model <- stealthy(
    model=params[['classifier_algorithm']],
    drift_method=params[['drifter_method']],
    norm_class=params[['norm_class']],
    warmup_size=warmup_size,
    incremental_memory=params[['incremental_memory']],
    class_balance='buffer',
    target_uni_drifter=params[['target_uni_drifter']],
    active_warmup = TRUE,
    verbose=TRUE
    # reporting = TRUE
  )
  if (model$target_uni_drifter){drifter_target <- 'target'}else{drifter_target <- 'distribution'}
  if (model$incremental_memory){memory_type <- 'incremental'}else{memory_type <- 'stable'}
  filepath <- paste0(
    results_path,
    params[['simulation_seed']],
    '_',
    df_name, '-',
    class(model$model)[1], '-', 
    params[['drifter_name']], '-', 
    params[['norm_name']], '-',
    drifter_target, '-',
    memory_type,
    batch_size,
    '.rds')
  if(file.exists(filepath)){
    return()
  }
  
  # Simulation
  start_time <- Sys.time()
  for (batch in ordered_batches[2:length(ordered_batches)]){
    start_batch_time <- Sys.time()
    
    print(paste0('Time Step:', batch))
    # print(paste0('Last Batch:', old_start_batch))
    
    new_batch <- df[df$batch_index == batch,]
    last_batch <- df[(df$batch_index < batch) & (df$batch_index >= old_start_batch),]
    
    # print(nrow(last_batch))
    # print(nrow(new_batch))
    old_start_batch <- batch
    
    # Model Training
    x_train <- last_batch[, features]
    y_train <- last_batch[, target, drop=FALSE]
    
    x_test <- new_batch[, features]
    y_test <- new_batch[, target]
    
    model <- fit(model, x_train, y_train)
    
    if(model$fitted){
      test_predictions <- predict(model, x_test)
      y_pred <- factor(test_predictions[, 2] > th)
      levels(y_pred) <- slevels
      
      # Evaluation
      y_pred <- as.numeric(as.character(y_pred))
      y_test <- as.numeric(as.character(y_test))
      accuracy <- evaluate(mt_accuracy(), y_pred, y_test)
      precision <- evaluate(mt_precision(), y_pred, y_test)
      recall <- evaluate(mt_recall(), y_pred, y_test)
      f1 <- evaluate(mt_fscore(), y_pred, y_test)
      auc <- 0#evaluate(mt_rocauc(), test_predictions[['TRUE']], y_test)
    }else{
      accuracy <- NA
      precision <- NA
      recall <- NA
      f1 <- NA
      auc <- NA
    }
    
    elap_batch_time <- Sys.time() - start_batch_time
    
    results <- rbind(results, 
                     c(
                       batch,
                       accuracy,
                       precision,
                       recall,
                       f1,
                       auc,
                       model$drifted,
                       elap_batch_time
                     )
    )
    
  }
  elap_time <- Sys.time() - start_time
  results <- as.data.frame(results)
  results['index'] <- as.Date(results$index)
  names(results) <- c('index', 'accuracy', 'precision', 'recall', 'f1', 'auc', 'drift', 'elap_batch_time')
  
  results[, length(results)] <- NULL
  
  results['classifier'] <- class(model$model)[1]
  results['drifter'] <- class(model$drift_method)[1]
  results['drifter_target'] <- drifter_target
  results['incremental_memory'] <- model$incremental_memory
  results['elap'] <- elap_time
  
  # write.csv(
  #   results,
  #   filepath
  # )
  
  saveRDS(list(
    results=results, 
    drifter_input=model$drifter_input,
    drifter_output=model$drifter_output
    ), file = gsub('csv', 'rds', filepath))
  
  drift_input_df <- as.vector(drift_input_df)
  
  return(list(results=results, norm_df=norm_df, drift_input_df=drift_input_df, hist_proj=hist_proj, recent_proj=recent_proj))
}

# Experiments
run_experiments <- function(df, df_name, batch_size, window_size, monitoring_step, warmup_size){
  # Create Combination List
  classifiers <- list(
    # cla_nb=cla_nb(target, slevels),
    cla_dtree=cla_dtree(target, slevels)
    # cla_rf=cla_rf(target, slevels)
    # cla_svm=cla_svm(target, slevels)
  )
  
  alpha <- 0.05
  
  drifters <- list(
    # dfr_passive=dfr_passive(),
    # dfr_inactive=dfr_inactive(),
    # dfr_adwin=dfr_adwin(),
    # dfr_kldist=dfr_kldist(window_size=window_size),
    # dfr_kswin=dfr_kswin(window_size=window_size, stat_size=monitoring_step),
    # dfr_mcdd=dfr_mcdd(window_size=window_size),
    # dfr_page_hinkley=dfr_page_hinkley(threshold=20),
    dfr_aedd_var=dfr_aedd(ae_class=autoenc_ed, encoding_size=1, criteria='levene', window_size=window_size, monitoring_step=monitoring_step, alpha=alpha),
    dfr_aedd_ks=dfr_aedd(ae_class=autoenc_ed, encoding_size=1, criteria='kolmogorov_smirnov', window_size=window_size, monitoring_step=monitoring_step, alpha=alpha),
    dfr_aedd_ls_var=dfr_aedd(ae_class=autoenc_e, encoding_size=1, criteria='levene', window_size=window_size, monitoring_step=monitoring_step, alpha=alpha),#, alpha=0.001),
    dfr_aedd_ls_ks=dfr_aedd(ae_class=autoenc_e, encoding_size=1, criteria='kolmogorov_smirnov', window_size=window_size, monitoring_step=monitoring_step, alpha=alpha)
    # dfr_vaedd_var=dfr_aedd(ae_class=autoenc_variational_ed, encoding_size=1, criteria='levene', window_size=window_size, monitoring_step=monitoring_step),
    # dfr_vaedd_ks=dfr_aedd(ae_class=autoenc_variational_ed, encoding_size=1, criteria='kolmogorov_smirnov', window_size=window_size, monitoring_step=monitoring_step),
    # dfr_vaedd_ls_var=dfr_aedd(ae_class=autoenc_variational_e, encoding_size=1, criteria='levene', window_size=window_size, monitoring_step=monitoring_step),
    # dfr_vaedd_ls_ks=dfr_aedd(ae_class=autoenc_variational_e, encoding_size=1, criteria='kolmogorov_smirnov', window_size=window_size, monitoring_step=monitoring_step)
    # dfr_adedd_var=dfr_aedd(ae_class=autoenc_adv_ed, encoding_size=1, criteria='levene', window_size=window_size, monitoring_step=monitoring_step, reporting=TRUE),
    # dfr_adedd_ks=dfr_aedd(ae_class=autoenc_adv_ed, encoding_size=1, criteria='kolmogorov_smirnov', window_size=window_size, monitoring_step=monitoring_step, reporting=TRUE),
    # dfr_adedd_ls_var=dfr_aedd(ae_class=autoenc_adv_e, encoding_size=1, criteria='levene', window_size=window_size, monitoring_step=monitoring_step, reporting=TRUE),
    # dfr_adedd_ls_ks=dfr_aedd(ae_class=autoenc_adv_e, encoding_size=1, criteria='kolmogorov_smirnov', window_size=window_size, monitoring_step=monitoring_step, reporting=TRUE)
  )
  # 'kolmogorov-smirnov'
  
  norm_class_list <- list(
    # zscore=nrm_memory(norm_class = zscore()),
    minmax=nrm_memory(norm_class = minmax())
    # fixed_zscore=nrm_memory(norm_class = fixed_zscore())
  )
  
  memories <- list(
    # TRUE,
    FALSE
  )
  
  target_uni_drifters <- list(
    # TRUE
    FALSE
  )
  
  balancing <- list(
    'inactive',
    'buffer'
  )
  
  
  print(seeds_param)
  experiment_seeds <- seeds_param
  
  param_list <- list()
  i <- 1
  for(cla in classifiers){
    for(dft in names(drifters)){
      for(nrm in names(norm_class_list)){
        for(mem in memories){
          for(tgt in target_uni_drifters){
            for(bl in balancing){
              for(sd in experiment_seeds){
                param_list[[i]] <- list(
                  classifier_algorithm=cla,
                  drifter_name=dft,
                  drifter_method=drifters[[dft]],
                  norm_name=nrm,
                  norm_class=norm_class_list[[nrm]],
                  balancing=bl,
                  incremental_memory=mem,
                  target_uni_drifter=tgt,
                  simulation_seed=sd
                )
                i <- i + 1
              }
            }
          }
        }
      }
    }
  }
  
  i <- 1
  # foreach(params = param_list, .options.future = list(seed = TRUE)) %dofuture% {
  for(params in param_list) {
    print(paste0('Progress:', i/length(param_list)))
    
    report <- simulate(df, df_name, features, params, batch_size=batch_size)
    
    i <- i + 1
  }

}