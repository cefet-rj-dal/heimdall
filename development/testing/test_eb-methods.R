#library('heimdall')
library(devtools)
load_all('/home/lucas/heimdall/R/')
# load_all("/home/lucas/daltoolbox/R/")
#source("/home/lucas/heimdall/R/ac_drifter.R")
#source("/home/lucas/heimdall/R/ac_metrics.R")
#source("/home/lucas/heimdall/R/ac_stealthy.R")
#source("/home/lucas/heimdall/R/dfr_ddm.R")
#source("/home/lucas/heimdall/R/dfr_ecdd.R")
#source("/home/lucas/heimdall/R/dfr_adwin.R")
#source("/home/lucas/heimdall/R/dfr_cumsum.R")
#source("/home/lucas/heimdall/R/dfr_mcdd.R")
#source("/home/lucas/heimdall/R/dfr_aedd.R")
#source("/home/lucas/heimdall/R/dfr_eddm.R")
#source("/home/lucas/heimdall/R/dfr_hddm.R")
#source("/home/lucas/heimdall/R/dfr_page_hinkley.R")
#source("/home/lucas/heimdall/development/iterator.R")

#install.packages("/home/lucas/heimdall",
#                 repos = NULL, 
#                 type = "source")


library("daltoolbox")
library("dplyr")
library('ggplot2')
library('reticulate')
library('caret')

#data("st_real_examples")
load('/home/lucas/heimdall/development/testing/data/bfd_2019.rdata')

#bfd <- st_real_examples$bfd1

bfd['batch_index'] <- format(bfd['expected_depart'], '%V')
bfd <- bfd[bfd['depart'] == 'SBSP',]
bfd <- subset(bfd, !is.na(depart_visibility))

## Target
bfd$delay_depart_bin <- bfd$delay_depart > 0
target = 'delay_depart_bin'
slevels <- c(TRUE, FALSE)

# Model features
features <- c(
  'depart_elevation', #'depart_wind_direction_cat', 
  'depart_visibility', #'depart_day_period', 
  'depart_pressure', 
  'depart_relative_humidity', 
  'depart_dew_point'#,
  #'depart_sky_coverage', 'depart_wind_speed_scale'
  #'delay_depart_bin'
  )

# Remove NA
bfd <- bfd[complete.cases(bfd[,c(features, target)]),]

# Save bfd data
#write.csv(bfd, '/home/lucas/bfd.csv')

# Evaluation
th=0.5

results <- c()
ordered_batches <- sort(unique(bfd$batch_index))
old_start_batch <- ordered_batches[1]

# Classification Algorithm
#dfr_aedd(features=features, input_size=length(features), encoding_size=3, window_size=1800, criteria='kolmogorov_smirnov')
#dfr_caedd(features=features, input_size=length(features), encoding_size=3, window_size=1800, criteria='mann_whitney')
model <- stealthy(manager(algorithm=cla_nb(target, slevels), model_selector=selector(), model_deleter=selector()), dfr_page_hinkley(), target_uni_drifter=TRUE, verbose=TRUE)

for (batch in ordered_batches[2:length(ordered_batches)]){
  print(batch)
  print(old_start_batch)
  
  new_batch <- bfd[bfd$batch_index == batch,]
  last_batch <- bfd[(bfd$batch_index < batch) & (bfd$batch_index >= old_start_batch),]
  
  old_start_batch <- batch
  
  x_train <- last_batch[, features]
  y_train <- last_batch[, target, drop=FALSE]
  
  x_test <- new_batch[, features]
  y_test <- new_batch[, target]
  
  model <- fit(model, x_train, y_train)
  
  test_predictions <- predict(model, x_test)
  y_pred <- test_predictions[, 2] > th
  
  # Evaluation
  accuracy <- evaluate(mt_accuracy(), y_pred, y_test)
  precision <- evaluate(mt_precision(), y_pred, y_test)
  recall <- evaluate(mt_recall(), y_pred, y_test)
  f1 <- evaluate(mt_fscore(), y_pred, y_test)
  
  results <- rbind(results, 
                   c(
                     batch,
                     accuracy,
                     precision,
                     recall,
                     f1,
                     model$drifted
                     )
                   )

  print(nrow(new_batch))
  print(nrow(last_batch))
}
results <- as.data.frame(results)
results['index'] <- as.Date(results$index)
names(results) <- c('index', 'accuracy', 'precision', 'recall', 'f1', 'drift')

results_plot <- ggplot(data=results, aes(x=index, y=as.numeric(accuracy), group=1)) + 
  geom_line() +
  xlab('') +
  ylab('Accuracy') +
  theme_classic()

for (detection in results[results['drift'] == TRUE, 'index']){
  results_plot <- results_plot + geom_vline(xintercept=detection, linetype='dotted', color='red')
}

results_plot
