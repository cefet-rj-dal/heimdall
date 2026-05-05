heimdall_path <- '//home/lucas/heimdall/'

detectors_list <- c('adwin', 'aedd', 'cusum', 'ddm', 'ecdd', 'eddm', 'hddm', 'kswin', 'lbdd', 'mcdd', 'page_hinkley')

for (detector in detectors_list){
  rmarkdown::render(paste0(heimdall_path, 'examples/1_detection/dfr_', detector, '.Rmd'), output_format='html_document', output_dir = paste0(heimdall_path, 'testing/history/'),output_file = paste0('dfr_', detector, '-', Sys.time(), '.html'), quiet = TRUE)
  print(paste0('Successfull run dfr_', detector))
}

