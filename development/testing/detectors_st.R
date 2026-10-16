# Run from the repository root. Reports are written to development/testing/history/
# (ignored by git), named with a timestamp that is valid on every platform.
heimdall_path <- './'
devtools::load_all(heimdall_path)

detectors_list <- c('adwin', 'aedd', 'cusum', 'ddm', 'ecdd', 'eddm', 'hddm', 'kswin', 'lbdd', 'mcdd', 'page_hinkley')

for (detector in detectors_list){
  rmarkdown::render(paste0(heimdall_path, 'Rmd/examples/1_detection/dfr_', detector, '.Rmd'), output_format='html_document', output_dir = paste0(heimdall_path, 'development/testing/history/'),output_file = paste0('dfr_', detector, '-', format(Sys.time(), '%Y%m%d-%H%M%S'), '.html'), quiet = TRUE)
  print(paste0('Successfull run dfr_', detector))
}

