# Run from the repository root. Reports are written to development/testing/history/
# (ignored by git), named with a timestamp that is valid on every platform.
heimdall_path <- './'
devtools::load_all(heimdall_path)

detectors_list <- c(
  'cusum', 'ddm', 'eddm', 'hddm' # Error-Based Drift Detectors
  ,
  'adwin','ecdd', 'kswin', 'mcdd', 'page_hinkley' # UV Dist-Based CT Drift Detectors
  ,
  'lbdd' # UV Dist-Based DP Drift Detectors
  ,
  'aedd', 'kldist' # MV Dist-Based Drift Detectors
                    )

for (detector in detectors_list){
  rmarkdown::render(paste0(heimdall_path, 'Rmd/examples/2_online_prediction/dfr_', detector, '.Rmd'), output_format='html_document', output_dir = paste0(heimdall_path, 'development/testing/history/'),output_file = paste0('dfr_', detector, '-', format(Sys.time(), '%Y%m%d-%H%M%S'), '.html'), quiet = TRUE)
  print(paste0('Successfull run dfr_', detector))
}

