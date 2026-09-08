### functions for package development


if (FALSE) {
  library(devtools)
  load_all()
}

if (FALSE) {
  library(devtools)
  document()
  load_all()
}

if (FALSE) { #run the test suite
  library(devtools)
  test()
}

if (FALSE) {
  library(devtools)
  suppressWarnings(check())
  load_all()
}

if (FALSE) {
  library(devtools)
  devtools::build_manual()
}

if (FALSE) {
  #update documentation site
  pkgdown::build_site()
}

if (FALSE) {
  #update homepage - edit README.Rmd
  library(devtools)
  devtools::build_readme()
}

if (FALSE) {
  devtools::install(dependencies = TRUE)
}

if (FALSE) { #build package for cran
  #run in RStudio
  library(devtools)
  pkgbuild::build(manual = TRUE)

  #run in terminal
  #R CMD check heimdall_1.3.0.tar.gz
  #R CMD check heimdall_1.3.0.tar.gz --as-cran
  #resaveRdaFiles('data/st_drift_examples.RData')

  #upload package
  #https://cran.r-project.org/submit.html
}
