#source("/home/lucas/heimdall/R/ac_drifter.R")
#source("/home/lucas/heimdall/R/ac_metrics.R")
#source("/home/lucas/heimdall/R/ac_stealthy.R")
#source("/home/lucas/heimdall/R/dfr_ddm.R")
#source("/home/lucas/heimdall/development/drift_techniques/dfr_ecdd.R")
source("/home/lucas/heimdall/development/drift_techniques/dfr_adwin.R")
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
library('heimdall')

#data("st_real_examples")
load('/home/lucas/heimdall/development/testing/data/bfd_2023.rdata')

#bfd <- st_real_examples$bfd1

bfd['batch_index'] <- format(bfd['expected_depart'], '%V')
bfd <- bfd[bfd['depart'] == 'SBSP',]

# Model features
features <- c(
  'depart_elevation', 'depart_wind_direction_cat', 'depart_visibility', 'depart_day_period', 'depart_pressure', 
  'depart_relative_humidity', 'depart_dew_point', 'depart_sky_coverage', 'depart_wind_speed_scale'
  #'delay_depart_bin'
)

## Target
bfd$delay_depart_bin <- bfd$delay_depart > 0
target = 'delay_depart_bin'
slevels <- c(TRUE, FALSE)

adwin <- import('river.drift.adwin')
ad <- adwin$ADWIN()


for (v in bfd$depart_visibility){
  ad$update(v)
  if (ad$drift_detected){
    print('Drift Detected')
  }
}