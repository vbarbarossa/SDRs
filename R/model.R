#' load assigned variables from MASTER.R
source('R/MASTER.R')

#' load packages needed
valerioUtils::libinv('dplyr')

#' exclude stations with less than `r MIN_MONITORING_YEARS` years of monitoring
#' From Gudmundsson et al., 2018:
#' 
#' *Index values at a yearly time step are reliable if at least 350 daily observations are declared reliable.*

# use the homogeneity table to get a list of stations with MIN_MONITORING_YEARS or more good years of record
stations_selection <- read.csv(paste0(GSIM_dir,'GSIM_indices/HOMOGENEITY/yearly_homogeneity.csv')) %>%
  as_tibble() %>%
  filter(number.good.time.steps >= 10) %>%
  select(gsim.no,ends_with('step'),ends_with('steps'))

# scan through these stations and compute long-term mean of indices provided
stations_indices <- lapply( # use lapply function that can be easily parallelized on linux
  
)

#' exclude stations with an upstream catchment area < X km2
#' 



#' exclude stations falling within areas with less than X% species coverage
#' 




#' exclude stations that within the same main basin (with an outlet to the sea) 
#' have the same stream order and overlapping upstream catchment
#' 

