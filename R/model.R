#' load assigned variables from MASTER.R
source('R/MASTER.R')

#' load packages needed
valerioUtils::libinv('dplyr'); valerioUtils::libinv('sf')


#' exclude stations with less than `r MIN_MONITORING_YEARS` years of monitoring.
#' 
#' Reliable yearly values are selected following Gudmundsson et al., 2018:
#' *Index values at a yearly time step are reliable if at least 350 daily observations are declared reliable.*

# use the homogeneity table to get a list of stations with MIN_MONITORING_YEARS or more good years of record
stations_selection <- read.csv(paste0(GSIM_dir,'GSIM_indices/HOMOGENEITY/yearly_homogeneity.csv')) %>%
  as_tibble() %>%
  filter(number.good.time.steps >= 10) %>%
  select(gsim.no,ends_with('step'),ends_with('steps'))

# scan through these stations and compute long-term mean of indices provided
start_time <- Sys.time() #monitor time elapsed
stations_indices <- lapply( # use lapply function that can be easily parallelized on linux
  pull(stations_selection,gsim.no), # get the vector of names from the tibble
  function(x){
    tab <- read.csv(paste0(GSIM_dir,'GSIM_indices/TIMESERIES/yearly/',x,'.year'),skip = 21) %>% #skip first 21 rows which are not tabulated
      dplyr::filter(n.available >= 350) %>%
      select(-date,-n.missing,-n.available) %>%
      #some files have been encoded as both csv and tab-delim
      #therefore I need to clean the strings from extra '\t' separators and read them again as numeric
      mutate_all(function(y) gsub('[\r\n\t]', '',y)) %>%
      #some records have n.available = 365 but 0 values everywhere
      #need to remove those. They have NA in CV (0/0) --> use that
      filter(CV != 'NA') %>% # no need to use is.na() as at this stage they are still all characters
      # and then convert to numeric to avoid warning messages
      mutate_all(as.numeric)
    
    return(
      bind_cols(data.frame(gsim.id = x),as.data.frame(t(colMeans(tab))) )
    )
  }
) %>% do.call('rbind',.) %>%
  full_join(stations_selection,., by = c('gsim.no' = 'gsim.id'))
Sys.time() - start_time #~6 min

#' merge indices with attribute table
#' 

stations_attributes <- read.csv(paste0(GSIM_dir,'GSIM_catalog/GSIM_catchment_characteristics.csv')) %>%
  as_tibble() %>%
  filter(gsim.no %in% stations_indices$gsim.no) %>%
  filter(!is.na(long.org)) %>% #make sure there are no missing values from the originla coordinates (but this is the case)
  # create new coordinate columns without missing values
  # paste the new lon, lat and where these are missing vals paste the original coordinates
  # this is needed to later plot the data points
  # since GSIM provides an estimated catchment boundary also for those stations 
  # that have not beed realocated by the automated routine
  mutate(LON = ifelse(is.na(long.new),long.org,long.new),
         LAT = ifelse(is.na(lat.new),lat.org,lat.new))

#' visualize the stations distribution

plot(st_geometry(st_as_sf(stations_attributes,coords = c('LON','LAT'),crs=4326)),
     pch = 21, cex = 0.5, main = paste0('n = ',nrow(stations_attributes)))

#' exclude stations with an upstream catchment area < `r MIN_UP_AREA` km2,
#' and check the plot again

plot(st_geometry(st_as_sf(stations_attributes %>% filter(area.est > MIN_UP_AREA),coords = c('LON','LAT'),crs=4326)),
     pch = 21, cex = 0.5, main = paste0('n = ',nrow(stations_attributes %>% filter(area.est > MIN_UP_AREA))))

#' exclude stations whose catchment boundary overlaps for more than `r MAX_OVERLAP_PERC`%

# assign a main basin value to each station
start_time <- Sys.time() #monitor time elapsed
basins <- lapply(
  c('af','ar','as','au','eu','gr','na','sa','si'), 
  function(x){
    readRDS(paste0(BAS_dir,x,'.rds')) %>%
      st_transform(54009) %>%
      mutate(MAIN_BAS_AREA = as.numeric(st_area(.)/10**6)) %>% #include area in km2
      # make the shapefile lighter, anyway there are no stations catchments < than the threshold
      # do minus 100 km2 since there might be differences in the area calculation depending on the projection used
      filter(MAIN_BAS_AREA >= (MIN_UP_AREA-100)) 
  }
) %>%
  do.call('rbind',.)
Sys.time() - start_time #~1.5 min

plot(st_geometry(basins)) # need to crop based on bounding box

st_intersects()

# for each main basin

# order from most to least downstream (using catchment area)

#identify the most downstream point

# intersect with upstream points

# calculate perc_overlap with intersecting points

# remove points that have perc_overlap > MAX_OVERLAP_PERC

# go to the next most downstream point and repeat until last row



#' exclude stations falling within areas with less than X% species coverage
#' 

stations_tab <- right_join(.,stations_indices)

#' exclude stations that within the same main basin (with an outlet to the sea) 
#' have the same stream order and overlapping upstream catchment
#' 

