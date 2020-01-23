# set wd
knitr::opts_knit$set(root.dir = 'D:/projects/SDRs')

#' load assigned variables from MASTER.R
source('R/MASTER.R')

# what has been loaded
as.list(.GlobalEnv)

#' load packages needed
valerioUtils::libinv(c('dplyr','sf'))


#' *****************************************************************************************************************************
#' # Compile monitoring stations data
#' 
#' 
#' ## Flow data
#' 
#' 
#' Exclude stations with less than `r MIN_MONITORING_YEARS` years of monitoring.
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

#' ## Physical attributes
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

stations_attributes <- stations_attributes %>% filter(area.est > MIN_UP_AREA)

plot(st_geometry(st_as_sf(stations_attributes,coords = c('LON','LAT'),crs=4326)),
     pch = 21, cex = 0.5, main = paste0('n = ',nrow(stations_attributes %>% filter(area.est > MIN_UP_AREA))))

#' exclude stations whose catchment boundary overlaps for more than `r MAX_OVERLAP_PERC`%

bb <- rnaturalearth::ne_download(type = "wgs84_bounding_box", category = "physical",returnclass = "sf") #%>% st_transform(54009)

# assign a main basin value to each station
start_time <- Sys.time() #monitor time elapsed
basins <- lapply(
  c('af','ar','as','au','eu','gr','na','sa','si'), 
  function(x){
    readRDS(paste0(BAS_dir,x,'.rds')) %>%
      st_buffer(0) %>%
      st_crop(bb) %>%
      st_transform(54009) %>%
      mutate(MAIN_BAS_AREA = as.numeric(st_area(.)/10**6)) %>% #include area in km2
      # make the shapefile lighter, anyway there are no stations catchments < than the threshold
      # do minus 100 km2 since there might be differences in the area calculation depending on the projection used
      filter(MAIN_BAS_AREA >= (MIN_UP_AREA-100)) 
  }
) %>%  do.call('rbind',.)
Sys.time() - start_time #~1.5 min

# get the MAIN_BAS id for each station by intersecting the polygons with the points
start_time <- Sys.time()
stations_filter1 <- st_intersection(
  st_as_sf(stations_attributes,coords = c('LON','LAT'),crs=4326) %>% st_transform(54009),
  basins %>% st_buffer(0) # buffer to avoid TopologyException error
) %>% as_tibble() %>% select(-geometry)
Sys.time() - start_time # ~6 min


# core function that compile the catchments for the stations based on the maximum area overlap criteria
start_time <- Sys.time()
stations_catchments <- lapply(
  # from the main basin with more stations
  table(stations_filter1$MAIN_BAS %>% as.character) %>% sort(.,decreasing = T) %>% names,
  function(ws){
    
    # sub-table with only the needed stations belonging to the main basin
    t <- stations_filter1 %>% filter(MAIN_BAS == ws) %>% select(gsim.no,area.est,MAIN_BAS)
    
    if(nrow(t) == 1){
      # simply return the catchment shpefile
      catch <- read_sf(paste0(GSIM_dir,'GSIM_catchments/',tolower(as.character(t$gsim.no)),'.shp'))%>% 
        mutate(gsim.no = t$gsim.no) %>% select(gsim.no) %>%
        full_join(.,t) %>%
        st_buffer(0) %>% # buffer for any further geometry operation needed
        select(gsim.no,area.est,MAIN_BAS)
      
      return(catch)
      
    }else{
      
      # compile a shapefile with all the catchments
      catch <- lapply(
        as.character(t$gsim.no), function(x) read_sf(paste0(GSIM_dir,'GSIM_catchments/',tolower(x),'.shp'))
      ) %>% do.call('rbind',.) %>% mutate(gsim.no = t$gsim.no) %>% select(gsim.no) %>%
        full_join(.,t) %>%
        # order from most to least downstream (using catchment area)
        arrange(desc(area.est)) %>%
        st_buffer(0) %>% 
        select(gsim.no,area.est,MAIN_BAS)
      
      # set index for iterative routine
      i = 1
      
      # iterate from most downstream to most upstream station
      # and exclude stations based on catchment overlap criteria
      while(!is.na(catch$gsim.no[i])){ # this stops at the end of the rows of the catch shapefile 
        
        # calculate difference polygon of most downstream catchment with upstream catchments
        d <- st_difference(catch[i,],catch[i+1:nrow(catch),]) %>% 
          # transform to projected coordinates to calculate area
          st_transform(54009) %>% 
          # calculate perc_overlap with intersecting points as (1 - area_diff/area)*100 = (area_up/area)*100
          mutate(perc_overlap = (1 - as.numeric(st_area(.)/10**6)/area.est)*100) %>% 
          # filter out stations with overlap > MAX_OVERLAP_PERC
          filter(perc_overlap <= MAX_OVERLAP_PERC)
        
        # update the catchment shapefile accordingly
        catch <- catch %>%
          filter(gsim.no %in% c(as.character(gsim.no[1:i]),as.character(d$gsim.no.1))) %>%
          arrange(desc(area.est))
        
        # update index
        i = i + 1
      }
      
      return(catch)
    }
    
  }
) %>% do.call('rbind',.)
Sys.time() - start_time #~ 26 min

#' exclude stations falling within areas with less than X% species coverage
#' <span style="color:red"> **TBD** </span>

#' save important layers

# tables
write.csv(stations_indices %>% filter(gsim.no %in% stations_catchments$gsim.no),'tabs/stations_indices.csv',row.names = F)
write.csv(stations_attributes %>% filter(gsim.no %in% stations_catchments$gsim.no),'tabs/stations_attributes.csv',row.names = F)

# shapefiles
st_write(stations_attributes %>% filter(gsim.no %in% stations_catchments$gsim.no) %>% 
           st_as_sf(.,coords = c('LON','LAT'),crs=4326) %>% select(gsim.no,area.est,quality),
         'spatial/stations_points.gpkg'
)
st_write(stations_catchments,
         'spatial/stations_catchments.gpkg'
)
st_write(basins %>% filter(MAIN_BAS %in% stations_catchments$MAIN_BAS) %>% st_transform(4326),
         'spatial/main_basins.gpkg'
)

# clear the console
rm(list = ls())


#' *****************************************************************************************************************************
#' # Species richness data sampling
#' 
#' 
#' **Q: should we exclude species occurring in endoreic basins?**

source('R/MASTER.R')


#' intersect the catchments with the HB12 points to assign the gsim id to each catchment

# load hydrobasins 12 shpefile points layer
hb12_p <- read_sf('data/hybas12_points_nolakes.gpkg') %>%
  select(HYBAS_ID,MAIN_BAS)

# load the previously produced catchments layer
catch <- read_sf('spatial/stations_catchments.gpkg')

# check crs
st_crs(hb12_p) == st_crs(catch)

# get the sparse list of intersections
lst <- st_intersects(catch,hb12_p)

# number of catchments without hydrobasins:
length(lst) - lst %>% sapply(.,function(x) length(x) != 0) %>% sum

# ids of hydrobasins
hybas_id <- hb12_p$HYBAS_ID

# compile table with HYBAS_ID and corresponding gsim.no
names(lst) <- catch$gsim.no
catch_hb <- lst[which(unname(lst %>% sapply(.,function(x) length(x) != 0)))] %>% 
  lapply(seq_along(.), function(n,v,i){data.frame(HYBAS_ID = hybas_id[v[[i]]],gsim.no = n[i])},n=names(.),v = .) %>%
  do.call('rbind',.) %>%
  as_tibble()

#' check fish endemism at the basin scale: if a fish species occurs in only one main basin then it is considered endemic

# get the fish data
fish <- bind_rows(
  vroom::vroom('data/hybas12_fish.csv',delim=','),
  vroom::vroom('data/hybas12_fish_custom_ranges_occth10.csv',delim=',')
)

# merge with hb12_p data to have info on MAIN_BAS

fish_end <- fish %>%
  full_join(.,hb12_p %>% as_tibble() %>% select(-geom)) %>%
  group_by(binomial) %>%
  summarize(endemism = as.integer(length(unique(MAIN_BAS)) == 1))

#' join table with the fish data
fish <- fish %>% 
  full_join(.,fish_end) %>%
  filter(HYBAS_ID %in% catch_hb$HYBAS_ID) %>%
  full_join(.,catch_hb)

#' include species traits and functional groups
#' <span style="color:red"> **TBD** </span>

# TBD

#' calculate species richness per catchment

fish_sr <- fish %>%
  group_by(gsim.no) %>%
  summarize(
    SR_tot = length(unique(binomial)),
    SR_end = length(unique(binomial[endemism == 1]))
  )

# range of SR values
summary(fish_sr)

# save table
write.csv(fish_sr,'tabs/stations_SR.csv',row.names = F)

#' put together and overall table with attributes, flow metrics and SR

write.csv(
  full_join(
    read.csv('tabs/stations_attributes.csv') %>% as_tibble(),
    read.csv('tabs/stations_indices.csv') %>% as_tibble()
  ) %>%
    full_join(.,catch %>% as_tibble() %>% select(gsim.no,MAIN_BAS)) %>%
    full_join(.,fish_sr)
  ,'tabs/stations_all_attributes.csv',row.names = F
)




