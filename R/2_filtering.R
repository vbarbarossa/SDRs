# set wd
knitr::opts_knit$set(root.dir = 'D:/projects/SDRs')

#' load assigned variables from MASTER.R
source('R/MASTER.R')

# what has been loaded
as.list(.GlobalEnv)

#' load packages needed
valerioUtils::libinv(c('dplyr','tidyr','ggplot2','foreach'))

#' # Table formatting
#' 
#' Select the variables used in the modelling
#' 
#' First, check the correlation among the flow indices provided by GSIM

#' put together an overall table with attributes, flow metrics and SR
tab_all <- full_join(
  read.csv('tabs/stations_attributes.csv') %>% as_tibble(),
  read.csv('tabs/stations_indices.csv') %>% as_tibble()
) %>%
  full_join(.,sf::read_sf('spatial/stations_catchments2.gpkg') %>%
              as_tibble() %>% dplyr::select(-geom,-area.est)) %>%
  full_join(.,read.csv('tabs/stations_SR.csv'),by='gsim.no') %>%
  as_tibble()

write.csv(tab_all,'tabs/stations_all_attributes.csv',row.names = F)

# tab_all <- read.csv('tabs/stations_all_attributes.csv') %>% as_tibble()

# compute the correlation matrix <<<< NEED TO NORMALIZE COEFF FOR CORR MATRIX
cm <- cor(tab_all %>% select(colnames(tab_all)[92:which(colnames(tab_all) == 'DOYMAX7')]),
          use = "pairwise.complete.obs")

# and visualize 
corrplot::corrplot(cm, method = 'number', type = 'lower',number.cex = 0.8)

# and save
jpeg('figs/corrplot_initial_flow_indices.jpg',width = 200, height = 200, res = 600, units = 'mm')
corrplot::corrplot(cm, method = 'number', type = 'lower',number.cex = 0.8)
dev.off()

# check dams GOOD2 vs GRanD
plot(log10(tab_all$dams_good2_no),log10(tab_all$no.dams))
abline(0,1) 

# interesting, some catchments have more grand dams than good2
# true for
sum((tab_all$dams_good2_no-tab_all$no.dams) < 0) # catchments

#' Select flow indices and additional covariates based on informed choice and theoreical reasoning (see paper)
#' 
#' **Discharge covariates**
#' 
#' -	Flow variability: IQR/p50 
#' 
#' -	Minimum flow: MIN7 
#' 
#' -	Maximum flow: MAX7 
#' 
#' -	Mean (to link up with previous studies and for applied purposes) 
#' 
#' 
#' **Additional covariates**
#' 
#' *Ecosystem productivity:*
#' 
#' - climate zone (major Koppen-Geiger climate zone A-E) 
#' 
#' - latitude 
#' 
#' *Evolutionary diversification potential:*
#' 
#' -	Basin area (log-transformed) 
#' 
#' -	Mean topographic index of the catchment ln(CA/tan(slope_gradient)). Ranges from -1 to xx where high values indicate small catchments with high terrain roughness.
#' 
#' -	Elevation heterogeneity: use (q3-q1)/q2
#' 
#' -	Drainage network density
#' 
#' -	Quaternary climate stability

(tab <- tab_all %>%
  # variables that need to be computed
  mutate(
    PREC_DELTA = abs(prec_cur_mean - prec_hist_mean),
    TEMP_DELTA = abs(temp_cur_mean - temp_hist_mean),
    CROP_2015 = cropland_2015_sum/cropland_2015_count,
    CROP_1992 = cropland_1992_sum/cropland_1992_count,
    URB = nl.mean*area.est
  ) %>%
  mutate(
    CROP_PRES = (CROP_1992+CROP_2015)/2 * area.est
  ) %>%
  # select the variables and rename them
  select(
    # ID variables
    ID = gsim.no, 
    BAS = MAIN_BAS, 
    QUALITY = quality,
    
    # Discharge covariates
    Q_MEAN = MEAN, 
    Q_MIN = MIN7, 
    Q_MAX = MAX7, 
    Q_CV = CV, 
    Q_DOYMIN = DOYMIN7, 
    Q_DOYMAX = DOYMAX7,
    
    # Ecosystem productivity
    PREC_PRES = prec_cur_mean, 
    TEMP_PRES = temp_cur_mean, 
    
    # Quaternary climate stabolity
    PREC_DELTA, 
    TEMP_DELTA,
    
    # Habitat area and heterogeneity
    AREA = area.est, 
    TI = tp.mean, 
    ELEVATION = ele.mean, 
    SLOPE = slp.mean,
    
    # Anthropogenic
    POP = pop.count, 
    DAMS = dams_good2_no, 
    URB,
    CROP_PRES,
    
    # Response
    SR_tot, 
    SR_end
  ))

#' kick-out quality level 'caution' (meaning catchment area estimate is uncertain)
(tab <- tab %>%
  filter(QUALITY !="Caution"))

#' check for NAs and flow/precipitation metrics < 0 and remove those records

# NAs
apply(tab,2,function(x) sum(is.na(x)))

# looks like URB has NAs instead of zeroes
tab$URB[is.na(tab$URB)] <- 0

# Q < 0
apply(tab %>% select(starts_with('Q'),-QUALITY,starts_with('PREC')),2,function(x) sum(x < 0,na.rm=T))

# adjust NA and Q<0 values
(tab <- tab %>%
  tidyr::drop_na() %>% # we are also dropping 351 basins with NAs in DRAINAGE
  filter(Q_MIN >= 0))

#'  save the final table
write.csv(tab,'tabs/stations_filtered.csv',row.names = F)

# make a table with densities instead ########################################################################################

(tab <- tab_all %>%
    # variables that need to be computed
    mutate(
      PREC_DELTA = abs(prec_cur_mean - prec_hist_mean),
      TEMP_DELTA = abs(temp_cur_mean - temp_hist_mean),
      CROP_2015 = cropland_2015_sum/cropland_2015_count,
      CROP_1992 = cropland_1992_sum/cropland_1992_count,
      URB = nl.mean*area.est
    ) %>%
    mutate(
      CROP_PRES = (CROP_1992+CROP_2015)/2
    ) %>%
    # select the variables and rename them
    select(
      # ID variables
      ID = gsim.no, 
      BAS = MAIN_BAS, 
      QUALITY = quality,
      
      # Discharge covariates
      Q_MEAN = MEAN, 
      Q_MIN = MIN7, 
      Q_MAX = MAX7, 
      Q_CV = CV, 
      Q_DOYMIN = DOYMIN7, 
      Q_DOYMAX = DOYMAX7,
      
      # Ecosystem productivity
      PREC_PRES = prec_cur_mean, 
      TEMP_PRES = temp_cur_mean, 
      
      # Quaternary climate stabolity
      PREC_DELTA, 
      TEMP_DELTA,
      
      # Habitat area and heterogeneity
      AREA = area.est, 
      TI = tp.mean, 
      ELEVATION = ele.mean, 
      SLOPE = slp.mean,
      
      # Anthropogenic
      POP = pop.count, 
      DAMS = dams_good2_no, 
      URB,
      CROP_PRES,
      
      # Response
      SR_tot, 
      SR_end
    ) %>%
   mutate(
     POP = POP/AREA,
     DAMS = DAMS/AREA,
     URB = URB/AREA,
     CROP_PRES = CROP_PRES/AREA
   ))

#' kick-out quality level 'caution' (meaning catchment area estimate is uncertain)
(tab <- tab %>%
    filter(QUALITY !="Caution"))

#' check for NAs and flow/precipitation metrics < 0 and remove those records

# NAs
apply(tab,2,function(x) sum(is.na(x)))

# looks like URB has NAs instead of zeroes
tab$URB[is.na(tab$URB)] <- 0

# Q < 0
apply(tab %>% select(starts_with('Q'),-QUALITY,starts_with('PREC')),2,function(x) sum(x < 0,na.rm=T))

# adjust NA and Q<0 values
(tab <- tab %>%
    tidyr::drop_na() %>% # we are also dropping 351 basins with NAs in DRAINAGE
    filter(Q_MIN >= 0))

#'  save the final table
write.csv(tab,'tabs/stations_filtered_divAREA.csv',row.names = F)
