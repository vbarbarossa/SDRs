
source('R/MASTER.R')
valerioUtils::libinv(c('dplyr','tidyr','ggplot2','sf'))

#' load table with all atributes
tab <- read.csv('tabs/stations_all_attributes.csv') %>%
  as_tibble()

#' check bivariate plots
p <- tab %>%
  select(colnames(tab)[92:ncol(tab)]) %>%
  gather(-SR, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = SR)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ var, scales = "free",ncol = 4) +
  theme_bw()
p

p_log <- tab %>%
  select(colnames(tab)[92:ncol(tab)]) %>%
  gather(-SR, key = "var", value = "value") %>% 
  ggplot(aes(x = log10(value), y = log10(SR))) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ var, scales = "free",ncol = 4) +
  theme_bw()
p_log

ggsave('figs/check_bivariate.pdf',p,width = 200,height = 300,units = 'mm')
ggsave('figs/check_bivariate_all_log.pdf',p_log,width = 200,height = 300,units = 'mm')


# map all the variables used spatially

# load the table
t <- read.csv('tabs/input_tab_transformed.csv')
t2 <- read.csv('tabs/input_tab.csv')
t3 <- read.csv('tabs/input_tab_transformed_divAREA.csv')
# read spatial data and bind
s <- read_sf('spatial/stations_catchments2.gpkg')

s <- s %>% select(ID = gsim.no) %>% right_join(t)

st_write(s,'spatial/input_tab.gpkg')


library(raster)
tc <- raster('spatial/temp_cur.tif')
th <- raster('spatial/temp_hist.tif')
plot(tc-th)
plot(raster('spatial/prec_cur.tif') - raster('spatial/prec_hist.tif'))


# main centroid map-----------------------------------------------------------------------------------------
library(sf)
s <- sf::read_sf('spatial/stations_catchments2.gpkg')

# calculate centroids of s
sc <- s %>% select(ID = gsim.no) %>% st_centroid()

# bind centroids to variables
t2 <- read.csv('tabs/input_tab.csv')
sc <- sc %>% right_join(t2)
s <- s %>% select(ID = gsim.no) %>% right_join(t2 %>% select(ID))


# base layers
crs_custom <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

world <- rnaturalearth::ne_countries(returnclass = "sf")[,1] %>%
  st_transform(crs_custom)
bb <- rnaturalearth::ne_download(type = "wgs84_bounding_box", category = "physical",returnclass = "sf") %>%
  st_transform(crs_custom)
graticules <- rnaturalearth::ne_download(type = "graticules_30", category = "physical",returnclass = "sf") %>%
  st_transform(crs_custom)

# and draw
p <- ggplot() +
  geom_sf(data = bb, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = graticules, fill = NA, color = "grey80", lwd = 0.1) +
  geom_sf(data = world, fill = "grey90", lwd = NA) +
  geom_sf(data = s, fill = "grey60", lwd = NA) +
  geom_sf(data = sc, aes(color = log10(SR_tot))) +
  # scale_fill_viridis_c(breaks = seq(0,1,0.1),
  #                      labels = seq(0,1,0.1),
  #                      limits = c(0,1),
  #                      option = 'C',na.value = "transparent") +
  # facet_grid(warming~scenario) +
  theme_minimal() +
  theme(text = element_text(size = 13),
        panel.grid.major = element_line(color=NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(6,'line'),
        strip.background = element_rect('white'),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1, size = 13),
        legend.title = element_blank()
  )
p
ggsave(paste0('figs/maps_RC.jpg'),p,
       width = 183,height = 200,dpi = 600,units = 'mm')





