## this step calculate movement metrics, local fence density, and produce data table to be put into the mixed models

################################
########### set up  ############
################################
#setwd("G:/My Drive/RESEARCH/Pronghorn/BaBA_Season2/")
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2/")

library(tidyverse)
library(amt)
library(hrbrthemes)
library(lubridate)

## read data
# raw movement data
deer <- read_csv("./data/deer_2h_pts.csv") %>% 
  mutate(date = ymd_hms(date, tz = "US/Mountain"),
         mo = month(date),
         yr = year(date),
         dy = day(date),
         id_yr_mo = paste0(animalID, "-", yr, "-", mo)) %>% 
  filter(!is.na(date)) 
length(unique(deer$id_yr_mo)) #2559

# remove individual month that is not complete (at least have 28-day data in a month)
deer1 <- deer %>% dplyr::select(id_yr_mo, dy) %>% 
  group_by(id_yr_mo) %>% 
  summarise(n = length(unique(dy))) %>% filter(n >=28)  #filtered out 395
id_mo_complete <- deer1$id_yr_mo #2164
deer <- deer %>% filter(id_yr_mo %in% id_mo_complete)

length(unique(deer$animalID)) #96
length(unique(deer$id_yr_mo)) #2164

# animal info 
ids <- unique(deer$animalID)
deer.info <- read_csv("./data/01CleanedMovement/deer_Info_All.csv") %>% filter(animalID %in% ids) 
rm(ids, deer1)

############################################################
####extract movement metrics################################
############################################################

# # ############# ############# #############
# # ## yearly movement metrics ##############
# # ### only for references #################
# # ############# ############# #############
# deer.y.trk <- deer %>% mutate(id_yr = paste0(animalID, "-", yr)) %>%
#   make_track(easting, northing, date, id = id_yr) %>% 
#   nest(data = -'id')
# 
# deer.y.step <- deer.y.trk %>%
#   mutate(steps = 
#            map(data, function(x) 
#              x %>% track_resample(rate = minutes(120), tolerance = minutes(5)) %>% steps_by_burst()),
#          nsd = 
#            map(data, function(x) x%>%nsd()))
# 
# #calculate yearly accumulated step length
# step_sum <- deer.y.step %>% unnest(cols = steps) %>%
#   group_by(id) %>% summarise(total_step_lengths = sum(sl_))
# 
# #calculate yearly max displacement
# nsd_sum <- deer.y.step %>% unnest_longer(nsd) %>% 
#   group_by(id) %>% summarise(max_displacement = sqrt(max(nsd)))
# 
# # merge with animal info
# # pick the ones that has at least 10 months in a year
# deer.y <- deer %>% select(animalID, yr, mo) %>% 
#   group_by(animalID, yr) %>% distinct() %>% 
#   summarize (n = n()) %>% filter (n >= 10) %>%
#   mutate(id_yr = paste0(animalID, "-", yr)) %>% select(id_yr)
# 
# deer.sum <- step_sum %>% left_join(nsd_sum) %>% separate (id, c("id", "yr"), sep = "-")
# deer.sum <- deer.info %>% dplyr::select(animalID, captureArea) %>% 
#   rename (id = animalID) %>% mutate(id = as.character(id)) %>%
#   left_join(deer.sum) %>% 
#   mutate(id_yr = paste0(id, "-", yr)) 
# deer.yr.sum <- deer.y %>% left_join(deer.sum)
# #write_csv(deer.yr.sum, "./result/deer_df_yearly.csv")


############# ############# #############
## monthly movement metrics #############
############# ############# #############

## make movement tracks for each animal_month
deer.trk <- deer %>% 
  make_track(easting, northing, date, id = id_yr_mo) %>% 
  nest(data = -'id')

# make steps and calculate nsd for each step
deer.step <- deer.trk %>%
  mutate(steps = 
           map(data, function(x) 
             x %>% track_resample(rate = minutes(120), tolerance = minutes(5)) %>% steps_by_burst()),
         # Defined as the ratio between total movement and the square root of the area of movement (LORETTO & VIEIRA 2005), 
         # it is proportional to the active time spent per unit area, which should increase with tortuosity of the path. (Almeida et al 2010)
         intensity_use = map(data, function(x)
           x %>% intensity_use()), 
         nsd = 
           map(data, function(x)   x%>%nsd()))

#calculate monthly accumulated step length
step_sum <- deer.step %>% unnest(cols = steps) %>%
  group_by(id) %>% summarise(total_step_lengths = sum(sl_))

#calculate monthly max displacement
nsd_sum <- deer.step %>% unnest_longer(nsd) %>% 
  group_by(id) %>% summarise(max_displacement = sqrt(max(nsd)))

#unlist intensity use column
intensity_use <- deer.step %>% dplyr::select(id, intensity_use) %>% unnest_longer(intensity_use)

# merge with animal info
deer.sum <- step_sum %>% left_join(nsd_sum) %>% left_join(intensity_use) %>% separate (id, c("id", "yr", "mo"), sep = "-")
deer.sum <- deer.info %>% dplyr::select(animalID, captureArea) %>% 
  rename (id = animalID) %>% mutate(id = as.character(id)) %>% left_join(deer.sum) %>% 
  mutate(
    rel_str = max_displacement/total_step_lengths,  # it is not exactly straightness between straightness use the start and the end to calculate displacement but we are using max displacement
    id_yr_mo = paste0(id, "-", yr, "-", mo)) 

# deer.sum <- deer.sum %>% mutate(id_yr_mo = paste0(id, "-", yr, "-", mo)) %>% 
#   left_join(deer %>% dplyr::select(id_yr_mo))
rm(step_sum, nsd_sum, intensity_use)

######################################################
########  add baba pca info ##########################
######################################################

# combine dataframes
deer.sum <- read_csv("./result/BaBA/BaBA_all_pca.csv") %>%
  right_join(deer.sum, by = c("id_yr_mo")) %>% 
  mutate(mo = as.double(mo))  %>%
  arrange(id, mo) # 2004

deer.sum <- read_csv("./result/BaBA/BaBA_deer_d90max_count.csv") %>%
  right_join(deer.sum, by = c("id_yr_mo"))

##############################################
####add env covariate: Local fence density####
##############################################
library(sf)
library(BBMM)
library(raster)
target.crs <- "+init=epsg:32612"
fence <- read_sf("./data/Fence_july2021__fieldupdated.shp")

# funtion that extract line density in a polygon
get_density <- function(polygon, line) {
  
  require(tidyverse)
  require(lubridate)
  require(sf)
  
  options(warn=-1)
  
  polygon <- st_make_valid(polygon)
  subline <- line %>% sf::st_intersection(., polygon) %>%
    dplyr::mutate(length_line = st_length(.),
                  length_line = ifelse(is.na(length_line), 0, length_line))
  tol.length <- sum(subline$length_line)
  
  density <-  tol.length/st_area(polygon)
  return(density)
}

# calculate time differences - prep data for BBMM
deer <- deer %>% group_by(id_yr_mo) %>% 
  mutate(time.lag = (date - lag(date, default = date[1]))/60) %>%
  filter(time.lag != 0)

# for each animal-month, run bbmm and get line density within the 99% BBMM
hr_fence_density <- data.frame() # create an empty list for density
for (i in unique(deer$id_yr_mo)[c(1: 1424, 1426:length(unique(deer$id_yr_mo)))]){
  
  animal.i <- deer %>% filter(id_yr_mo == i)
  
  cell.size = 100
  x = animal.i$easting
  y = animal.i$northing
  range.x <- range(x)
  range.y <- range(y)
  min.grid.x <- round(range.x[1] - 2 * sd(x))
  max.grid.x <- round(range.x[2] + 2 * sd(x))
  min.grid.y <- round(range.y[1] - 2 * sd(y))
  max.grid.y <- round(range.y[2] + 2 * sd(y))
  x. <- seq(min.grid.x, max.grid.x, cell.size)
  y. <- seq(min.grid.y, max.grid.y, cell.size)
  area.grid <- merge(x., y.)
  
  BBMM = brownian.bridge(x=x, y=y, 
                         time.lag=as.numeric(animal.i$time.lag),location.error=30, 
                         area.grid = area.grid, cell.size = cell.size)  
  
  # get 99% contour and turn it into polygon
  contours <- bbmm.contour(BBMM, levels=c(99), locations=locations, plot=FALSE)
  BBMM.df <-  data.frame(x=BBMM$x,y=BBMM$y,z=BBMM$probability)
  BBMM.raster <- rasterFromXYZ(BBMM.df, crs=target.crs, digits=2)
  raster.contour <- rasterToContour(BBMM.raster,levels=contours$Z)
  contour <- st_as_sf(raster.contour)
  if (st_geometry_type(st_as_sf(raster.contour)) == "MULTILINESTRING") {
    contour <- st_cast(contour, "MULTIPOLYGON") %>% dplyr::select(-level) %>% mutate (id_mo = i)
  } else {
    contour <- st_cast(contour, "POLYGON") %>%  dplyr::select(-level) %>% mutate (id_mo = i)
  }
  
  #get density 
  density <- get_density(contour, fence)
  HR.size <- st_area(st_make_valid(contour))
  
  hr_fence_density <- rbind(hr_fence_density, data.frame(id_yr_mo = i, fence_density = density, HR_size = HR.size))
  print(i)
}

#write_csv(hr_fence_density, "./data/derived_data/deer_hr_fence_density.csv")

deer.sum <- hr_fence_density %>% right_join(deer.sum) %>% #2004
  filter(!is.na(fence_density)) %>% #2003 
  filter(!is.na(PC1), !is.na(PC2)) #1404

# write_csv(data.frame(deer.sum), "./result/deer_df_monthly.csv")

# # ##############################################
# # ############## intial visualization ##########
# # ##############################################
# deer.sum <- read_csv("./result/deer_df_monthly.csv")
# library(forcats)
# 
# ###### first visualize individual differences in BaB ##########
# deer.sum <- deer.sum %>% left_join((deer.sum %>% group_by(id) %>% 
#                                                 summarise(val1= mean(PC1),
#                                                           val2=mean(PC2))))
# deer.sum %>% 
# #  filter(total_encounter >= 10) %>%
#   mutate(id = as.character(id),
#          id = fct_reorder(id,val1)) %>%
#   ggplot (aes(x = id, y = PC1)) +
#   geom_point() +
#   geom_smooth() +
#   theme_minimal()
# 
# deer.sum %>% 
#   mutate(id = as.character(id),
#          id = fct_reorder(id,val2)) %>%
#   ggplot (aes(x = id, y = PC2)) +
#   geom_point() +
#   geom_smooth() +
#   theme_minimal()
# 
# ###### visualize potential behavioral syndrome (monthly movement metrics) #########
# deer.sum %>%
#   ggplot (aes(x = PC2, y = log(max_displacement))) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_minimal() 
# 
# deer.sum %>%
#   ggplot (aes(x = PC2, y = log(intensity_use))) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_minimal() 
# 
# deer.sum %>%
#   ggplot (aes(x = PC1, y = log(total_step_lengths))) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_minimal() # more average movement, less total movement
# 
# deer.sum %>%
#   ggplot (aes(x = PC2, y = log(total_step_lengths))) + #smaller PC2 more altered movement, less quick cross
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_minimal()  # this one looks very prominant. which makes sense. 
# 
# deer.sum %>%
#   ggplot (aes(x = PC2, y = log(rel_str))) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_minimal() # better at crossing fence - more straight movement 
# 
# # ####### visualize potential behaviroal syndrom (yearly movement metrics) #######
# # deer.yr.sum <- read_csv( "./result/deer_df_yearly.csv")
# # deer.sum  <- deer.yr.sum %>% rename(yr_stp_l = total_step_lengths,
# #                                               yr_max_disp = max_displacement) %>%
# #   left_join(deer.sum, by = "id")
# # 
# # deer.sum %>%
# #   ggplot (aes(x = PC1, y = log(yr_stp_l))) +
# #   geom_point() +
# #   geom_smooth(method = "lm") +
# #   theme_minimal() # more average movement less total moving distance; 
# # 
# # deer.sum %>%
# #   ggplot (aes(x = PC2, y = log(yr_stp_l))) +
# #   geom_point() +
# #   geom_smooth(method = "lm") +
# #   theme_minimal() # more average movement less total moving distance; 
# # 
# # deer.sum %>%
# #   ggplot (aes(x = PC1, y = log(yr_max_disp))) +
# #   geom_point() +
# #   geom_smooth(method = "lm") +
# #   theme_minimal()
