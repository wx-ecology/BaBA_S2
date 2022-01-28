## run barrier behavior analysis for both pronghorn and mule deer 
setwd("G:/My Drive/RESEARCH/Pronghorn/BaBA_Season2/")
target.crs <- "+init=epsg:32612"

library(tidyverse)
library(lubridate)
library(BaBA)
source("./code/BaBA_QC.r") # this modified baba only pick up quick cross for validating optimal fence buffer distance

#######################################################################################################################################################
############   pronghorn baba  ######################################################################################################################
#######################################################################################################################################################

pronghorn <- read_csv("./data/pronghorn_2h_pts.csv") %>% mutate(date = ymd_hms(date, tz = "US/Mountain")) %>% filter(!is.na(date))
pronghorn.sp <- SpatialPointsDataFrame(coords = cbind(pronghorn$Easting, pronghorn$Northing), data = pronghorn[,c("Animal.ID", "date", "Capture.Area")], proj4string = CRS(target.crs))

fence.sp <- readOGR("./data/Fence_july2021__fieldupdated.shp")
fence.sp <- spTransform(fence.sp, CRS(target.crs))

# get brief BaBA (identify Quick Corss only) result.
for (d in c(50,60,70,80,91,100,110,120,130,140,150)) {
  BaBA.d <- BaBA_QC(animal = pronghorn.sp, barrier = fence.sp, d = d, interval =2, units = "hours")
  writeOGR(BaBA.d$encounters, paste0("./result/BaBA_QC/BaBA_d", d, ".shp"), paste0("BaBA_d",d), driver = "ESRI Shapefile")
  write_csv(BaBA.d$classification, paste0("./result/BaBA_QC/BaBA_d",d, ".csv"))
}

# calculate # of Quick Cross across individuals at each buffer distance.
path <- paste0(getwd(), "/result/BaBA_QC/")
files <- dir(path = path, recursive = FALSE, pattern = "*\\.csv") #get all files in directory
QC_summary <- data.frame(NULL)
for (x in files) {
  d <- (x %>% str_split("_d") %>% unlist() %>% str_split("\\.") %>% unlist())[2]
  n <- paste0(path, x) %>%
    read_csv(.) %>%
    filter(eventTYPE == "Quick_Cross") %>%
    summarise( n = n()) %>% mutate (d = as.numeric(d))
  QC_summary <- rbind(QC_summary, n)
}

d_max <- QC_summary %>% filter(n == max(n)) %>% select(d)
# 110 

######## Run full baba based on the optimal buffer distance ###############
BaBA.d <- BaBA(animal = pronghorn.sp, barrier = fence.sp, d = d_max, interval =2, max_cross = 1, units = "hours")  
# writeOGR(BaBA.d$encounters, paste0("./result/BaBA/BaBA_d", d_max, ".shp"), paste0("BaBA_d",d_max), driver = "ESRI Shapefile")
# write_csv(BaBA.d$classification, paste0("./result/BaBA/BaBA_d",d_max, ".csv"))

#######################################################################################################################################################
############   repeat for deer   ######################################################################################################################
#######################################################################################################################################################
deer <- read_csv("./data/deer_2h_pts.csv") %>% mutate(date = ymd_hms(date, tz = "US/Mountain")) %>% filter(!is.na(date)) %>% rename(Animal.ID = animalID)
deer.sp <- SpatialPointsDataFrame(coords = cbind(deer$easting, deer$northing), data = deer[,c("Animal.ID", "date", "captureArea")], proj4string = CRS(target.crs))

fence.sp <- readOGR("./data/Fence_july2021__fieldupdated.shp")
fence.sp <- spTransform(fence.sp, CRS(target.crs))

###### brief BABA FOR DEER ########
for (d in c(50,60,70,80,90,100,110,120,130,140,150)) {
  BaBA.d <- BaBA_QC(animal = deer.sp, barrier = fence.sp, d = d, interval =2, units = "hours")  
  writeOGR(BaBA.d$encounters, paste0("./result/BaBA_QC/BaBA_deer_d", d, ".shp"), paste0("BaBA_deer_d",d), driver = "ESRI Shapefile")
  write_csv(BaBA.d$classification, paste0("./result/BaBA_QC/BaBA_deer_d",d, ".csv"))
}

# calculate # of Quick Cross across individuals at each buffer distance.
path <- paste0(getwd(), "/result/BaBA_QC/")
files <- dir(path = path, recursive = FALSE, pattern = "\\.csv") #get all files in directory
files <- files[grep("^BaBA_deer", files)]
QC_summary <- data.frame(NULL)
for (x in files) {
  d <- (x %>% str_split("_d") %>% unlist() %>% str_split("\\.") %>% unlist())[3]
  n <- paste0(path, x) %>%
    read_csv(.) %>%
    filter(eventTYPE == "Quick_Cross") %>%
    summarise( n = n()) %>% mutate (d = as.numeric(d))
  QC_summary <- rbind(QC_summary, n)
}

d_max <- QC_summary %>% filter(n == max(n)) %>% select(d)
# 90 

BaBA.d <- BaBA(animal = deer.sp, barrier = fence.sp, d = d_max, interval =2, max_cross = 1, units = "hours")  
#writeOGR(BaBA.d$encounters, paste0("G:/My Drive/RESEARCH/Pronghorn/BaBA_Season2/result/BaBA/BaBA_deer_d90max.shp"), paste0("BaBA_deer_d90"), driver = "ESRI Shapefile")
#write_csv(BaBA.d$classification, paste0("./result/BaBA/BaBA_deer_d",d_max, "max.csv"))

#######################################################################################################################################################
############### PCA analysis on BaBA result ###########################################################################################################
#######################################################################################################################################################
library(PLNmodels)
library(factoextra)
## first organize dataframe ######
deer.baba <- read_csv("./result/BaBA/BaBA_deer_d90max.csv") %>% 
  dplyr::select(AnimalID, start_time, eventTYPE) %>%
  mutate(mo = month(start_time),
         yr = year(start_time)) %>% 
  dplyr::rename(id = AnimalID) %>% 
  mutate(id_yr_mo = paste0(id,"-",yr, "-", mo)) %>%
  group_by(id_yr_mo, eventTYPE) %>% 
  dplyr::summarise( n = n()) %>% #summarize # of event each month
  pivot_wider(names_from = eventTYPE, values_from = n, values_fill = 0) %>% 
  mutate (total_encounter = 
            (Average_Movement + Bounce + Quick_Cross + unknown + Back_n_forth + Trace + Trapped), 
          Altered_Movement = Bounce + Back_n_forth + Trace + Trapped) %>%
  dplyr::select(id_yr_mo, total_encounter, Altered_Movement, Average_Movement, Quick_Cross) %>%
  filter(Average_Movement + Altered_Movement + Quick_Cross != 0) # remote rows with all counts = 0 (cannot be handelled by PCA)

pronghorn.baba <- read_csv("./result/BaBA/BaBA_d110max.csv") %>%
  dplyr::select(AnimalID, start_time, eventTYPE) %>%
  mutate(mo = month(start_time),
         yr = year(start_time)) %>%
  dplyr::rename(id = AnimalID) %>%
  mutate(id_yr_mo = paste0(id,"-",yr, "-", mo)) %>%
  group_by(id_yr_mo, eventTYPE) %>%
  dplyr::summarise( n = n()) %>% #summarize # of event each month
  pivot_wider(names_from = eventTYPE, values_from = n, values_fill = 0) %>%
  mutate (total_encounter =
            (Average_Movement + Bounce + Quick_Cross + unknown + Back_n_forth + Trace + Trapped),
          Altered_Movement = Bounce + Back_n_forth + Trace + Trapped) %>%
  dplyr::select(id_yr_mo, total_encounter, Altered_Movement, Average_Movement, Quick_Cross) %>%
  filter(Average_Movement + Altered_Movement + Quick_Cross != 0) # remote rows with all counts = 0 (cannot be handelled by PCA)

# combine two species results
all.baba <- rbind(pronghorn.baba %>% mutate (spp = "pronghorn"), (deer.baba %>% mutate (spp = "deer")))
all.baba0 <- prepare_data(counts = all.baba[, 3:5], covariates = all.baba[,c(1:2,6)])

# conduct PLNPCA
baba_plnpca<- PLNPCA(
  Abundance ~ 1 + offset(log(Offset)), #offset is the total count
  data  = all.baba0, 
  ranks = 1:3 # number of axis to be considered. Because we only have 3 to start with, so test 1-3
)

myPCA_BIC <- getModel(baba_plnpca, 2)
plot(myPCA_BIC)
# following https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2656.2007.01284.x
# Principal component analysis (PCA) followed by varimax rotation (Tabachnick & Fidell 2001) was used to summarize the behavioural measures for each type of test (Table 2). I

rawLoadings <- myPCA_BIC$rotation
rotated_loadings <- varimax(rawLoadings)$loadings
# Scores computed via rotating the scores
rotated_scores <- scale(myPCA_BIC$scores) %*% varimax(rawLoadings)$rotmat
# pca dataframe
all_pca <- data.frame(spp = all.baba$spp, id_yr_mo = all.baba$id_yr_mo, PC1 = rotated_scores[,1], PC2 = rotated_scores[,2]) 
# write_csv(all_pca, "./result/BaBA/BaBA_all_pca.csv")
# saveRDS(rotated_loadings,"./result/BaBA/BaBA_all_loadings.RDS" )

factoextra::fviz_pca_biplot(myPCA_BIC, label = "none", habillage = all.baba$spp)
