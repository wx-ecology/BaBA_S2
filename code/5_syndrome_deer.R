### behavioral syndrome at lo, me, hi, and exhi fence density level using multivariate MMs

#################################################################
########################## set up ###############################
#################################################################
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2")
setwd("G:/My Drive/RESEARCH/Pronghorn/BaBA_Season2")

library(tidyverse)
library(data.table)
library(MCMCglmm)
library(coda)

deer <- read_csv("./result/deer_df_monthly.csv") %>% mutate(mo = as.numeric(mo), yr = as.numeric(yr)) %>% 
  dplyr::select(id_yr_mo, id, fence_density, mo, yr, PC1, PC2, total_step_lengths, HR_size)

deer.mig.status <- read_csv('./result/midproduct/manualmigstatus_deer.csv') %>% 
  pivot_longer(cols = 2:13, names_to = "month", values_to = "mig_status") %>%
  filter(!is.na(mig_status)) %>%
  mutate(id_yr_mo = paste0(id_yr, "-",month),
         mig_status = factor(mig_status)) 

deer.l <- deer %>%
  mutate(
    fence_density = fence_density*1000, #to bring to similar magnitute and also means km/km2, easier for interpreting
    sin_mo = sin(2*pi*mo/12), 
    cos_mo = cos(2*pi*mo/12),  #Stolwijk, A. M., H. M. P. M. Straatman, and G. A. Zielhuis. "Studying seasonality by using sine and cosine functions in regression analysis." Journal of Epidemiology & Community Health 53.4 (1999): 235-238.Stolwijk, A. M., H. M. P. M. Straatman, and G. A. Zielhuis. "Studying seasonality by using sine and cosine functions in regression analysis." Journal of Epidemiology & Community Health 53.4 (1999): 235-238.
    HR_size = ifelse (HR_size <1, 1, HR_size), 
    # intensity = log((total_step_lengths/HR_size)*1000),  # to bring to similar magnitude
    HR_size = log(HR_size),
    total_step_lengths = log(total_step_lengths)
  ) %>% 
  left_join(deer.mig.status, by = "id_yr_mo")

lo = 0.39
me = 0.68
hi = 1.00
exhi = 1.49

deer.l <- deer.l %>% mutate(fence_density_loc = fence_density - lo,
                            fence_density_mec = fence_density - me,
                            fence_density_hic = fence_density - hi,
                            fence_density_ehc = fence_density - exhi)

#################################################################
########################## models ###############################
#################################################################
# #parameter-expandedprior, which should be uninformative for the model (course note 8.0.2)
# prior G structure is for random variable error. the dimension is determined by the number of response variables
# prior R is for residual variance and a list with elements V and nu specifying the prior for the variance

# prior_03 = list(R = list(V = diag(4), nu = 0.002),
#                 G = list(G1 = list(V = diag(8), nu = 8, alpha.mu = rep(0,8), #prior means
#                                    alpha.V = diag(25^2,8,8)))) #prior covariance matrix
# 
# mcmc_lo <- MCMCglmm (cbind(PC1,
#                            PC2,
#                            total_step_lengths,
#                            HR_size) ~ trait-1 + # trait -1 tells the model to estimate distinct intercept for each trait
#                        trait:mig_status +
#                        trait:cos_mo +
#                        trait:sin_mo +
#                        trait:fence_density_loc,
#                      random = ~ us(trait + fence_density_loc:trait):id,
#                      rcov = ~ us(trait):units,
#                      family = c("gaussian", "gaussian","gaussian","gaussian"),
#                      prior = prior_03,
#                      nitt = 840000, thin = 100, burnin = 40000,
#                      verbose = TRUE,
#                      data = as.data.frame(deer.l))
# 
# saveRDS (mcmc_lo, "./result/models/mcmc_syndrome_deer_loc.RDS")
# 
# mcmc_me <- MCMCglmm (cbind(PC1,
#                            PC2,
#                            total_step_lengths,
#                            HR_size) ~ trait-1 + # trait -1 tells the model to estimate distinct intercept for each trait
#                        trait:mig_status +
#                        trait:cos_mo +
#                        trait:sin_mo +
#                        trait:fence_density_mec,
#                      random = ~ us(trait + fence_density_mec:trait):id,
#                      rcov = ~ us(trait):units,
#                      family = c("gaussian", "gaussian","gaussian","gaussian"),
#                      prior = prior_03,
#                      nitt = 840000, thin = 100, burnin = 40000,
#                      verbose = TRUE,
#                      data = as.data.frame(deer.l))
# 
# saveRDS (mcmc_me, "./result/models/mcmc_syndrome_deer_mec.RDS")
# 
# mcmc_hi <- MCMCglmm (cbind(PC1,
#                            PC2,
#                            total_step_lengths,
#                            HR_size) ~ trait-1 + # trait -1 tells the model to estimate distinct intercept for each trait
#                        trait:mig_status +
#                        trait:cos_mo +
#                        trait:sin_mo +
#                        trait:fence_density_hic,
#                      random = ~ us(trait + fence_density_hic:trait):id,
#                      rcov = ~ us(trait):units,
#                      family = c("gaussian", "gaussian","gaussian","gaussian"),
#                      prior = prior_03,
#                      nitt = 840000, thin = 100, burnin = 40000,
#                      verbose = TRUE,
#                      data = as.data.frame(deer.l))
# 
# saveRDS (mcmc_hi, "./result/models/mcmc_syndrome_deer_hic.RDS")
# 
# mcmc_exhi <- MCMCglmm (cbind(PC1,
#                              PC2,
#                              total_step_lengths,
#                              HR_size) ~ trait-1 + # trait -1 tells the model to estimate distinct intercept for each trait
#                          trait:mig_status +
#                          trait:cos_mo +
#                          trait:sin_mo +
#                          trait:fence_density_ehc,
#                        random = ~ us(trait + fence_density_ehc:trait):id,
#                        rcov = ~ us(trait):units,
#                        family = c("gaussian", "gaussian","gaussian","gaussian"),
#                        prior = prior_03,
#                        nitt = 840000, thin = 100, burnin = 40000,
#                        verbose = TRUE,
#                        data = as.data.frame(deer.l))
# 
# saveRDS (mcmc_exhi, "./result/models/mcmc_syndrome_deer_ehc.RDS")

############################################################
####### SYNDROME ###########################################
############################################################

mcmc_lo <- readRDS("./result/models/mcmc_syndrome_deer_loc.RDS")
mcmc_me <- readRDS("./result/models/mcmc_syndrome_deer_mec.RDS")
mcmc_hi <- readRDS("./result/models/mcmc_syndrome_deer_hic.RDS")
mcmc_exhi <- readRDS("./result/models/mcmc_syndrome_deer_ehc.RDS")

cmc_cor_PC1_steplength <- mcmc_lo$VCV[,"traitPC1:traittotal_step_lengths.id"]/
  (sqrt(mcmc_lo$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_lo$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_steplength)

mcmc_cor_PC1_HR <- mcmc_lo$VCV[,"traitPC1:traitHR_size.id"]/
  (sqrt(mcmc_lo$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_lo$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_HR)

mcmc_cor_PC2_steplength <- mcmc_lo$VCV[,"traitPC2:traittotal_step_lengths.id"]/
  (sqrt(mcmc_lo$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_lo$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_steplength)

mcmc_cor_PC2_HR <- mcmc_lo$VCV[,"traitPC2:traitHR_size.id"]/
  (sqrt(mcmc_lo$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_lo$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_HR)

mcmc_cor_lo <- tibble(Traits = c("PC1, total distance",
                                 "PC1, range size",
                                 "PC2, total distance",
                                 "PC2, range size"),
                      Estimate = c(median(mcmc_cor_PC1_steplength),median(mcmc_cor_PC1_HR),
                                   median(mcmc_cor_PC2_steplength), median(mcmc_cor_PC2_HR)),
                      Lower = c(HPDinterval(mcmc_cor_PC1_steplength)[,"lower"],HPDinterval(mcmc_cor_PC1_HR)[,"lower"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"lower"],HPDinterval(mcmc_cor_PC2_HR)[,"lower"]),
                      Upper = c(HPDinterval(mcmc_cor_PC1_steplength)[,"upper"],HPDinterval(mcmc_cor_PC1_HR)[,"upper"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"upper"],HPDinterval(mcmc_cor_PC2_HR)[,"upper"]))

mcmc_cor_PC1_steplength <- mcmc_me$VCV[,"traitPC1:traittotal_step_lengths.id"]/
  (sqrt(mcmc_me$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_me$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_steplength)

mcmc_cor_PC1_HR <- mcmc_me$VCV[,"traitPC1:traitHR_size.id"]/
  (sqrt(mcmc_me$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_me$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_HR)

mcmc_cor_PC2_steplength <- mcmc_me$VCV[,"traitPC2:traittotal_step_lengths.id"]/
  (sqrt(mcmc_me$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_me$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_steplength)

mcmc_cor_PC2_HR <- mcmc_me$VCV[,"traitPC2:traitHR_size.id"]/
  (sqrt(mcmc_me$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_me$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_HR)

mcmc_cor_me <- tibble(Traits = c("PC1, total distance",
                                 "PC1, range size",
                                 "PC2, total distance",
                                 "PC2, range size"),
                      Estimate = c(median(mcmc_cor_PC1_steplength),median(mcmc_cor_PC1_HR),
                                   median(mcmc_cor_PC2_steplength), median(mcmc_cor_PC2_HR)),
                      Lower = c(HPDinterval(mcmc_cor_PC1_steplength)[,"lower"],HPDinterval(mcmc_cor_PC1_HR)[,"lower"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"lower"],HPDinterval(mcmc_cor_PC2_HR)[,"lower"] ),
                      Upper = c(HPDinterval(mcmc_cor_PC1_steplength)[,"upper"],HPDinterval(mcmc_cor_PC1_HR)[,"upper"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"upper"],HPDinterval(mcmc_cor_PC2_HR)[,"upper"]))


mcmc_cor_PC1_steplength <- mcmc_hi$VCV[,"traitPC1:traittotal_step_lengths.id"]/
  (sqrt(mcmc_hi$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_hi$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_steplength)

mcmc_cor_PC1_HR <- mcmc_hi$VCV[,"traitPC1:traitHR_size.id"]/
  (sqrt(mcmc_hi$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_hi$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_HR)

mcmc_cor_PC2_steplength <- mcmc_hi$VCV[,"traitPC2:traittotal_step_lengths.id"]/
  (sqrt(mcmc_hi$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_hi$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_steplength)

mcmc_cor_PC2_HR <- mcmc_hi$VCV[,"traitPC2:traitHR_size.id"]/
  (sqrt(mcmc_hi$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_hi$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_HR)

mcmc_cor_hi <- tibble(Traits = c("PC1, total distance",
                                 "PC1, range size",
                                 "PC2, total distance",
                                 "PC2, range size"),
                      Estimate = c(median(mcmc_cor_PC1_steplength),median(mcmc_cor_PC1_HR),
                                   median(mcmc_cor_PC2_steplength), median(mcmc_cor_PC2_HR)),
                      Lower = c(HPDinterval(mcmc_cor_PC1_steplength)[,"lower"],HPDinterval(mcmc_cor_PC1_HR)[,"lower"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"lower"],HPDinterval(mcmc_cor_PC2_HR)[,"lower"]),
                      Upper = c(HPDinterval(mcmc_cor_PC1_steplength)[,"upper"],HPDinterval(mcmc_cor_PC1_HR)[,"upper"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"upper"],HPDinterval(mcmc_cor_PC2_HR)[,"upper"]))


mcmc_cor_PC1_steplength <- mcmc_exhi$VCV[,"traitPC1:traittotal_step_lengths.id"]/
  (sqrt(mcmc_exhi$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_exhi$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_steplength)

mcmc_cor_PC1_HR <- mcmc_exhi$VCV[,"traitPC1:traitHR_size.id"]/
  (sqrt(mcmc_exhi$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_exhi$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_HR)

mcmc_cor_PC2_steplength <- mcmc_exhi$VCV[,"traitPC2:traittotal_step_lengths.id"]/
  (sqrt(mcmc_exhi$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_exhi$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_steplength)

mcmc_cor_PC2_HR <- mcmc_exhi$VCV[,"traitPC2:traitHR_size.id"]/
  (sqrt(mcmc_exhi$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_exhi$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_HR)

mcmc_cor_exhi <- tibble(Traits = c("PC1, total distance",
                                   "PC1, range size",
                                   "PC2, total distance",
                                   "PC2, range size"),
                        Estimate = c(median(mcmc_cor_PC1_steplength),median(mcmc_cor_PC1_HR),
                                     median(mcmc_cor_PC2_steplength), median(mcmc_cor_PC2_HR)),
                        Lower = c(HPDinterval(mcmc_cor_PC1_steplength)[,"lower"],HPDinterval(mcmc_cor_PC1_HR)[,"lower"],
                                  HPDinterval(mcmc_cor_PC2_steplength)[,"lower"],HPDinterval(mcmc_cor_PC2_HR)[,"lower"]),
                        Upper = c(HPDinterval(mcmc_cor_PC1_steplength)[,"upper"],HPDinterval(mcmc_cor_PC1_HR)[,"upper"],
                                  HPDinterval(mcmc_cor_PC2_steplength)[,"upper"],HPDinterval(mcmc_cor_PC2_HR)[,"upper"]))

mcmc_cor_deer <- bind_rows(mcmc_cor_lo %>% mutate (mod = "lo"),
                           mcmc_cor_me %>% mutate (mod = "me"),
                           mcmc_cor_hi %>% mutate (mod = "hi"),
                           mcmc_cor_exhi %>% mutate (mod = "exhi"))
write_csv(mcmc_cor_deer, "./result/midproduct/behaviorsyndrome_deer.csv")