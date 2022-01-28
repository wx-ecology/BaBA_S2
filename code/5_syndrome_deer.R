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

pronghorn <- read_csv("./result/prong_df_monthly.csv") %>% mutate(mo = as.numeric(mo), yr = as.numeric(yr)) %>% filter(fence_density != 0) %>% 
  dplyr::select(id_yr_mo, id, spp, fence_density, mo, yr, PC1, PC2, total_step_lengths, HR_size, rel_str)
deer <- read_csv("./result/deer_df_monthly.csv") %>% mutate(mo = as.numeric(mo), yr = as.numeric(yr)) %>% 
  dplyr::select(id_yr_mo, id, spp,fence_density, mo, yr, PC1, PC2, total_step_lengths, HR_size, rel_str)

animal.l <- rbind(pronghorn, deer) %>%
  mutate(
    fence_density = fence_density*1000, #to bring to similar magnitute and also means km/km2, easier for interpreting
    mo = mo - 1, # so that when calculating intercept, x = 0 makes sense. Otherwise m0 can never be 0 in reality. 
    cos_mo = cos(mo),
    sin_mo = sin(mo),
    HR_size = log(HR_size),
    total_step_lengths = log(total_step_lengths),
    rel_str = log(rel_str)
  )

lo = round(quantile(animal.l$fence_density, .25),2) #0.39
me = round(quantile(animal.l$fence_density, .5), 2) #0.68
hi = round(quantile(animal.l$fence_density, .75), 2) #1.00
exhi = round(quantile(animal.l$fence_density, .90), 2) #1.49

animal.l <- animal.l %>% mutate(fence_density_loc = fence_density - lo,
                                fence_density_mec = fence_density - me,
                                fence_density_hic = fence_density - hi,
                                fence_density_ehc = fence_density - exhi)

# conduct transformation to normalize data
deer.l <- animal.l %>% filter(spp == "deer")

rm(animal.l, pronghorn, deer)

# #################################################################
# ########################## models ###############################
# #################################################################
# # #parameter-expandedprior, which should be uninformative for the model (course note 8.0.2)
# # prior G structure is for random variable error. the dimension is determined by the number of response variables
# # prior R is for residual variance and a list with elements V and nu specifying the prior for the variance
# 
# prior_03 = list(R = list(V = diag(5), nu = 0.002),
#                G = list(G1 = list(V = diag(10), nu = 10, alpha.mu = rep(0,10), #prior means
#                                   alpha.V = diag(25^2,10,10)))) #prior covariance matrix
# 
# mcmc_lo <- MCMCglmm (cbind(PC1,
#                            PC2,
#                            total_step_lengths,
#                            HR_size,
#                            rel_str) ~ trait-1 + # trait -1 tells the model to estimate distinct intercept for each trait
#                        trait:mo +
#                        trait:cos_mo +
#                        trait:sin_mo +
#                        trait:fence_density_loc,
#                      random = ~ us(trait + fence_density_loc:trait):id,
#                      rcov = ~ us(trait):units,
#                      family = c("gaussian", "gaussian","gaussian","gaussian","gaussian"),
#                      prior = prior_03,
#                      nitt = 840000, thin = 100, burnin = 40000,
#                      verbose = TRUE,
#                      data = as.data.frame(deer.l))
# 
# saveRDS (mcmc_lo, "./result/mcmc_syndrome_deer_loc.RDS")
# 
# mcmc_me <- MCMCglmm (cbind(PC1,
#                            PC2,
#                            total_step_lengths,
#                            HR_size,
#                            rel_str) ~ trait-1 + # trait -1 tells the model to estimate distinct intercept for each trait
#                        trait:mo +
#                        trait:cos_mo +
#                        trait:sin_mo +
#                        trait:fence_density_mec,
#                      random = ~ us(trait + fence_density_mec:trait):id,
#                      rcov = ~ us(trait):units,
#                      family = c("gaussian", "gaussian","gaussian","gaussian","gaussian"),
#                      prior = prior_03,
#                      nitt = 840000, thin = 100, burnin = 40000,
#                      verbose = TRUE,
#                      data = as.data.frame(deer.l))
# 
# saveRDS (mcmc_me, "./result/mcmc_syndrome_deer_mec.RDS")
# 
# mcmc_hi <- MCMCglmm (cbind(PC1,
#                            PC2,
#                            total_step_lengths,
#                            HR_size,
#                            rel_str) ~ trait-1 + # trait -1 tells the model to estimate distinct intercept for each trait
#                        trait:mo +
#                        trait:cos_mo +
#                        trait:sin_mo +
#                        trait:fence_density_hic,
#                      random = ~ us(trait + fence_density_hic:trait):id,
#                      rcov = ~ us(trait):units,
#                      family = c("gaussian", "gaussian","gaussian","gaussian","gaussian"),
#                      prior = prior_03,
#                      nitt = 840000, thin = 100, burnin = 40000,
#                      verbose = TRUE,
#                      data = as.data.frame(deer.l))
# 
# saveRDS (mcmc_hi, "./result/mcmc_syndrome_deer_hic.RDS")
# 
# mcmc_exhi <- MCMCglmm (cbind(PC1,
#                              PC2,
#                              total_step_lengths,
#                              HR_size,
#                              rel_str) ~ trait-1 + # trait -1 tells the model to estimate distinct intercept for each trait
#                          trait:mo +
#                          trait:cos_mo +
#                          trait:sin_mo +
#                          trait:fence_density_ehc,
#                        random = ~ us(trait + fence_density_ehc:trait):id,
#                        rcov = ~ us(trait):units,
#                        family = c("gaussian", "gaussian","gaussian","gaussian","gaussian"),
#                        prior = prior_03,
#                        nitt = 840000, thin = 100, burnin = 40000,
#                        verbose = TRUE,
#                        data = as.data.frame(deer.l))
# 
# saveRDS (mcmc_exhi, "./result/mcmc_syndrome_deer_ehc.RDS")

############################################################
####### SYNDROME ###########################################
############################################################

mcmc_lo <- readRDS("./result/mcmc_syndrome_deer_loc.RDS")
mcmc_me <- readRDS("./result/mcmc_syndrome_deer_mec.RDS")
mcmc_hi <- readRDS("./result/mcmc_syndrome_deer_hic.RDS")
mcmc_exhi <- readRDS("./result/mcmc_syndrome_deer_ehc.RDS")

mcmc_cor_PC1_steplength <- mcmc_lo$VCV[,"traitPC1:traittotal_step_lengths.id"]/
  (sqrt(mcmc_lo$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_lo$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_steplength)

mcmc_cor_PC1_HR <- mcmc_lo$VCV[,"traitPC1:traitHR_size.id"]/
  (sqrt(mcmc_lo$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_lo$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_HR)

mcmc_cor_PC1_str <- mcmc_lo$VCV[,"traitPC1:traitrel_str.id"]/
  (sqrt(mcmc_lo$VCV[,"traitrel_str:traitrel_str.id"]) * 
     sqrt(mcmc_lo$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_str)

mcmc_cor_PC2_steplength <- mcmc_lo$VCV[,"traitPC2:traittotal_step_lengths.id"]/
  (sqrt(mcmc_lo$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_lo$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_steplength)

mcmc_cor_PC2_HR <- mcmc_lo$VCV[,"traitPC2:traitHR_size.id"]/
  (sqrt(mcmc_lo$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_lo$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_HR)

mcmc_cor_PC2_str <- mcmc_lo$VCV[,"traitPC2:traitrel_str.id"]/
  (sqrt(mcmc_lo$VCV[,"traitrel_str:traitrel_str.id"]) * 
     sqrt(mcmc_lo$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_str)

mcmc_cor_lo <- tibble(Traits = c("PC1, total distance",
                                 "PC1, range size",
                                 "PC1, straightness",
                                 "PC2, total distance",
                                 "PC2, range size",
                                 "PC2, straightness"),
                      Estimate = c(median(mcmc_cor_PC1_steplength),median(mcmc_cor_PC1_HR),
                                   median(mcmc_cor_PC1_str),
                                   median(mcmc_cor_PC2_steplength), median(mcmc_cor_PC2_HR),
                                   median(mcmc_cor_PC2_str) ),
                      Lower = c(HPDinterval(mcmc_cor_PC1_steplength)[,"lower"],HPDinterval(mcmc_cor_PC1_HR)[,"lower"],
                                HPDinterval(mcmc_cor_PC1_str)[,"lower"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"lower"],HPDinterval(mcmc_cor_PC2_HR)[,"lower"],
                                HPDinterval(mcmc_cor_PC2_str)[,"lower"]),
                      Upper = c(HPDinterval(mcmc_cor_PC1_steplength)[,"upper"],HPDinterval(mcmc_cor_PC1_HR)[,"upper"],
                                HPDinterval(mcmc_cor_PC1_str)[,"upper"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"upper"],HPDinterval(mcmc_cor_PC2_HR)[,"upper"],
                                HPDinterval(mcmc_cor_PC2_str)[,"upper"]))

mcmc_cor_PC1_steplength <- mcmc_me$VCV[,"traitPC1:traittotal_step_lengths.id"]/
  (sqrt(mcmc_me$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_me$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_steplength)

mcmc_cor_PC1_HR <- mcmc_me$VCV[,"traitPC1:traitHR_size.id"]/
  (sqrt(mcmc_me$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_me$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_HR)

mcmc_cor_PC1_str <- mcmc_me$VCV[,"traitPC1:traitrel_str.id"]/
  (sqrt(mcmc_me$VCV[,"traitrel_str:traitrel_str.id"]) * 
     sqrt(mcmc_me$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_str)

mcmc_cor_PC2_steplength <- mcmc_me$VCV[,"traitPC2:traittotal_step_lengths.id"]/
  (sqrt(mcmc_me$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_me$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_steplength)

mcmc_cor_PC2_HR <- mcmc_me$VCV[,"traitPC2:traitHR_size.id"]/
  (sqrt(mcmc_me$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_me$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_HR)

mcmc_cor_PC2_str <- mcmc_me$VCV[,"traitPC2:traitrel_str.id"]/
  (sqrt(mcmc_me$VCV[,"traitrel_str:traitrel_str.id"]) * 
     sqrt(mcmc_me$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_str)

mcmc_cor_me <- tibble(Traits = c("PC1, total distance",
                                 "PC1, range size",
                                 "PC1, straightness",
                                 "PC2, total distance",
                                 "PC2, range size",
                                 "PC2, straightness"),
                      Estimate = c(median(mcmc_cor_PC1_steplength),median(mcmc_cor_PC1_HR),
                                   median(mcmc_cor_PC1_str),
                                   median(mcmc_cor_PC2_steplength), median(mcmc_cor_PC2_HR),
                                   median(mcmc_cor_PC2_str) ),
                      Lower = c(HPDinterval(mcmc_cor_PC1_steplength)[,"lower"],HPDinterval(mcmc_cor_PC1_HR)[,"lower"],
                                HPDinterval(mcmc_cor_PC1_str)[,"lower"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"lower"],HPDinterval(mcmc_cor_PC2_HR)[,"lower"],
                                HPDinterval(mcmc_cor_PC2_str)[,"lower"]),
                      Upper = c(HPDinterval(mcmc_cor_PC1_steplength)[,"upper"],HPDinterval(mcmc_cor_PC1_HR)[,"upper"],
                                HPDinterval(mcmc_cor_PC1_str)[,"upper"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"upper"],HPDinterval(mcmc_cor_PC2_HR)[,"upper"],
                                HPDinterval(mcmc_cor_PC2_str)[,"upper"]))


mcmc_cor_PC1_steplength <- mcmc_hi$VCV[,"traitPC1:traittotal_step_lengths.id"]/
  (sqrt(mcmc_hi$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_hi$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_steplength)

mcmc_cor_PC1_HR <- mcmc_hi$VCV[,"traitPC1:traitHR_size.id"]/
  (sqrt(mcmc_hi$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_hi$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_HR)

mcmc_cor_PC1_str <- mcmc_hi$VCV[,"traitPC1:traitrel_str.id"]/
  (sqrt(mcmc_hi$VCV[,"traitrel_str:traitrel_str.id"]) * 
     sqrt(mcmc_hi$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_str)

mcmc_cor_PC2_steplength <- mcmc_hi$VCV[,"traitPC2:traittotal_step_lengths.id"]/
  (sqrt(mcmc_hi$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_hi$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_steplength)

mcmc_cor_PC2_HR <- mcmc_hi$VCV[,"traitPC2:traitHR_size.id"]/
  (sqrt(mcmc_hi$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_hi$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_HR)

mcmc_cor_PC2_str <- mcmc_hi$VCV[,"traitPC2:traitrel_str.id"]/
  (sqrt(mcmc_hi$VCV[,"traitrel_str:traitrel_str.id"]) * 
     sqrt(mcmc_hi$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_str)

mcmc_cor_hi <- tibble(Traits = c("PC1, total distance",
                                 "PC1, range size",
                                 "PC1, straightness",
                                 "PC2, total distance",
                                 "PC2, range size",
                                 "PC2, straightness"),
                      Estimate = c(median(mcmc_cor_PC1_steplength),median(mcmc_cor_PC1_HR),
                                   median(mcmc_cor_PC1_str),
                                   median(mcmc_cor_PC2_steplength), median(mcmc_cor_PC2_HR),
                                   median(mcmc_cor_PC2_str) ),
                      Lower = c(HPDinterval(mcmc_cor_PC1_steplength)[,"lower"],HPDinterval(mcmc_cor_PC1_HR)[,"lower"],
                                HPDinterval(mcmc_cor_PC1_str)[,"lower"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"lower"],HPDinterval(mcmc_cor_PC2_HR)[,"lower"],
                                HPDinterval(mcmc_cor_PC2_str)[,"lower"]),
                      Upper = c(HPDinterval(mcmc_cor_PC1_steplength)[,"upper"],HPDinterval(mcmc_cor_PC1_HR)[,"upper"],
                                HPDinterval(mcmc_cor_PC1_str)[,"upper"],
                                HPDinterval(mcmc_cor_PC2_steplength)[,"upper"],HPDinterval(mcmc_cor_PC2_HR)[,"upper"],
                                HPDinterval(mcmc_cor_PC2_str)[,"upper"]))


mcmc_cor_PC1_steplength <- mcmc_exhi$VCV[,"traitPC1:traittotal_step_lengths.id"]/
  (sqrt(mcmc_exhi$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_exhi$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_steplength)

mcmc_cor_PC1_HR <- mcmc_exhi$VCV[,"traitPC1:traitHR_size.id"]/
  (sqrt(mcmc_exhi$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_exhi$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_HR)

mcmc_cor_PC1_str <- mcmc_exhi$VCV[,"traitPC1:traitrel_str.id"]/
  (sqrt(mcmc_exhi$VCV[,"traitrel_str:traitrel_str.id"]) * 
     sqrt(mcmc_exhi$VCV[,"traitPC1:traitPC1.id"]))
HPDinterval(mcmc_cor_PC1_str)

mcmc_cor_PC2_steplength <- mcmc_exhi$VCV[,"traitPC2:traittotal_step_lengths.id"]/
  (sqrt(mcmc_exhi$VCV[,"traittotal_step_lengths:traittotal_step_lengths.id"]) * 
     sqrt(mcmc_exhi$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_steplength)

mcmc_cor_PC2_HR <- mcmc_exhi$VCV[,"traitPC2:traitHR_size.id"]/
  (sqrt(mcmc_exhi$VCV[,"traitHR_size:traitHR_size.id"]) * 
     sqrt(mcmc_exhi$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_HR)

mcmc_cor_PC2_str <- mcmc_exhi$VCV[,"traitPC2:traitrel_str.id"]/
  (sqrt(mcmc_exhi$VCV[,"traitrel_str:traitrel_str.id"]) * 
     sqrt(mcmc_exhi$VCV[,"traitPC2:traitPC2.id"]))
HPDinterval(mcmc_cor_PC2_str)

mcmc_cor_exhi <- tibble(Traits = c("PC1, total distance",
                                   "PC1, range size",
                                   "PC1, straightness",
                                   "PC2, total distance",
                                   "PC2, range size",
                                   "PC2, straightness"),
                        Estimate = c(median(mcmc_cor_PC1_steplength),median(mcmc_cor_PC1_HR),
                                     median(mcmc_cor_PC1_str),
                                     median(mcmc_cor_PC2_steplength), median(mcmc_cor_PC2_HR),
                                     median(mcmc_cor_PC2_str) ),
                        Lower = c(HPDinterval(mcmc_cor_PC1_steplength)[,"lower"],HPDinterval(mcmc_cor_PC1_HR)[,"lower"],
                                  HPDinterval(mcmc_cor_PC1_str)[,"lower"],
                                  HPDinterval(mcmc_cor_PC2_steplength)[,"lower"],HPDinterval(mcmc_cor_PC2_HR)[,"lower"],
                                  HPDinterval(mcmc_cor_PC2_str)[,"lower"]),
                        Upper = c(HPDinterval(mcmc_cor_PC1_steplength)[,"upper"],HPDinterval(mcmc_cor_PC1_HR)[,"upper"],
                                  HPDinterval(mcmc_cor_PC1_str)[,"upper"],
                                  HPDinterval(mcmc_cor_PC2_steplength)[,"upper"],HPDinterval(mcmc_cor_PC2_HR)[,"upper"],
                                  HPDinterval(mcmc_cor_PC2_str)[,"upper"]))

mcmc_cor_deer <- bind_rows(mcmc_cor_lo %>% mutate (mod = "lo"),
                            mcmc_cor_me %>% mutate (mod = "me"),
                            mcmc_cor_hi %>% mutate (mod = "hi"),
                            mcmc_cor_exhi %>% mutate (mod = "exhi"))
write_csv(mcmc_cor_deer, "./result/midproduct/behaviorsyndrome_deer.csv")
