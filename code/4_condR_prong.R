## to calculate conditional R at a certain fence density value, re-center fence density at these values and run models

### calculating conditional R at lo, me, hi, exhi fence density
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2")
library(tidyverse)
library(data.table)
library(MCMCglmm)
library(coda)

pronghorn <- read_csv("./result/prong_df_monthly.csv") %>% 
  mutate(mo = as.numeric(mo), yr = as.numeric(yr)) %>% 
  dplyr::select(id_yr_mo, id, fence_density, mo, yr, PC1, PC2)

#data transformation 
pronghorn.l <- pronghorn %>% 
  mutate(
    fence_density = fence_density*1000, #to bring to similar magnitute and also means km/km2, easier for interpreting
    mo = mo - 1, # so that when calculating intercept, x = 0 makes sense. Otherwise m0 can never be 0 in reality. 
    cos_mo = cos(mo),
    sin_mo = sin(mo)
  )
#ggpairs(pronghorn.l, columns= 3:9) 

lo = 0.39
me = 0.68
hi = 1.00
exhi = 1.49

# #################################################################
# ############################ models #############################
# #################################################################

# pronghorn.l <- pronghorn.l %>% mutate(fence_dens_ehc = fence_density - exhi,
#                                        fence_dens_hc = fence_density - hi,
#                                        fence_dens_mc = fence_density - me,
#                                        fence_dens_lc = fence_density - lo)
# 
# prior_2 <- list(G =list(G1 = list(V = diag(2), nu = 2, alpha.mu = rep(0,2), alpha.V = diag(25^2,2,2))),
#                   R  = list(V = 1, nu=0.002))
# 
# #################### median centered###########
# mcmc_PC1_mc <- MCMCglmm(PC1 ~ fence_dens_mc + sin_mo + cos_mo + mig_status,
#                        random =~ us(1 + fence_dens_mc):id,
#                        rcov = ~units,
#                        family = "gaussian",
#                        prior = prior_2,
#                        nitt=420000,
#                        burnin=20000,
#                        thin=100,
#                        verbose = TRUE,
#                        data = pronghorn.l)
# saveRDS(mcmc_PC1_mc, "result/models/mcmc_prong_pc1_condR_mc.RDS")
# 
# mcmc_PC2_mc <- MCMCglmm(PC2 ~ fence_dens_mc + sin_mo + cos_mo + mig_status,
#                           random =~ us(1 + fence_dens_mc):id,
#                           rcov = ~units,
#                           family = "gaussian",
#                           prior = prior_2,
#                           nitt=420000,
#                           burnin=20000,
#                           thin=100,
#                           verbose = TRUE,
#                           data = pronghorn.l)
# saveRDS(mcmc_PC2_mc, "result/models/mcmc_prong_pc2_condR_mc.RDS")
# 
# mcmc_length_mc <- MCMCglmm(total_step_lengths ~ fence_dens_mc + sin_mo + cos_mo + mig_status,
#                         random =~ us(1 + fence_dens_mc):id,
#                         rcov = ~units,
#                         family = "gaussian",
#                         prior = prior_2,
#                         nitt=420000,
#                         burnin=20000,
#                         thin=100,
#                         verbose = TRUE,
#                         data = pronghorn.l)
# saveRDS(mcmc_length_mc, "result/models/mcmc_prong_length_condR_mc.RDS")
# 
# mcmc_range_mc <- MCMCglmm(HR_size ~ fence_dens_mc + sin_mo + cos_mo + mig_status,
#                         random =~ us(1 + fence_dens_mc):id,
#                         rcov = ~units,
#                         family = "gaussian",
#                         prior = prior_2,
#                         nitt=420000,
#                         burnin=20000,
#                         thin=100,
#                         verbose = TRUE,
#                         data = pronghorn.l)
# saveRDS(mcmc_range_mc, "result/models/mcmc_prong_range_condR_mc.RDS")
# 
# ################### lo centered###########
# mcmc_PC1_lc <- MCMCglmm(PC1 ~ fence_dens_lc + sin_mo + cos_mo + mig_status,
#                         random =~ us(1 + fence_dens_lc):id,
#                         rcov = ~units,
#                         family = "gaussian",
#                         prior = prior_2,
#                         nitt=420000,
#                         burnin=20000,
#                         thin=100,
#                         verbose = TRUE,
#                         data = pronghorn.l)
# saveRDS(mcmc_PC1_lc, "result/models/mcmc_prong_pc1_condR_lc.RDS")
# 
# mcmc_PC2_lc <- MCMCglmm(PC2 ~ fence_dens_lc + sin_mo + cos_mo + mig_status,
#                         random =~ us(1 + fence_dens_lc):id,
#                         rcov = ~units,
#                         family = "gaussian",
#                         prior = prior_2,
#                         nitt=420000,
#                         burnin=20000,
#                         thin=100,
#                         verbose = TRUE,
#                         data = pronghorn.l)
# saveRDS(mcmc_PC2_lc, "result/models/mcmc_prong_pc2_condR_lc.RDS")
# 
# mcmc_length_lc <- MCMCglmm(total_step_lengths ~ fence_dens_lc + sin_mo + cos_mo + mig_status,
#                            random =~ us(1 + fence_dens_lc):id,
#                            rcov = ~units,
#                            family = "gaussian",
#                            prior = prior_2,
#                            nitt=420000,
#                            burnin=20000,
#                            thin=100,
#                            verbose = TRUE,
#                            data = pronghorn.l)
# saveRDS(mcmc_length_lc, "result/models/mcmc_prong_length_condR_lc.RDS")
# 
# mcmc_range_lc <- MCMCglmm(HR_size ~ fence_dens_lc + sin_mo + cos_mo + mig_status,
#                           random =~ us(1 + fence_dens_lc):id,
#                           rcov = ~units,
#                           family = "gaussian",
#                           prior = prior_2,
#                           nitt=420000,
#                           burnin=20000,
#                           thin=100,
#                           verbose = TRUE,
#                           data = pronghorn.l)
# saveRDS(mcmc_range_lc, "result/models/mcmc_prong_range_condR_lc.RDS")
# 
# #################### hi centered ###########
# mcmc_PC1_hc <- MCMCglmm(PC1 ~ fence_dens_hc + sin_mo + cos_mo + mig_status,
#                         random =~ us(1 + fence_dens_hc):id,
#                         rcov = ~units,
#                         family = "gaussian",
#                         prior = prior_2,
#                         nitt=420000,
#                         burnin=20000,
#                         thin=100,
#                         verbose = TRUE,
#                         data = pronghorn.l)
# saveRDS(mcmc_PC1_hc, "result/models/mcmc_prong_pc1_condR_hc.RDS")
# 
# mcmc_PC2_hc <- MCMCglmm(PC2 ~ fence_dens_hc + sin_mo + cos_mo + mig_status,
#                         random =~ us(1 + fence_dens_hc):id,
#                         rcov = ~units,
#                         family = "gaussian",
#                         prior = prior_2,
#                         nitt=420000,
#                         burnin=20000,
#                         thin=100,
#                         verbose = TRUE,
#                         data = pronghorn.l)
# saveRDS(mcmc_PC2_hc, "result/models/mcmc_prong_pc2_condR_hc.RDS")
# 
# mcmc_length_hc <- MCMCglmm(total_step_lengths ~ fence_dens_hc + sin_mo + cos_mo + mig_status,
#                            random =~ us(1 + fence_dens_hc):id,
#                            rcov = ~units,
#                            family = "gaussian",
#                            prior = prior_2,
#                            nitt=420000,
#                            burnin=20000,
#                            thin=100,
#                            verbose = TRUE,
#                            data = pronghorn.l)
# saveRDS(mcmc_length_hc, "result/models/mcmc_prong_length_condR_hc.RDS")
# 
# mcmc_range_hc <- MCMCglmm(HR_size ~ fence_dens_hc + sin_mo + cos_mo + mig_status,
#                           random =~ us(1 + fence_dens_hc):id,
#                           rcov = ~units,
#                           family = "gaussian",
#                           prior = prior_2,
#                           nitt=420000,
#                           burnin=20000,
#                           thin=100,
#                           verbose = TRUE,
#                           data = pronghorn.l)
# saveRDS(mcmc_range_hc, "result/models/mcmc_prong_range_condR_hc.RDS")
# #################### extra hi centered ###########
# mcmc_PC1_ehc <- MCMCglmm(PC1 ~ fence_dens_ehc + sin_mo + cos_mo + mig_status,
#                         random =~ us(1 + fence_dens_ehc):id,
#                         rcov = ~units,
#                         family = "gaussian",
#                         prior = prior_2,
#                         nitt=420000,
#                         burnin=20000,
#                         thin=100,
#                         verbose = TRUE,
#                         data = pronghorn.l)
# saveRDS(mcmc_PC1_ehc, "result/models/mcmc_prong_pc1_condR_ehc.RDS")
# 
# mcmc_PC2_ehc <- MCMCglmm(PC2 ~ fence_dens_ehc  + sin_mo + cos_mo + mig_status,
#                         random =~ us(1 + fence_dens_ehc):id,
#                         rcov = ~units,
#                         family = "gaussian",
#                         prior = prior_2,
#                         nitt=420000,
#                         burnin=20000,
#                         thin=100,
#                         verbose = TRUE,
#                         data = pronghorn.l)
# saveRDS(mcmc_PC2_ehc, "result/models/mcmc_prong_pc2_condR_ehc.RDS")
# 
# mcmc_length_ehc <- MCMCglmm(total_step_lengths ~ fence_dens_ehc + sin_mo + cos_mo + mig_status,
#                            random =~ us(1 + fence_dens_ehc):id,
#                            rcov = ~units,
#                            family = "gaussian",
#                            prior = prior_2,
#                            nitt=420000,
#                            burnin=20000,
#                            thin=100,
#                            verbose = TRUE,
#                            data = pronghorn.l)
# saveRDS(mcmc_length_ehc, "result/models/mcmc_prong_length_condR_ehc.RDS")
# 
# mcmc_range_ehc <- MCMCglmm(HR_size ~ fence_dens_ehc + sin_mo + cos_mo + mig_status,
#                           random =~ us(1 + fence_dens_ehc):id,
#                           rcov = ~units,
#                           family = "gaussian",
#                           prior = prior_2,
#                           nitt=420000,
#                           burnin=20000,
#                           thin=100,
#                           verbose = TRUE,
#                           data = pronghorn.l)
# saveRDS(mcmc_range_ehc, "result/models/mcmc_prong_range_condR_ehc.RDS")
# #################################################################
# ############################ condR  #############################
# #################################################################

mcmc_PC1_lc <- readRDS("result/models/mcmc_prong_pc1_condR_lc.RDS")
mcmc_PC1_mc <- readRDS("result/models/mcmc_prong_pc1_condR_mc.RDS")
mcmc_PC1_hc <- readRDS("result/models/mcmc_prong_pc1_condR_hc.RDS")
mcmc_PC1_ehc <- readRDS("result/models/mcmc_prong_pc1_condR_ehc.RDS")

mcmc_PC2_lc <- readRDS("result/models/mcmc_prong_pc2_condR_lc.RDS")
mcmc_PC2_mc <- readRDS("result/models/mcmc_prong_pc2_condR_mc.RDS")
mcmc_PC2_hc <- readRDS("result/models/mcmc_prong_pc2_condR_hc.RDS")
mcmc_PC2_ehc <- readRDS("result/models/mcmc_prong_pc2_condR_ehc.RDS")

mcmc_length_lc <- readRDS("result/models/mcmc_prong_length_condR_lc.RDS")
mcmc_length_mc <- readRDS("result/models/mcmc_prong_length_condR_mc.RDS")
mcmc_length_hc <- readRDS("result/models/mcmc_prong_length_condR_hc.RDS")
mcmc_length_ehc <- readRDS("result/models/mcmc_prong_length_condR_ehc.RDS")

mcmc_range_lc <- readRDS("result/models/mcmc_prong_range_condR_lc.RDS")
mcmc_range_mc <- readRDS("result/models/mcmc_prong_range_condR_mc.RDS")
mcmc_range_hc <- readRDS("result/models/mcmc_prong_range_condR_hc.RDS")
mcmc_range_ehc <- readRDS("result/models/mcmc_prong_range_condR_ehc.RDS")

#### PC1 conditional R
PC1_r_lc <- mcmc_PC1_lc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC1_lc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC1_lc$VCV[,"units"] )
mean(PC1_r_lc) 
HPDinterval(PC1_r_lc)

PC1_r_mc <- mcmc_PC1_mc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC1_mc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC1_mc$VCV[,"units"] )
mean(PC1_r_mc) 
HPDinterval(PC1_r_mc)

PC1_r_hc <- mcmc_PC1_hc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC1_hc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC1_hc$VCV[,"units"] )
mean(PC1_r_hc) 
HPDinterval(PC1_r_hc)

PC1_r_ehc <- mcmc_PC2_ehc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC1_ehc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC1_ehc$VCV[,"units"] )
mean(PC1_r_ehc) 
HPDinterval(PC1_r_ehc)

#### PC2 conditional R
PC2_r_lc <- mcmc_PC2_lc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC2_lc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC2_lc$VCV[,"units"] )
mean(PC2_r_lc) 
HPDinterval(PC2_r_lc)

PC2_r_mc <- mcmc_PC2_mc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC2_mc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC2_mc$VCV[,"units"] )
mean(PC2_r_mc) 
HPDinterval(PC2_r_mc)

PC2_r_hc <- mcmc_PC2_hc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC2_hc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC2_hc$VCV[,"units"] )
mean(PC2_r_hc) 
HPDinterval(PC2_r_hc)

PC2_r_ehc <- mcmc_PC2_ehc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC2_ehc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC2_ehc$VCV[,"units"] )
mean(PC2_r_ehc) 
HPDinterval(PC2_r_ehc)

#### movement lenghth conditional R
length_r_lc <- mcmc_length_lc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_length_lc$VCV[,"(Intercept):(Intercept).id"] + mcmc_length_lc$VCV[,"units"] )
mean(length_r_lc) 
HPDinterval(length_r_lc)

length_r_mc <- mcmc_length_mc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_length_mc$VCV[,"(Intercept):(Intercept).id"] + mcmc_length_mc$VCV[,"units"] )
mean(length_r_mc) 
HPDinterval(length_r_mc)

length_r_hc <- mcmc_length_hc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_length_hc$VCV[,"(Intercept):(Intercept).id"] + mcmc_length_hc$VCV[,"units"] )
mean(length_r_hc) 
HPDinterval(length_r_hc)

length_r_ehc <- mcmc_length_ehc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_length_ehc$VCV[,"(Intercept):(Intercept).id"] + mcmc_length_ehc$VCV[,"units"] )
mean(length_r_ehc) 
HPDinterval(length_r_ehc)

#### range size conditional R
range_r_lc <- mcmc_range_lc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_range_lc$VCV[,"(Intercept):(Intercept).id"] + mcmc_range_lc$VCV[,"units"] )
mean(range_r_lc) 
HPDinterval(range_r_lc)

range_r_mc <- mcmc_range_mc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_range_mc$VCV[,"(Intercept):(Intercept).id"] + mcmc_range_mc$VCV[,"units"] )
mean(range_r_mc) 
HPDinterval(range_r_mc)

range_r_hc <- mcmc_range_hc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_range_hc$VCV[,"(Intercept):(Intercept).id"] + mcmc_range_hc$VCV[,"units"] )
mean(range_r_hc) 
HPDinterval(range_r_hc)

range_r_ehc <- mcmc_range_ehc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_range_ehc$VCV[,"(Intercept):(Intercept).id"] + mcmc_range_ehc$VCV[,"units"] )
mean(range_r_ehc) 
HPDinterval(range_r_ehc)


pronghorn.condR <- tibble(Trait = c(rep("PC1", 4), rep("PC2",4)),
                          mod = c(rep(c("lc", "mc", "hc", "ehc"),2)),
                          condR.estimate = c(mean(PC1_r_lc), mean(PC1_r_mc), mean(PC1_r_hc), mean(PC1_r_ehc),
                                             mean(PC2_r_lc), mean(PC2_r_mc), mean(PC2_r_hc), mean(PC2_r_ehc)),
                          condR.lower = c(HPDinterval(PC1_r_lc)[1], HPDinterval(PC1_r_mc)[1], HPDinterval(PC1_r_hc)[1], HPDinterval(PC1_r_ehc)[1],
                                          HPDinterval(PC2_r_lc)[1], HPDinterval(PC2_r_mc)[1], HPDinterval(PC2_r_hc)[1], HPDinterval(PC2_r_ehc)[1]),
                          condR.upper = c(HPDinterval(PC1_r_lc)[2], HPDinterval(PC1_r_mc)[2], HPDinterval(PC1_r_hc)[2], HPDinterval(PC1_r_ehc)[2],
                                          HPDinterval(PC2_r_lc)[2], HPDinterval(PC2_r_mc)[2], HPDinterval(PC2_r_hc)[2], HPDinterval(PC2_r_ehc)[2])
)

pronghorn.allbehavior.condR <- tibble(Trait = c(rep("PC1", 4), rep("PC2",4), rep("total distance",4), rep("range size",4)),
                                      mod = c(rep(c("lc", "mc", "hc", "ehc"),4)),
                                      condR.estimate = c(mean(PC1_r_lc), mean(PC1_r_mc), mean(PC1_r_hc), mean(PC1_r_ehc),
                                                         mean(PC2_r_lc), mean(PC2_r_mc), mean(PC2_r_hc), mean(PC2_r_ehc),
                                                         mean(length_r_lc), mean(length_r_mc), mean(length_r_hc), mean(length_r_ehc),
                                                         mean(range_r_lc), mean(range_r_mc), mean(range_r_hc), mean(range_r_ehc)),
                                      condR.lower = c(HPDinterval(PC1_r_lc)[1], HPDinterval(PC1_r_mc)[1], HPDinterval(PC1_r_hc)[1], HPDinterval(PC1_r_ehc)[1],
                                                      HPDinterval(PC2_r_lc)[1], HPDinterval(PC2_r_mc)[1], HPDinterval(PC2_r_hc)[1], HPDinterval(PC2_r_ehc)[1],
                                                      HPDinterval(length_r_lc)[1], HPDinterval(length_r_mc)[1], HPDinterval(length_r_hc)[1], HPDinterval(length_r_ehc)[1],
                                                      HPDinterval(range_r_lc)[1], HPDinterval(range_r_mc)[1], HPDinterval(range_r_hc)[1], HPDinterval(range_r_ehc)[1]),
                                      condR.upper = c(HPDinterval(PC1_r_lc)[2], HPDinterval(PC1_r_mc)[2], HPDinterval(PC1_r_hc)[2], HPDinterval(PC1_r_ehc)[2],
                                                      HPDinterval(PC2_r_lc)[2], HPDinterval(PC2_r_mc)[2], HPDinterval(PC2_r_hc)[2], HPDinterval(PC2_r_ehc)[2],
                                                      HPDinterval(length_r_lc)[2], HPDinterval(length_r_mc)[2], HPDinterval(length_r_hc)[2], HPDinterval(length_r_ehc)[2],
                                                      HPDinterval(range_r_lc)[2], HPDinterval(range_r_mc)[2], HPDinterval(range_r_hc)[2], HPDinterval(range_r_ehc)[2]))

write_csv(pronghorn.condR, "./result/midproduct/condR_4scenarios_prong.csv")
write_csv(pronghorn.allbehavior.condR, "./result/midproduct/condR_allbehavior_4scenarios_prong.csv")