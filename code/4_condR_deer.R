## to calculate conditional R at a certain fence density value, re-center fence density at these values and run models

### calculating conditional R at lo, me, hi, exhi fence density
library(tidyverse)
library(data.table)
library(MCMCglmm)
library(coda)
library(bayestestR)

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
    HR_size = log(HR_size),
    total_step_lengths = log(total_step_lengths)
  ) %>% 
  left_join(deer.mig.status, by = "id_yr_mo")


# calculation of these fence density quantiles see c_prongondR
lo = 0.39
me = 0.68
hi = 1.00
exhi = 1.49

# #################################################################
# ############################ models #############################
# #################################################################

deer.l <- deer.l %>% mutate(fence_dens_ehc = fence_density - exhi,
                                       fence_dens_hc = fence_density - hi,
                                       fence_dens_mc = fence_density - me,
                                       fence_dens_lc = fence_density - lo)

prior_2 <- list(G =list(G1 = list(V = diag(2), nu = 2, alpha.mu = rep(0,2), alpha.V = diag(25^2,2,2))),
                  R  = list(V = 1, nu=0.002))

#################### centering at different fence densities  ###########
mcmc_PC1_mc <- MCMCglmm(PC1 ~ fence_dens_mc+ sin_mo + cos_mo + mig_status,
                       random =~ us(1 + fence_dens_mc):id,
                       rcov = ~units,
                       family = "gaussian",
                       prior = prior_2,
                       nitt=420000,
                       burnin=20000,
                       thin=100,
                       verbose = TRUE,
                       data = deer.l)
saveRDS(mcmc_PC1_mc, "result/models/mcmc_deer_pc1_condR_mc.RDS")

mcmc_PC2_mc <- MCMCglmm(PC2 ~ fence_dens_mc+ sin_mo + cos_mo + mig_status,
                          random =~ us(1 + fence_dens_mc):id,
                          rcov = ~units,
                          family = "gaussian",
                          prior = prior_2,
                          nitt=420000,
                          burnin=20000,
                          thin=100,
                          verbose = TRUE,
                          data = deer.l)
saveRDS(mcmc_PC2_mc, "result/models/mcmc_deer_pc2_condR_mc.RDS")

mcmc_length_mc <- MCMCglmm(total_step_lengths ~ fence_dens_mc + sin_mo + cos_mo + mig_status,
                           random =~ us(1 + fence_dens_mc):id,
                           rcov = ~units,
                           family = "gaussian",
                           prior = prior_2,
                           nitt=420000,
                           burnin=20000,
                           thin=100,
                           verbose = TRUE,
                           data = deer.l)
saveRDS(mcmc_length_mc, "result/models/mcmc_deer_length_condR_mc.RDS")

mcmc_range_mc <- MCMCglmm(HR_size ~ fence_dens_mc + sin_mo + cos_mo + mig_status,
                          random =~ us(1 + fence_dens_mc):id,
                          rcov = ~units,
                          family = "gaussian",
                          prior = prior_2,
                          nitt=420000,
                          burnin=20000,
                          thin=100,
                          verbose = TRUE,
                          data = deer.l)
saveRDS(mcmc_range_mc, "result/models/mcmc_deer_range_condR_mc.RDS")

#################### lo centered###########
mcmc_PC1_lc <- MCMCglmm(PC1 ~ fence_dens_lc + sin_mo + cos_mo + mig_status,
                        random =~ us(1 + fence_dens_lc):id,
                        rcov = ~units,
                        family = "gaussian",
                        prior = prior_2,
                        nitt=420000,
                        burnin=20000,
                        thin=100,
                        verbose = TRUE,
                        data = deer.l)
saveRDS(mcmc_PC1_lc, "result/models/mcmc_deer_pc1_condR_lc.RDS")

mcmc_PC2_lc <- MCMCglmm(PC2 ~ fence_dens_lc + sin_mo + cos_mo + mig_status,
                        random =~ us(1 + fence_dens_lc):id,
                        rcov = ~units,
                        family = "gaussian",
                        prior = prior_2,
                        nitt=420000,
                        burnin=20000,
                        thin=100,
                        verbose = TRUE,
                        data = deer.l)
saveRDS(mcmc_PC2_lc, "result/models/mcmc_deer_pc2_condR_lc.RDS")


mcmc_length_lc <- MCMCglmm(total_step_lengths ~ fence_dens_lc + sin_mo + cos_mo + mig_status,
                           random =~ us(1 + fence_dens_lc):id,
                           rcov = ~units,
                           family = "gaussian",
                           prior = prior_2,
                           nitt=420000,
                           burnin=20000,
                           thin=100,
                           verbose = TRUE,
                           data = deer.l)
saveRDS(mcmc_length_lc, "result/models/mcmc_deer_length_condR_lc.RDS")

mcmc_range_lc <- MCMCglmm(HR_size ~ fence_dens_lc + sin_mo + cos_mo + mig_status,
                          random =~ us(1 + fence_dens_lc):id,
                          rcov = ~units,
                          family = "gaussian",
                          prior = prior_2,
                          nitt=420000,
                          burnin=20000,
                          thin=100,
                          verbose = TRUE,
                          data = deer.l)
saveRDS(mcmc_range_lc, "result/models/mcmc_deer_range_condR_lc.RDS")

#################### hi centered ###########
mcmc_PC1_hc <- MCMCglmm(PC1 ~ fence_dens_hc + sin_mo + cos_mo + mig_status,
                        random =~ us(1 + fence_dens_hc):id,
                        rcov = ~units,
                        family = "gaussian",
                        prior = prior_2,
                        nitt=420000,
                        burnin=20000,
                        thin=100,
                        verbose = TRUE,
                        data = deer.l)
saveRDS(mcmc_PC1_hc, "result/models/mcmc_deer_pc1_condR_hc.RDS")

mcmc_PC2_hc <- MCMCglmm(PC2 ~ fence_dens_hc + sin_mo + cos_mo + mig_status,
                        random =~ us(1 + fence_dens_hc):id,
                        rcov = ~units,
                        family = "gaussian",
                        prior = prior_2,
                        nitt=420000,
                        burnin=20000,
                        thin=100,
                        verbose = TRUE,
                        data = deer.l)
saveRDS(mcmc_PC2_hc, "result/models/mcmc_deer_pc2_condR_hc.RDS")

mcmc_length_hc <- MCMCglmm(total_step_lengths ~ fence_dens_hc + sin_mo + cos_mo + mig_status,
                           random =~ us(1 + fence_dens_hc):id,
                           rcov = ~units,
                           family = "gaussian",
                           prior = prior_2,
                           nitt=420000,
                           burnin=20000,
                           thin=100,
                           verbose = TRUE,
                           data = deer.l)
saveRDS(mcmc_length_hc, "result/models/mcmc_deer_length_condR_hc.RDS")

mcmc_range_hc <- MCMCglmm(HR_size ~ fence_dens_hc + sin_mo + cos_mo + mig_status,
                          random =~ us(1 + fence_dens_hc):id,
                          rcov = ~units,
                          family = "gaussian",
                          prior = prior_2,
                          nitt=420000,
                          burnin=20000,
                          thin=100,
                          verbose = TRUE,
                          data = deer.l)
saveRDS(mcmc_range_hc, "result/models/mcmc_deer_range_condR_hc.RDS")

#################### extra hi centered ###########
mcmc_PC1_ehc <- MCMCglmm(PC1 ~ fence_dens_ehc + sin_mo + cos_mo + mig_status,
                        random =~ us(1 + fence_dens_ehc):id,
                        rcov = ~units,
                        family = "gaussian",
                        prior = prior_2,
                        nitt=420000,
                        burnin=20000,
                        thin=100,
                        verbose = TRUE,
                        data = deer.l)
saveRDS(mcmc_PC1_ehc, "result/models/mcmc_deer_pc1_condR_ehc.RDS")

mcmc_PC2_ehc <- MCMCglmm(PC2 ~ fence_dens_ehc+ sin_mo + cos_mo + mig_status,
                        random =~ us(1 + fence_dens_ehc):id,
                        rcov = ~units,
                        family = "gaussian",
                        prior = prior_2,
                        nitt=420000,
                        burnin=20000,
                        thin=100,
                        verbose = TRUE,
                        data = deer.l)
saveRDS(mcmc_PC2_ehc, "result/models/mcmc_deer_pc2_condR_ehc.RDS")

mcmc_length_ehc <- MCMCglmm(total_step_lengths ~ fence_dens_ehc + sin_mo + cos_mo + mig_status,
                            random =~ us(1 + fence_dens_ehc):id,
                            rcov = ~units,
                            family = "gaussian",
                            prior = prior_2,
                            nitt=420000,
                            burnin=20000,
                            thin=100,
                            verbose = TRUE,
                            data = deer.l)
saveRDS(mcmc_length_ehc, "result/models/mcmc_deer_length_condR_ehc.RDS")

mcmc_range_ehc <- MCMCglmm(HR_size ~ fence_dens_ehc + sin_mo + cos_mo + mig_status,
                           random =~ us(1 + fence_dens_ehc):id,
                           rcov = ~units,
                           family = "gaussian",
                           prior = prior_2,
                           nitt=420000,
                           burnin=20000,
                           thin=100,
                           verbose = TRUE,
                           data = deer.l)
saveRDS(mcmc_range_ehc, "result/models/mcmc_deer_range_condR_ehc.RDS")

# #################################################################
# ############################ condR  #############################
# #################################################################
mcmc_PC1_lc <- readRDS("result/models/mcmc_deer_pc1_condR_lc.RDS")
mcmc_PC1_mc <- readRDS("result/models/mcmc_deer_pc1_condR_mc.RDS")
mcmc_PC1_hc <- readRDS("result/models/mcmc_deer_pc1_condR_hc.RDS")
mcmc_PC1_ehc <- readRDS("result/models/mcmc_deer_pc1_condR_ehc.RDS")

mcmc_PC2_lc <- readRDS("result/models/mcmc_deer_pc2_condR_lc.RDS")
mcmc_PC2_mc <- readRDS("result/models/mcmc_deer_pc2_condR_mc.RDS")
mcmc_PC2_hc <- readRDS("result/models/mcmc_deer_pc2_condR_hc.RDS")
mcmc_PC2_ehc <- readRDS("result/models/mcmc_deer_pc2_condR_ehc.RDS")

mcmc_length_lc <- readRDS("result/models/mcmc_deer_length_condR_lc.RDS")
mcmc_length_mc <- readRDS("result/models/mcmc_deer_length_condR_mc.RDS")
mcmc_length_hc <- readRDS("result/models/mcmc_deer_length_condR_hc.RDS")
mcmc_length_ehc <- readRDS("result/models/mcmc_deer_length_condR_ehc.RDS")

mcmc_range_lc <- readRDS("result/models/mcmc_deer_range_condR_lc.RDS")
mcmc_range_mc <- readRDS("result/models/mcmc_deer_range_condR_mc.RDS")
mcmc_range_hc <- readRDS("result/models/mcmc_deer_range_condR_hc.RDS")
mcmc_range_ehc <- readRDS("result/models/mcmc_deer_range_condR_ehc.RDS")

###### PC1 condR
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

PC1_r_ehc <- mcmc_PC1_ehc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC1_ehc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC1_ehc$VCV[,"units"] )
mean(PC1_r_ehc)

##### PC2 condR
PC2_r_lc <- mcmc_PC2_lc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC2_lc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC2_lc$VCV[,"units"] )
mean(PC2_r_lc) #0.0410578
HPDinterval(PC2_r_lc)

PC2_r_mc <- mcmc_PC2_mc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC2_mc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC2_mc$VCV[,"units"] )
mean(PC2_r_mc) #0.03955209
HPDinterval(PC2_r_mc)

PC2_r_hc <- mcmc_PC2_hc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC2_hc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC2_hc$VCV[,"units"] )
mean(PC2_r_hc) #0.09128384
HPDinterval(PC2_r_hc)

PC2_r_ehc <- mcmc_PC2_ehc$VCV[,"(Intercept):(Intercept).id"]/
  (mcmc_PC2_ehc$VCV[,"(Intercept):(Intercept).id"] + mcmc_PC2_ehc$VCV[,"units"] )
mean(PC2_r_ehc) #0.2356035
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

deer.condR <- tibble(Trait = c(rep("PC1", 4), rep("PC2",4)),
                          mod = c(rep(c("lc", "mc", "hc", "ehc"),2)),
                          condR.estimate = c(mean(PC1_r_lc), mean(PC1_r_mc), mean(PC1_r_hc), mean(PC1_r_ehc),
                                             mean(PC2_r_lc), mean(PC2_r_mc), mean(PC2_r_hc), mean(PC2_r_ehc)),
                          condR.lower = c(HPDinterval(PC1_r_lc)[1], HPDinterval(PC1_r_mc)[1], HPDinterval(PC1_r_hc)[1], HPDinterval(PC1_r_ehc)[1],
                                          HPDinterval(PC2_r_lc)[1], HPDinterval(PC2_r_mc)[1], HPDinterval(PC2_r_hc)[1], HPDinterval(PC2_r_ehc)[1]),
                          condR.upper = c(HPDinterval(PC1_r_lc)[2], HPDinterval(PC1_r_mc)[2], HPDinterval(PC1_r_hc)[2], HPDinterval(PC1_r_ehc)[2],
                                          HPDinterval(PC2_r_lc)[2], HPDinterval(PC2_r_mc)[2], HPDinterval(PC2_r_hc)[2], HPDinterval(PC2_r_ehc)[2])
                          )

deer.allbehavior.condR <- tibble(Trait = c(rep("PC1", 4), rep("PC2",4), rep("total distance",4), rep("range size",4)),
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

                                 
write_csv(deer.condR, "./result/midproduct/condR_4scenarios_deer.csv")
write_csv(deer.allbehavior.condR, "./result/midproduct/condR_allbehavior_4scenarios_deer.csv")


###########################################
#for appendix s5 table of residual variance
deer.variance <- tibble (Trait = c(rep("PC1", 4), rep("PC2", 4)),
                         mod = c(rep(c("lc", "mc", "hc", "ehc"),2)),
                         Vi.estimate = c(mean( mcmc_PC1_lc$VCV[,"(Intercept):(Intercept).id"] ), 
                                         mean( mcmc_PC1_mc$VCV[,"(Intercept):(Intercept).id"] ), 
                                         mean( mcmc_PC1_hc$VCV[,"(Intercept):(Intercept).id"] ), 
                                         mean( mcmc_PC1_ehc$VCV[,"(Intercept):(Intercept).id"] ),
                                         
                                         mean( mcmc_PC2_lc$VCV[,"(Intercept):(Intercept).id"] ), 
                                         mean( mcmc_PC2_mc$VCV[,"(Intercept):(Intercept).id"] ), 
                                         mean( mcmc_PC2_hc$VCV[,"(Intercept):(Intercept).id"] ), 
                                         mean( mcmc_PC2_ehc$VCV[,"(Intercept):(Intercept).id"] )),
                         
                         Vi.lower = c(HPDinterval(mcmc_PC1_lc$VCV[,"(Intercept):(Intercept).id"])[1], HPDinterval(mcmc_PC1_mc$VCV[,"(Intercept):(Intercept).id"])[1], 
                                      HPDinterval(mcmc_PC1_hc$VCV[,"(Intercept):(Intercept).id"])[1], HPDinterval(mcmc_PC1_ehc$VCV[,"(Intercept):(Intercept).id"])[1],
                                      
                                      HPDinterval(mcmc_PC2_lc$VCV[,"(Intercept):(Intercept).id"])[1], HPDinterval(mcmc_PC2_mc$VCV[,"(Intercept):(Intercept).id"])[1], 
                                      HPDinterval(mcmc_PC2_hc$VCV[,"(Intercept):(Intercept).id"])[1], HPDinterval(mcmc_PC2_ehc$VCV[,"(Intercept):(Intercept).id"])[1]),
                         
                         Vi.upper = c(HPDinterval(mcmc_PC1_lc$VCV[,"(Intercept):(Intercept).id"])[2], HPDinterval(mcmc_PC1_mc$VCV[,"(Intercept):(Intercept).id"])[2], 
                                      HPDinterval(mcmc_PC1_hc$VCV[,"(Intercept):(Intercept).id"])[2], HPDinterval(mcmc_PC1_ehc$VCV[,"(Intercept):(Intercept).id"])[2],
                                      
                                      HPDinterval(mcmc_PC2_lc$VCV[,"(Intercept):(Intercept).id"])[2], HPDinterval(mcmc_PC2_mc$VCV[,"(Intercept):(Intercept).id"])[2], 
                                      HPDinterval(mcmc_PC2_hc$VCV[,"(Intercept):(Intercept).id"])[2], HPDinterval(mcmc_PC2_ehc$VCV[,"(Intercept):(Intercept).id"])[2]),
                         
                         Vr.estimate = c(mean( mcmc_PC1_lc$VCV[,"units"] ), 
                                         mean( mcmc_PC1_mc$VCV[,"units"] ), 
                                         mean( mcmc_PC1_hc$VCV[,"units"] ), 
                                         mean( mcmc_PC1_ehc$VCV[,"units"] ),
                                         
                                         mean( mcmc_PC2_lc$VCV[,"units"] ), 
                                         mean( mcmc_PC2_mc$VCV[,"units"] ), 
                                         mean( mcmc_PC2_hc$VCV[,"units"] ), 
                                         mean( mcmc_PC2_ehc$VCV[,"units"] )),
                         
                         Vr.lower = c(HPDinterval(mcmc_PC1_lc$VCV[,"units"])[1], HPDinterval(mcmc_PC1_mc$VCV[,"units"])[1], 
                                      HPDinterval(mcmc_PC1_hc$VCV[,"units"])[1], HPDinterval(mcmc_PC1_ehc$VCV[,"units"])[1],
                                      
                                      HPDinterval(mcmc_PC2_lc$VCV[,"units"])[1], HPDinterval(mcmc_PC2_mc$VCV[,"units"])[1], 
                                      HPDinterval(mcmc_PC2_hc$VCV[,"units"])[1], HPDinterval(mcmc_PC2_ehc$VCV[,"units"])[1]),
                         
                         Vr.upper = c(HPDinterval(mcmc_PC1_lc$VCV[,"units"])[2], HPDinterval(mcmc_PC1_mc$VCV[,"units"])[2], 
                                      HPDinterval(mcmc_PC1_hc$VCV[,"units"])[2], HPDinterval(mcmc_PC1_ehc$VCV[,"units"])[2],
                                      
                                      HPDinterval(mcmc_PC2_lc$VCV[,"units"])[2], HPDinterval(mcmc_PC2_mc$VCV[,"units"])[2], 
                                      HPDinterval(mcmc_PC2_hc$VCV[,"units"])[2], HPDinterval(mcmc_PC2_ehc$VCV[,"units"])[2]))
write_csv(deer.variance, "./result/midproduct/Vr_Vi_4scenarios_deer.csv")


###########################################
## for table intercept-slope covariance ###
###########################################
#pc1
PC1_cor_RR_lc <- mcmc_PC1_lc$VCV[, "(Intercept):fence_dens_lc.id"]/
  (sqrt(mcmc_PC1_lc$VCV[,"(Intercept):(Intercept).id"]) * 
     sqrt(mcmc_PC1_lc$VCV[,"fence_dens_lc:fence_dens_lc.id"]))

PC1_cor_RR_mc <- mcmc_PC1_mc$VCV[, "(Intercept):fence_dens_mc.id"]/
  (sqrt(mcmc_PC1_mc$VCV[,"(Intercept):(Intercept).id"]) * 
     sqrt(mcmc_PC1_mc$VCV[,"fence_dens_mc:fence_dens_mc.id"]))

PC1_cor_RR_hc <- mcmc_PC1_hc$VCV[, "(Intercept):fence_dens_hc.id"]/
  (sqrt(mcmc_PC1_hc$VCV[,"(Intercept):(Intercept).id"]) * 
     sqrt(mcmc_PC1_hc$VCV[,"fence_dens_hc:fence_dens_hc.id"]))

PC1_cor_RR_ehc <- mcmc_PC1_ehc$VCV[, "(Intercept):fence_dens_ehc.id"]/
  (sqrt(mcmc_PC1_ehc$VCV[,"(Intercept):(Intercept).id"]) * 
     sqrt(mcmc_PC1_ehc$VCV[,"fence_dens_ehc:fence_dens_ehc.id"]))

# pc2
PC2_cor_RR_lc <- mcmc_PC2_lc$VCV[, "(Intercept):fence_dens_lc.id"]/
  (sqrt(mcmc_PC2_lc$VCV[,"(Intercept):(Intercept).id"]) * 
     sqrt(mcmc_PC2_lc$VCV[,"fence_dens_lc:fence_dens_lc.id"]))

PC2_cor_RR_mc <- mcmc_PC2_mc$VCV[, "(Intercept):fence_dens_mc.id"]/
  (sqrt(mcmc_PC2_mc$VCV[,"(Intercept):(Intercept).id"]) * 
     sqrt(mcmc_PC2_mc$VCV[,"fence_dens_mc:fence_dens_mc.id"]))

PC2_cor_RR_hc <- mcmc_PC2_hc$VCV[, "(Intercept):fence_dens_hc.id"]/
  (sqrt(mcmc_PC2_hc$VCV[,"(Intercept):(Intercept).id"]) * 
     sqrt(mcmc_PC2_hc$VCV[,"fence_dens_hc:fence_dens_hc.id"]))

PC2_cor_RR_ehc <- mcmc_PC2_ehc$VCV[, "(Intercept):fence_dens_ehc.id"]/
  (sqrt(mcmc_PC2_ehc$VCV[,"(Intercept):(Intercept).id"]) * 
     sqrt(mcmc_PC2_ehc$VCV[,"fence_dens_ehc:fence_dens_ehc.id"]))

deer.slo.int <- tibble (Trait = c(rep("PC1", 4), rep("PC2", 4)),
                         mod = c(rep(c("lc", "mc", "hc", "ehc"),2)),
                         RR.estimate = c(mean( PC1_cor_RR_lc  ), 
                                         mean( PC1_cor_RR_mc ), 
                                         mean( PC1_cor_RR_hc ), 
                                         mean( PC1_cor_RR_ehc ),
                                         
                                         mean( PC2_cor_RR_lc ), 
                                         mean( PC2_cor_RR_mc ), 
                                         mean( PC2_cor_RR_hc), 
                                         mean( PC2_cor_RR_ehc)),
                         
                         RR.lower = c(HPDinterval(PC1_cor_RR_lc)[1], HPDinterval(PC1_cor_RR_mc)[1], 
                                      HPDinterval(PC1_cor_RR_hc)[1], HPDinterval(PC1_cor_RR_ehc)[1],
                                      
                                      HPDinterval(PC2_cor_RR_lc)[1], HPDinterval(PC2_cor_RR_mc)[1], 
                                      HPDinterval(PC2_cor_RR_hc)[1], HPDinterval(PC2_cor_RR_ehc)[1]),
                         
                         RR.upper = c(HPDinterval(PC1_cor_RR_lc)[2], HPDinterval(PC1_cor_RR_mc)[2], 
                                      HPDinterval(PC1_cor_RR_hc)[2], HPDinterval(PC1_cor_RR_ehc)[2],
                                      
                                      HPDinterval(PC2_cor_RR_lc)[2], HPDinterval(PC2_cor_RR_mc)[2], 
                                      HPDinterval(PC2_cor_RR_hc)[2], HPDinterval(PC2_cor_RR_ehc)[2]),
                         pd = c(p_direction(PC1_cor_RR_lc)$pd[1], p_direction(PC1_cor_RR_mc)$pd[1],
                                p_direction(PC1_cor_RR_hc)$pd[1], p_direction(PC1_cor_RR_ehc)$pd[1],
                                
                                p_direction(PC2_cor_RR_lc)$pd[1], p_direction(PC2_cor_RR_mc)$pd[1],
                                p_direction(PC2_cor_RR_hc)$pd[1], p_direction(PC2_cor_RR_ehc)$pd[1]))
write_csv(deer.slo.int, "./result/midproduct/Cor_Slo_Int_4scenarios_deer.csv")

