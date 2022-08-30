# compare model with random intercept and random regression model to examine plasticity 
# then use conR methods to calculating conditional/visualizing on a (near)continuous scale

#################################################################
########################## set up ###############################
#################################################################
library(tidyverse)
library(data.table)
library(MCMCglmm)
library(coda)
library(bayestestR)

pronghorn <- read_csv("./result/prong_df_monthly.csv") %>% 
  mutate(mo = as.numeric(mo), yr = as.numeric(yr)) %>% 
  dplyr::select(id_yr_mo, id, fence_density, mo, yr, PC1, PC2)
length(unique(pronghorn$id_yr_mo)) # 822 id_mo left
length(unique(pronghorn$id)) # 61 animals

pronghorn.mig.status <- read_csv('./result/midproduct/manualmigstatus_prong.csv') %>% 
  pivot_longer(cols = 2:13, names_to = "month", values_to = "mig_status") %>%
  filter(!is.na(mig_status)) %>%
  mutate(id_yr_mo = paste0(id_yr, "-",month),
         mig_status = factor(mig_status)) 

#prep dataframe
pronghorn.l <- pronghorn %>% 
  mutate(
    fence_density = fence_density*1000, #to bring to similar magnitute and also means km/km2, easier for interpretation
    sin_mo = sin(2*pi*mo/12), 
    cos_mo = cos(2*pi*mo/12)) %>%  #Stolwijk, A. M., H. M. P. M. Straatman, and G. A. Zielhuis. "Studying seasonality by using sine and cosine functions in regression analysis." Journal of Epidemiology & Community Health 53.4 (1999): 235-238.Stolwijk, A. M., H. M. P. M. Straatman, and G. A. Zielhuis. "Studying seasonality by using sine and cosine functions in regression analysis." Journal of Epidemiology & Community Health 53.4 (1999): 235-238.
  left_join(pronghorn.mig.status, by = "id_yr_mo")
ggpairs(pronghorn.l, columns= c(3:9, 11:12)) 

# # #################################################################
# # ############################ models #############################
# # #################################################################
# #  #for each PC, first build univariate random intercept model
# prior_1 <- list(G = list(G1 = list(V = 1, nu=1, alpha.mu= 0, alpha.V=25^2)),
#                             R  = list(V = 1, nu=0.002))
# 
# mcmc_PC1_1 <- MCMCglmm(PC1 ~ fence_density + sin_mo + cos_mo + mig_status,
#                       random = ~id,
#                       rcov = ~units,
#                       family = "gaussian",
#                       prior = prior_1,
#                       nitt=420000,
#                       burnin=20000,
#                       thin=100,
#                       verbose = TRUE,
#                       data = pronghorn.l)
# # #plot(mcmc_PC1_1)
# saveRDS(mcmc_PC1_1, "result/models/mcmc_prong_pc1_randint.RDS")
# # 
# mcmc_PC2_1 <- MCMCglmm(PC2 ~ fence_density + sin_mo + cos_mo + mig_status,
#                        random =~id,
#                        rcov = ~units,
#                        family = "gaussian",
#                        prior = prior_1,
#                        nitt=420000,
#                        burnin=20000,
#                        thin=100,
#                        verbose = TRUE,
#                        data = pronghorn.l)
# # #plot(mcmc_PC2_1)
# saveRDS(mcmc_PC2_1, "result/models/mcmc_prong_pc2_randint.RDS")
# # 
# # # for each PC, then build univariate random regression models
# prior_2 <- list(G =list(G1 = list(V = diag(2), nu = 2, alpha.mu = rep(0,2), alpha.V = diag(25^2,2,2))),
#                   R  = list(V = 1, nu=0.002))
# 
# mcmc_PC1_2 <- MCMCglmm(PC1 ~ fence_density + sin_mo + cos_mo + mig_status,
#                     random =~ us(1 + fence_density):id,
#                     rcov = ~units,
#                     family = "gaussian",
#                     prior = prior_2,
#                     nitt=420000,
#                     burnin=20000,
#                     thin=100,
#                     verbose = TRUE,
#                     data = pronghorn.l,
#                     pr=TRUE,
#                     saveX = TRUE,
#                     saveZ = TRUE)
# # #plot(mcmc_PC1_2)
# saveRDS(mcmc_PC1_2, "result/models/mcmc_prong_pc1_randreg.RDS")
# 
# #
# mcmc_PC2_2 <- MCMCglmm(PC2 ~ fence_density + sin_mo + cos_mo + mig_status,
#                        random =~ us(1 + fence_density):id,
#                        rcov = ~units,
#                        family = "gaussian",
#                        prior = prior_2,
#                        nitt=420000,
#                        burnin=20000,
#                        thin=100,
#                        verbose = TRUE,
#                        data = pronghorn.l,
#                        pr=TRUE,
#                        saveX = TRUE,
#                        saveZ = TRUE)
# # #plot(mcmc_PC2_2)
# saveRDS(mcmc_PC2_2, "result/models/mcmc_prong_pc2_randreg.RDS")

#################################################################
### now compare the random intercept models and the random regression models 
#################################################################
mcmc_PC1_1 <- readRDS("result/models/mcmc_prong_pc1_randint.RDS")
mcmc_PC2_1 <- readRDS("result/models/mcmc_prong_pc2_randint.RDS")
mcmc_PC1_2 <- readRDS("result/models/mcmc_prong_pc1_randreg.RDS")
mcmc_PC2_2 <- readRDS("result/models/mcmc_prong_pc2_randreg.RDS")

DIC = data.frame(PC = rep(c("PC1","PC2"), each = 2), 
                mod = rep(c("random intercept", "random regression"), 2),
                DIC = c(mcmc_PC1_1$DIC, mcmc_PC1_2$DIC, mcmc_PC2_1$DIC, mcmc_PC2_2$DIC))

#write_csv(DIC, "result/midproduct/DIC_riVSrr_pronghorn.csv")
# based on DIC random regression models are better. 

# for  table 1 and s table
ci(mcmc_PC1_2)
p_direction(mcmc_PC1_2)
rope(mcmc_PC1_2)

ci(mcmc_PC2_2)
p_direction(mcmc_PC2_2)
rope(mcmc_PC2_2)

#############################################################
##################### Continuous CondR ######################
#############################################################

######################## condR function ########################
# based on Schielzeth and Nakagawa 2020 
# https://www.biorxiv.org/content/biorxiv/early/2020/03/11/2020.03.11.987073.full.pdf

condR <- function(betaX, meanX, Vx, Vr, Vu, Vv, Cuv, xrange) {
  # Variance explained by covariate x
  Vf     <- betaX^2 * Vx
  # Average between-group variance across x
  Vi     <- Vu + Vv*Vx + meanX^2 * Vv + 2* meanX * Cuv
  # Variance explaind by random slopes
  Vs     <- Vv*Vx + meanX^2 * Vv
  # total phenotypic variance
  Vp     <- Vf + Vi + Vr + Vo
  # R2 for random slopes (in proportion of Vp)
  R2s    <- Vs / Vp
  # Marginalized repeatability (between-group variance across x in propoportion of Vp)
  ## note! this is difference from the conditional R at meanX
  Rmar   <- Vi / Vp
  # Value of covariate x where the between-group variance is at its minimum
  xmin   <- -Cuv/Vv
  # Minimum value of the between-group variance (at xmin)
  minVix <- Vu - Cuv^2/Vv
  # create a dataframe of conditional repeatabilities and covariate
  l1     <- floor(meanX/((xrange[2] - xrange[1])/401))
  x.1 	 <- rep(seq(xrange[1], meanX, length.out = l1), each = 500)
  x.1    <- x.1[!x.1 == meanX]
  x.2    <- rep(seq(meanX, xrange[2], length.out = 401-l1), each = 500)
  condR  <- data.frame(x = c(x.1, x.2)) 
  condR  <- condR %>% mutate(condR.x = Vu + x^2 * Vv + 2* x * Cuv,
                             Vp.x = condR.x + Vr,
                             var.std.condR.x = condR.x/Vp.x)
  return(list(Vf=Vf, Vi=Vi, Vs=Vs, R2s=R2s, Rmar=Rmar, minVix=minVix, xmin=xmin, condR=condR))
}

########### pc 1 ################
mod = mcmc_PC1_2 
dat = pronghorn.l # the dataset used to create mod
# summary(mod)

# target fixed effect estimates
betaX <- mod$Sol[,"fence_density"]
# mean target covariate 
meanX <- mean(dat$fence_density)
# variance of target covariate
Vx <- var(dat$fence_density)
# range of target covariate
xrange <- c(0,2) #2.5 is 95% quantile fence density
# variance random intercept
Vu <- mod$VCV[,"(Intercept):(Intercept).id"]
# variance random slope
Vv <- mod$VCV[,"fence_density:fence_density.id"]
# random intercept*random slope covariance 
Cuv <- mod$VCV[, "fence_density:(Intercept).id"]
# residual variance 
Vr <- mod$VCV[, "units"]
# other variance components (in our case, additional fixed effect)
Vo <- ((mod$Sol[,"mig_status1"])^2 + (mod$Sol[,"sin_mo"])^2 + (mod$Sol[,"cos_mo"])^2) * Vx 

condR.PC1 <- condR(betaX, meanX, Vx, Vr, Vu, Vv, Cuv, xrange)
#write_csv(condR.PC1$condR, "./result/midproduct/condR_continuous_prong_PC1.csv")

########### PC2 ####################
mod <- mcmc_PC2_2 
dat = pronghorn.l # the dataset used to create mod
# summary(mod)

# target fixed effect estimates
betaX <- mod$Sol[,"fence_density"]
# mean target covariate 
meanX <- mean(dat$fence_density)
# variance of target covariate
Vx <- var(dat$fence_density)
# range of target covariate
xrange <- range(dat$fence_density)
xrange <- c(0, 2)
# variance random intercept
Vu <- mod$VCV[,"(Intercept):(Intercept).id"]
# variance random slope
Vv <- mod$VCV[,"fence_density:fence_density.id"]
# random intercept*random slope covariance 
Cuv <- mod$VCV[, "fence_density:(Intercept).id"]
# residual variance 
Vr <- mod$VCV[, "units"]
# other variance components (in our case, additional fixed effect)
Vo <- ((mod$Sol[,"mig_status1"])^2 + (mod$Sol[,"sin_mo"])^2 + (mod$Sol[,"cos_mo"])^2) * Vx 

condR.PC2 <- condR(betaX, meanX, Vx, Vr, Vu, Vv, Cuv, xrange)
#write_csv(condR.PC2$condR, "./result/midproduct/condR_continuous_prong_PC2.csv")

########### marginalized R (averaging across fence density) ######################
mar.r <- data.frame(PC = c("PC1", "PC2"),
                            Rmar = c(mean(condR.PC1$Rmar), mean(condR.PC2$Rmar)),
                            Rmar.lower = c(HPDinterval(condR.PC1$Rmar)[1], HPDinterval(condR.PC2$Rmar)[1]),
                            Rmar.upper = c(HPDinterval(condR.PC1$Rmar)[2], HPDinterval(condR.PC2$Rmar)[2]))
#write_csv(mar.r, "result/midproduct/marR_pronghorn.csv")
