# compare model with random intercept and random regression model to examine plasticity 
# then use conR methods to calculating/visualizing conditional on a (near)continuous scale

#################################################################
########################## set up ###############################
#################################################################
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2")
#setwd("G:/My Drive/RESEARCH/Pronghorn/BaBA_Season2")

library(tidyverse)
library(data.table)
library(MCMCglmm)
library(coda)

deer <- read_csv("./result/deer_df_monthly.csv") %>% 
  mutate(mo = as.numeric(mo), yr = as.numeric(yr)) %>% 
  dplyr::select(id_yr_mo, id, fence_density, mo, yr, PC1, PC2)
length(unique(deer$id_yr_mo)) # 1404 id_mo left
length(unique(deer$id)) # 96 animals

#prep dataframe 
deer.l <- deer %>% 
  mutate(
    fence_density = fence_density*1000, #to bring to similar magnitute and also means km/km2, easier for interpretation
    mo = mo - 1, # so that when calculating intercept, x = 0 makes sense. Otherwise m0 can never be 0 in reality. 
    cos_mo = cos(mo), # using cos and sin to consider the circular nature of month
    sin_mo = sin(mo)
    )
#ggpairs(deer.l, columns= 3:9) 

# #################################################################
# ############################ models #############################
# #################################################################
# #for each PC, build univariate random intercept model and random regression models
# prior_1 <- list(G = list(G1 = list(V = 1, nu=1, alpha.mu= 0, alpha.V=25^2)),
#                             R  = list(V = 1, nu=0.002))
# 
# mcmc_PC1_1 <- MCMCglmm(PC1 ~ fence_density + mo + sin_mo + cos_mo,
#                       random = ~id,
#                       rcov = ~units,
#                       family = "gaussian",
#                       prior = prior_1,
#                       nitt=420000,
#                       burnin=20000,
#                       thin=100,
#                       verbose = TRUE,
#                       data = deer.l)
# plot(mcmc_PC1_1)
# saveRDS(mcmc_PC1_1, "result/mcmc_deer_pc1_randint.RDS")
# 
# mcmc_PC2_1 <- MCMCglmm(PC2 ~ fence_density + mo + sin_mo + cos_mo,
#                        random =~id,
#                        rcov = ~units,
#                        family = "gaussian",
#                        prior = prior_1,
#                        nitt=420000,
#                        burnin=20000,
#                        thin=100,
#                        verbose = TRUE,
#                        data = deer.l)
# plot(mcmc_PC2_1)
# saveRDS(mcmc_PC2_1, "result/mcmc_deer_pc2_randint.RDS")
# 
# prior_2 <- list(G =list(G1 = list(V = diag(2), nu = 2, alpha.mu = rep(0,2), alpha.V = diag(25^2,2,2))),
#                   R  = list(V = 1, nu=0.002))
# 
# mcmc_PC1_2 <- MCMCglmm(PC1 ~ fence_density + mo + sin_mo + cos_mo,
#                     random =~ us(1 + fence_density):id,
#                     rcov = ~units,
#                     family = "gaussian",
#                     prior = prior_2,
#                     nitt=420000,
#                     burnin=20000,
#                     thin=100,
#                     verbose = TRUE,
#                     data = deer.l,
#                     pr=TRUE,
#                     saveX = TRUE,
#                     saveZ = TRUE)
# plot(mcmc_PC1_2)
# saveRDS(mcmc_PC1_2, "result/mcmc_deer_pc1_randreg.RDS")
# 
# mcmc_PC2_2 <- MCMCglmm(PC2 ~ fence_density + mo + sin_mo + cos_mo,
#                        random =~ us(1 + fence_density):id,
#                        rcov = ~units,
#                        family = "gaussian",
#                        prior = prior_2,
#                        nitt=420000,
#                        burnin=20000,
#                        thin=100,
#                        verbose = TRUE,
#                        data = deer.l,
#                        pr=TRUE,
#                        saveX = TRUE,
#                        saveZ = TRUE)
# plot(mcmc_PC2_2)
# saveRDS(mcmc_PC2_2, "result/mcmc_deer_pc2_randreg.RDS")
# 
# #for each PC, build univariate random regression model center fence density at median 
# me <- median(deer.l$fence_density) # 0.6681416
# deer.l <- deer.l %>% mutate(fence_density_mc = fence_density - me)
# 
# mcmc_PC1_2_mc <- MCMCglmm(PC1 ~ fence_density_mc + mo + sin_mo + cos_mo,
#                           random =~ us(1 + fence_density_mc):id,
#                           rcov = ~units,
#                           family = "gaussian",
#                           prior = prior_2,
#                           nitt=420000,
#                           burnin=20000,
#                           thin=100,
#                           verbose = TRUE,
#                           data = deer.l)
# #plot(mcmc_PC1_2_mc)
# saveRDS(mcmc_PC1_2_mc, "result/mcmc_deer_pc1_randreg_mc.RDS")
# 
# mcmc_PC2_2_mc <- MCMCglmm(PC2 ~ fence_density_mc + mo + sin_mo + cos_mo,
#                           random =~ us(1 + fence_density_mc):id,
#                           rcov = ~units,
#                           family = "gaussian",
#                           prior = prior_2,
#                           nitt=420000,
#                           burnin=20000,
#                           thin=100,
#                           verbose = TRUE,
#                           data = deer.l)
# saveRDS(mcmc_PC2_2_mc, "result/mcmc_deer_pc2_randreg_mc.RDS")

#################################################################
### now compare the random intercept models and the random regression models 
#################################################################
# pc 1 
mcmc_PC1_1 <- readRDS("result/mcmc_deer_pc1_randint.RDS")
mcmc_PC2_1 <- readRDS("result/mcmc_deer_pc2_randint.RDS")
mcmc_PC1_2 <- readRDS("result/mcmc_deer_pc1_randreg.RDS")
mcmc_PC2_2 <- readRDS("result/mcmc_deer_pc2_randreg.RDS")

DIC = data.frame(PC = rep(c("PC1","PC2"), each = 2), 
                mod = rep(c("random intercept", "random regression"), 2),
                DIC = c(mcmc_PC1_1$DIC, mcmc_PC1_2$DIC, mcmc_PC2_1$DIC, mcmc_PC2_2$DIC))
# write_csv(DIC, "result/midproduct/DIC_riVSrr_deer.csv")
# based on DIC random regression models are better. 

########################################################################
##################### CONDITIONAL REPEATABILITY  #######################
########################################################################

#################################################################
######################## condR function #########################
#################################################################
# below calculations are based on Schielzeth and Nakagawa 2020 
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
  l1     <- floor(meanX/((xrange[2] - xrange[1])/101))
  x.1 	 <- rep(seq(xrange[1], meanX, length.out = l1), each = 4000)
  x.1    <- x.1[!x.1 == meanX]
  x.2    <- rep(seq(meanX, xrange[2], length.out = 101-l1), each = 4000)
  condR  <- data.frame(x = c(x.1, x.2)) 
  condR  <- condR %>% mutate(condR.x = Vu + x^2 * Vv + 2* x * Cuv,
                             Vp.x = condR.x + Vr,
                             var.std.condR.x = condR.x/Vp.x)
  return(list(Vf=Vf, Vi=Vi, Vs=Vs, R2s=R2s, Rmar=Rmar, minVix=minVix, xmin=xmin, condR=condR))
}


########### pc 1 ################
mod = mcmc_PC1_2 
dat = deer.l # the dataset used to create mod
# summary(mod)

# target fixed effect estimates
betaX <- mod$Sol[,"fence_density"]
# mean target covariate 
meanX <- mean(dat$fence_density)
# variance of target covariate
Vx <- var(dat$fence_density)
# range of target covariate
#xrange <- range(dat$fence_density)
xrange <- c(0,2)
# variance random intercept
Vu <- mod$VCV[,"(Intercept):(Intercept).id"]
# variance random slope
Vv <- mod$VCV[,"fence_density:fence_density.id"]
# random intercept*random slope covariance 
Cuv <- mod$VCV[, "fence_density:(Intercept).id"]
# residual variance 
Vr <- mod$VCV[, "units"]
# other variance components (in our case, additional fixed effect)
Vo <- ((mod$Sol[,"mo"])^2 + (mod$Sol[,"sin_mo"])^2 + (mod$Sol[,"cos_mo"])^2) * Vx 

condR.PC1 <- condR(betaX, meanX, Vx, Vr, Vu, Vv, Cuv, xrange)
# write_csv(condR.PC1$condR, "./result/midproduct/condR_continuous_deer_PC1.csv")

########### PC2 ####################
mod <- mcmc_PC2_2 
dat = deer.l # the dataset used to create mod
# summary(mod)

# target fixed effect estimates
betaX <- mod$Sol[,"fence_density"]
# mean target covariate 
meanX <- mean(dat$fence_density)
# variance of target covariate
Vx <- var(dat$fence_density)
# range of target covariate
#xrange <- range(dat$fence_density)
xrange <- c(0,2)
# variance random intercept
Vu <- mod$VCV[,"(Intercept):(Intercept).id"]
# variance random slope
Vv <- mod$VCV[,"fence_density:fence_density.id"]
# random intercept*random slope covariance 
Cuv <- mod$VCV[, "fence_density:(Intercept).id"]
# residual variance 
Vr <- mod$VCV[, "units"]
# other variance components (in our case, additional fixed effect)
Vo <- ((mod$Sol[,"mo"])^2 + (mod$Sol[,"sin_mo"])^2 + (mod$Sol[,"cos_mo"])^2) * Vx 

condR.PC2 <- condR(betaX, meanX, Vx, Vr, Vu, Vv, Cuv, xrange)
# write_csv(condR.PC2$condR, "./result/midproduct/condR_continuous_deer_PC2.csv")

conditional.r <- data.frame(PC = c("PC1", "PC2"),
                            Rmar = c(mean(condR.PC1$Rmar), mean(condR.PC2$Rmar)),
                            Rmar.lower = c(HPDinterval(condR.PC1$Rmar)[1], HPDinterval(condR.PC2$Rmar)[1]),
                            Rmar.upper = c(HPDinterval(condR.PC1$Rmar)[2], HPDinterval(condR.PC2$Rmar)[2]))
#write_csv(conditional.r, "result/midproduct/marR_deer.csv")
