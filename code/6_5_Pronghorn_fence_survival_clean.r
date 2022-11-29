setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2")
library(RMark)
library(dplyr)
library(ggplot2)
library(msm)

## Import data
dat<-import.chdata("./data/Survival_input_final_time_varying_pronghorn.txt", header=T, field.types = rep("n", 167))

##Process data for known-fate analysis
dat.processed=process.data(dat,model="Known")

##Make design data for known-fate analysis
dat.ddl<-make.design.data(dat.processed)


## Run models (time varying covariates only)

## Null model
S.dot<-list(formula=~1)
mod_null=mark(data=dat.processed,ddl=dat.ddl,model.parameters=list(S=S.dot))
mod_null

## Monthly fence density model
S.fence_month<-list(formula=~Fencem)
mod_fence_month<-mark(data=dat.processed,ddl=dat.ddl,model.parameters=list(S=S.fence_month))

## Monthly total encounters model
S.encount_month<-list(formula=~Encount)
mod_encount_month<-mark(data=dat.processed,ddl=dat.ddl,model.parameters=list(S=S.encount_month))

##Monthly altered movement model
S.altered_month<-list(formula=~Alter)
mod_alter_month<-mark(data=dat.processed, ddl=dat.ddl, model.parameters=list(S=S.altered_month))


## AIC table of models
collect.models()




