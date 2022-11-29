library(RMark)
library(dplyr)
library(ggplot2)
library(msm)


## MULE DEER ####
## Import data
md_dat<-import.chdata("MD_Survival_input_final_time_varying_10Feb2022.txt", header=T, field.types = rep("n", 493))

##Process data for known-fate analysis
md.dat.processed=process.data(md_dat,model="Known")

##Make design data for known-fate analysis
md.dat.ddl<-make.design.data(md.dat.processed)


## Run models (time varying covariates only)

## Null model
S.dot<-list(formula=~1)
mod_null_md=mark(data=md.dat.processed,ddl=md.dat.ddl,model.parameters=list(S=S.dot))
mod_null_md

## Monthly fence density model
S.fence_month<-list(formula=~Fencem)
mod_fence_month_md<-mark(data=md.dat.processed,ddl=md.dat.ddl,model.parameters=list(S=S.fence_month))

## Monthly total encounters model
S.encount_month<-list(formula=~Encount)
mod_encount_month<-mark(data=md.dat.processed,ddl=md.dat.ddl,model.parameters=list(S=S.encount_month))

##Monthly altered movement model
S.altered_month<-list(formula=~Alter)
mod_alter_month<-mark(data=md.dat.processed, ddl=md.dat.ddl, model.parameters=list(S=S.altered_month))

## AIC table of model
collect.models()

## Fence density figure
##Create new data for prediction
newdat<-data.frame(Fencem2=seq(from=0, to=4, by=0.05))

## Predict using top model
fence_pred<-covariate.predictions(mod_fence_month_md, data=newdat, indices=c(1))

fence_graph<-fence_pred$estimates

ggplot(fence_graph, aes(y=estimate, x=covdata))+
  geom_line()+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.3)+
  theme_classic()+
  ylab("Monthly survival")+
  xlab(expression(km~fence/km^2))
