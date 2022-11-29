library(dplyr)
library(ggplot2)
setwd("/Volumes/GoogleDrive/My Drive/RESEARCH/Pronghorn/BaBA_Season2/")

## Pronghorn covariates ####
# prong<-read.csv("./result/pronghorn_cov.csv")
# 
# 
# ggplot(prong, aes(y=Covatiate, x=Estimate))+
#   geom_point()+
#   geom_errorbar(aes(xmin=LCL, xmax=UCL))+
#   geom_vline(xintercept = 0, linetype=2)+
#   theme_classic()+
#   ylab("Covariate")+
#   xlab(expression(beta))


## Effects of fence density on mule deer survival
md<-read.csv("./result/mule_deer_survival_graph_data.csv")

p <- ggplot(md, aes(y=estimate, x=covdata))+
  geom_line()+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.2)+
  theme_classic() +
  geom_vline (xintercept= 0.39, linetype = "dashed", color = "#6e6d6d") +
  geom_vline (xintercept = 0.68, linetype = "dashed", color = "#6e6d6d") +
  geom_vline (xintercept = 1, linetype = "dashed", color = "#6e6d6d") +
  geom_vline (xintercept = 1.49, linetype = "dashed", color = "#6e6d6d") +
  ylim(0.85, 1) +
  ylab("Monthly survival")+
  xlab(paste("Fence density," , expression(km/km^2))) +
  theme_ipsum(base_size = 15, axis_title_size = 18) + 
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=16))

tiff("figures/materials/Figure5-survival.tiff", units="mm", width=220, height=180, res=600)
p
dev.off()
