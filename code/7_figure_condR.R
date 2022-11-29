library(tidyverse)
library(ggplot2)
library(cowplot)
library(GGally)
library(hrbrthemes)
library(ggdist)
library(gridExtra)
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2")
# conditional R plot across fence density 
condR.PC1.prong <- read_csv("./result/midproduct/condR_continuous_prong_PC1.csv")
condR.PC2.prong <- read_csv("./result/midproduct/condR_continuous_prong_PC2.csv")

condR.PC1.deer <- read_csv("./result/midproduct/condR_continuous_deer_PC1.csv")
condR.PC2.deer <- read_csv("./result/midproduct/condR_continuous_deer_PC2.csv")

####################################################
########### plotting GRADIENT ######################
####################################################

a.1 <- condR.PC1.prong %>% ggplot(aes(x = x, y = var.std.condR.x)) +
  geom_point(alpha = .01, size = .5, color = "black") +
  geom_vline(xintercept = c(0.39, 0.68, 1.0, 1.49), linetype = "dashed", color = "#606060", size = 1) +
  ylab ("conditional R") +
  xlab(paste("Fence density," , expression(km/km^2))) +
  ggtitle('A') +
  xlim(0, 2) +
  ylim(0, 1) +
  theme_ipsum(base_size = 15, axis_title_size = 18)

b.1 <- condR.PC2.prong %>% ggplot(aes(x = x, y = var.std.condR.x)) +
  geom_point(alpha = .01, size = .5, color = "black") +
  geom_vline( xintercept = c(0.39, 0.68, 1.0, 1.49), linetype = "dashed", color = "#606060", size = 1) +
  ylab ("conditional R") +
  xlab(paste("Fence density," , expression(km/km^2))) +
  ggtitle('B') +
  xlim(0, 2) +
  ylim(0, 1) +
  theme_ipsum(base_size = 15, axis_title_size = 18)

a.2 <- condR.PC1.deer %>% ggplot(aes(x = x, y = var.std.condR.x)) +
  geom_point(alpha = .01, size = .5, color = "black") +
  geom_vline( xintercept = c(0.39, 0.68, 1.0, 1.49), linetype = "dashed", color = "#606060", size = 1) +
  ylab ("conditional R") +
  xlab(paste("Fence density," , expression(km/km^2))) +
  ggtitle('C') +
  xlim(0, 2) +
  ylim(0, 1) +
  theme_ipsum(base_size = 15, axis_title_size = 18)

b.2 <- condR.PC2.deer %>% ggplot(aes(x = x, y = var.std.condR.x)) +
  geom_point(alpha = .01, size = .5, color = "black") +
  geom_vline( xintercept = c(0.39, 0.68, 1.0, 1.49), linetype = "dashed", color = "#606060", size = 1) +
  ylab ("conditional R") +
  xlab(paste("Fence density," , expression(km/km^2))) +
  ggtitle('D') +
  xlim(0, 2) +
  ylim(0, 1) +
  theme_ipsum(base_size = 15, axis_title_size = 18)

tiff("./figures/materials/Figure3-continuousR.tiff", units="mm", width=330, height=270, res=600)
grid.arrange(a.1, b.1, a.2, b.2, ncol = 2)
dev.off()

####### dark mode #####
# a.1 <- condR.PC1.prong %>% ggplot(aes(x = x, y = var.std.condR.x)) +
#   geom_point(alpha = .05, size = .5, color = "white") +
#   geom_vline(xintercept = c(0.39, 0.68, 1.0, 1.49), linetype = "dashed", color = "white", size = 1) +
#   ylab ("conditional R") +
#   xlab(paste("Fence density," , expression(km/km^2))) +
#   ggtitle('Unaltered Behavior Propensity') +
#   xlim(0, 2) +
#   ylim(0, 1) +
#   theme_ipsum(axis_title_size = 18) +
#   theme(legend.position = "bottom",
#         legend.key.size = unit(1, 'cm'),
#         legend.title = element_blank(),
#         # Specify axis options
#         axis.line = element_blank(),  
#         axis.text.x = element_text(size = 16, color = "white", lineheight = 0.9),  
#         axis.text.y = element_text(size = 16, color = "white", lineheight = 0.9),  
#         axis.ticks = element_line(color = "white", size  =  0.2),  
#         axis.title.x = element_text(size = 14, color = "white", margin = margin(0, 10, 0, 0)),  
#         axis.title.y = element_text(size =14, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
#         axis.ticks.length = unit(0.3, "lines"),   
#         # Specify legend options
#         legend.background = element_rect(color = NA, fill = "black"),  
#         legend.key = element_rect(color = "white",  fill = "black"),  
#         legend.key.height = NULL,  
#         legend.key.width = NULL,      
#         legend.text = element_text(size = 16, color = "white"),  
#         legend.text.align = NULL,  
#         legend.title.align = NULL,  
#         legend.direction = "vertical",  
#         legend.box = NULL, 
#         # Specify panel options
#         panel.background = element_rect(fill = "black", color  =  NA),  
#         panel.border = element_rect(fill = NA, color = "black"),  
#         panel.grid.major = element_line(color = "grey35"),  
#         panel.grid.minor = element_line(color = "grey20"),  
#         #panel.margin = unit(0.5, "lines"),   
#         # Specify facetting options
#         strip.background = element_rect(fill = "grey30", color = "grey10"),  
#         strip.text.x = element_text(size = 16, color = "white"),  
#         strip.text.y = element_text(size = 16, color = "white",angle = -90),  
#         # Specify plot options
#         plot.background = element_rect(color = "black", fill = "black"),  
#         #plot.title = element_text(size = base_size*1.2, color = "white"),  
#         plot.margin = unit(rep(1, 4), "lines"))
# 
# b.1 <- condR.PC2.prong %>% ggplot(aes(x = x, y = var.std.condR.x)) +
#   geom_point(alpha = .05, size = .5, color = "white") +
#   geom_vline( xintercept = c(0.39, 0.68, 1.0, 1.49), linetype = "dashed", color = "white", size = 1) +
#   ylab ("conditional R") +
#   xlab(paste("Fence density," , expression(km/km^2))) +
#   ggtitle('Quick Cross Tendency') +
#   xlim(0, 2) +
#   ylim(0, 1) +
#   theme_ipsum(axis_title_size = 18)+
#   theme(legend.position = "bottom",
#         legend.key.size = unit(1, 'cm'),
#         legend.title = element_blank(),
#         # Specify axis options
#         axis.line = element_blank(),  
#         axis.text.x = element_text(size = 16, color = "white", lineheight = 0.9),  
#         axis.text.y = element_text(size = 16, color = "white", lineheight = 0.9),  
#         axis.ticks = element_line(color = "white", size  =  0.2),  
#         axis.title.x = element_text(size = 14, color = "white", margin = margin(0, 10, 0, 0)),  
#         axis.title.y = element_text(size =14, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
#         axis.ticks.length = unit(0.3, "lines"),   
#         # Specify legend options
#         legend.background = element_rect(color = NA, fill = "black"),  
#         legend.key = element_rect(color = "white",  fill = "black"),  
#         legend.key.height = NULL,  
#         legend.key.width = NULL,      
#         legend.text = element_text(size = 16, color = "white"),  
#         legend.text.align = NULL,  
#         legend.title.align = NULL,  
#         legend.direction = "vertical",  
#         legend.box = NULL, 
#         # Specify panel options
#         panel.background = element_rect(fill = "black", color  =  NA),  
#         panel.border = element_rect(fill = NA, color = "black"),  
#         panel.grid.major = element_line(color = "grey35"),  
#         panel.grid.minor = element_line(color = "grey20"),  
#         #panel.margin = unit(0.5, "lines"),   
#         # Specify facetting options
#         strip.background = element_rect(fill = "grey30", color = "grey10"),  
#         strip.text.x = element_text(size = 16, color = "white"),  
#         strip.text.y = element_text(size = 16, color = "white",angle = -90),  
#         # Specify plot options
#         plot.background = element_rect(color = "black", fill = "black"),  
#         #plot.title = element_text(size = base_size*1.2, color = "white"),  
#         plot.margin = unit(rep(1, 4), "lines"))
# 
# tiff("./figures/materials/dark-Figure3-pronghorn-continuousR.tiff", units="mm", width=330, height=230, res=300)
# grid.arrange(a.1, b.1, ncol = 2)
# dev.off()
# 

# 
# ########### plotting FOUR SCENARIOS ######################
# condR.pronghorn <- read_csv("./result/midproduct/condR_4scenarios_prong.csv")
# condR.deer <- read_csv("./result/midproduct/condR_4scenarios_deer.csv")
# condR <- rbind(condR.pronghorn %>% mutate (spp = "pronghorn"), condR.deer %>% mutate(spp = "mule deer")) %>%
#   mutate(mod = ifelse(mod == "lc", 0.39, 
#                       ifelse (mod == "mc", 0.68, 
#                               ifelse (mod == "hc", 1.0, 1.49))
#                       ),
#          mod = as.numeric(mod),
#          spp = factor(spp, levels = c("pronghorn", "mule deer")))
# 
# a <- condR %>% filter(spp == "pronghorn", Trait == "PC1") %>%
#   ggplot(aes(y = condR.estimate, x = mod, ymin = condR.lower, ymax = condR.upper, color = Trait)) + 
#   geom_pointrange(position = position_dodge(width = 0.5), stat = "identity") +
#   scale_color_manual(values="#FAB90F") +
#   xlab ("fence density (km/km2)") +
#   ggtitle('A') +
#   ylab("conditional R") +
#   xlim(0, 2) +
#   ylim(0, 1) +
#   theme_ipsum(base_size = 15, axis_title_size = 18) +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# b <- condR %>% filter(spp == "pronghorn", Trait == "PC2") %>%
#   ggplot(aes(y = condR.estimate, x = mod, ymin = condR.lower, ymax = condR.upper, color = Trait)) + 
#   geom_pointrange(position = position_dodge(width = 0.5), stat = "identity") +
#   scale_color_manual(values="#FAB90F") +
#   xlab ("fence density (km/km2)") +
#   ggtitle('B') +
#   ylab("conditional R") +
#   xlim(0, 2) +
#   ylim(0, 1) +
#   theme_ipsum(base_size = 15, axis_title_size = 18) +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# c <- condR %>% filter(spp == "mule deer", Trait == "PC1") %>%
#   ggplot(aes(y = condR.estimate, x = mod, ymin = condR.lower, ymax = condR.upper, color = Trait)) + 
#   geom_pointrange(position = position_dodge(width = 0.5), stat = "identity") +
#   scale_color_manual(values="#995C84") +
#   xlab ("fence density (km/km2)") +
#   ggtitle('C') +
#   ylab("conditional R") +
#   xlim(0, 2) +
#   ylim(0, 1) +
#   theme_ipsum(base_size = 15, axis_title_size = 18) +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# d <- condR %>% filter(spp == "mule deer", Trait == "PC2") %>%
#   ggplot(aes(y = condR.estimate, x = mod, ymin = condR.lower, ymax = condR.upper, color = Trait)) + 
#   geom_pointrange(position = position_dodge(width = 0.5), stat = "identity") +
#   scale_color_manual(values="#995C84") +
#   xlab ("fence density (km/km2)") +
#   ggtitle('D') +
#   ylab("conditional R") +
#   xlim(0, 2) +
#   ylim(0, 1) +
#   theme_ipsum(base_size = 15, axis_title_size = 18) +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# tiff("./figures/materials/Figure3-4scenarioR.tiff", units="mm", width=330, height=270, res=600)
# grid.arrange(a,b,c,d, ncol = 2)
# dev.off()

########### plotting FOUR SCENARIOS, V2 ######################
library(wesanderson)
condR.pronghorn <- read_csv("./result/midproduct/condR_allbehavior_4scenarios_prong.csv")
condR.deer <- read_csv("./result/midproduct/condR_allbehavior_4scenarios_deer.csv")
condR <- rbind(condR.pronghorn %>% mutate (spp = "pronghorn"), condR.deer %>% mutate(spp = "mule deer")) %>%
  mutate(mod = ifelse(mod == "lc", "low",
                      ifelse (mod == "mc", "median",
                              ifelse (mod == "hc", "high", "very high"))
  ))

tiff("figures/materials/Appendix-Fig4-condR-allbhv.tiff", units="mm", width=330, height=200, res=600)
condR %>%
  mutate(
    mod = factor(mod, levels = c("low", "median", "high", "very high")),
    spp = factor(spp, levels = c("pronghorn", "mule deer"))
  ) %>%
  ggplot(aes(y = condR.estimate, x = mod, ymin = condR.lower, ymax = condR.upper, color = Trait)) +
  geom_pointrange(position = position_dodge(width = 0.5), stat = "identity") +
  scale_color_manual(values=wes_palette("Zissou1")) +
  facet_wrap("spp") +
  xlab(paste("Fence density," , expression(km/km^2))) +
  ylab("Conditional R") +
  ylim(0, 1) +
  theme_ipsum(base_size = 15, axis_title_size = 18) +
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=16))
dev.off()
