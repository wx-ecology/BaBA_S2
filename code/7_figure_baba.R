# 6_figure_baba_results
library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(ggplot2)
library(cowplot)
library(gghalves)
library(gridExtra)
library(gganimate)
# library(ggraph)

setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2")

# extract legend from one ggplot
g_legend <- function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}

#################################
######## total encounter ########
#################################

#tiff("figures/total_encounter.tiff", width = 3200, height = 1800, res = 300)
tiff("figures/materials/FigS2-totalencounter.tiff", units="mm", width=330, height=170, res=600)
a <- read_csv("./result/prong_df_monthly.csv") %>%
  mutate(mo = factor(mo)) %>% 
  ggplot( aes ( x = mo, y = total_encounter)) +
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    justification = -.2,
    .width = 0,
    point_color = NA
  ) +
  geom_boxplot(
    width = .12,
    outlier.color = NA
  ) +
  gghalves::geom_half_point (
    side = "l",
    range_scale = .4,
    alpha = .3
  ) + 
  ggtitle('Pronghorn') +
  xlab("month") +
  ylab("total fence encounter") +
  ylim(0,80) +
  theme_ipsum(base_size = 15, axis_title_size = 18)


b <- read_csv("./result/deer_df_monthly.csv") %>%
  mutate(mo = factor(mo)) %>% 
  ggplot( aes ( x = mo, y = total_encounter)) +
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    justification = -.2,
    .width = 0,
    point_color = NA
  ) +
  geom_boxplot(
    width = .12,
    outlier.color = NA
  ) +
  gghalves::geom_half_point (
    side = "l",
    range_scale = .4,
    alpha = .3
  ) + 
  ggtitle('Mule deer') +
  xlab("month") +
  ylab("total fence encounter") +
  ylim(0,80) +
  theme_ipsum(base_size = 15, axis_title_size = 18)

grid.arrange(a, b, ncol = 2)
dev.off()

#################################
######### BaBA results ##########
#################################
tiff("figures/materials/FigS2-baba-results.tiff", units="mm", width=330, height=160, res=600)
a <- read_csv("./result/prong_df_monthly.csv") %>%
  mutate(mo = factor(mo)) %>% 
  dplyr::select(id_yr_mo, mo, Altered_Movement, Average_Movement, Quick_Cross) %>% 
  pivot_longer(cols = 3:5, names_to = "BaBA") %>%
  ggplot( aes ( x = mo, y = value, color= BaBA)) +
  ggdist::stat_pointinterval(
    .width = c(0.5, 0.95),
    position = "dodge"
  ) +
  scale_color_manual(values=c("#FACB0F", "#BA8CAA", "#93B7BE")) +
  ylim(0,40) +
  ggtitle('Pronghorn') +
  xlab("month") +
  ylab("frequency") +
  theme_ipsum(base_size = 15, axis_title_size = 18) +
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=18))

b <- read_csv("./result/deer_df_monthly.csv") %>%
  mutate(mo = factor(mo)) %>% 
  dplyr::select(id_yr_mo, mo, Altered_Movement, Average_Movement, Quick_Cross) %>% 
  pivot_longer(cols = 3:5, names_to = "BaBA") %>%
  ggplot( aes ( x = mo, y = value, color= BaBA)) +
  ggdist::stat_pointinterval(
    position = "dodge",
    .width = c(0.5, 0.95)
  ) +
  scale_color_manual(values=c("#FACB0F", "#BA8CAA", "#93B7BE")) +
  ggtitle('Mule deer') +
  xlab("month") +
  ylab("frequency") +
  ylim(0,40) +
  theme_ipsum(base_size = 15, axis_title_size = 18)

grid.arrange(arrangeGrob(a + theme(legend.position="none"),
                               b + theme(legend.position="none"),
                               nrow=1),
             g_legend(a), nrow=2,heights=c(10, 1))
dev.off()

#################################
####### BaBA animation ##########
#################################
baba.a <- read_csv("./result/BaBA/BaBA_d110max.csv") %>%
  filter(eventTYPE != "unknown") %>%
  mutate(eventTYPE1 = ifelse((eventTYPE == "Quick_Cross" )|(eventTYPE == "Average_Movement"), 
                             eventTYPE, "Altered_Movement"),
         yday = as.integer(yday(start_time))) 

baba.b <- read_csv("./result/BaBA/BaBA_deer_d90max.csv") %>%
  filter(eventTYPE != "unknown") %>%
  mutate(eventTYPE1 = ifelse((eventTYPE == "Quick_Cross" )|(eventTYPE == "Average_Movement"), 
                             eventTYPE, "Altered_Movement"),
         yday = as.integer(yday(start_time))) 

baba <- rbind(baba.a %>% mutate(animal = "Pronghorn"), baba.b %>% mutate(animal = "Deer"))

baba.plot <- baba %>% mutate(animal = factor(animal, levels = c("Pronghorn", "Deer"))) %>% 
  filter(animal == "Pronghorn") %>%
  ggplot(aes(x = easting, y = northing, color = eventTYPE1)) + 
  geom_point() +
  scale_color_manual(values=c( "#FACB0F", "#BA8CAA", "#93B7BE")) +
  theme_ipsum(base_size = 13,
              axis_title_size = 13) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        # Specify axis options
        axis.line = element_blank(),  
        axis.text.x = element_text(size = 12, color = "white", lineheight = 0.9),  
        axis.text.y = element_text(size = 12, color = "white", lineheight = 0.9),  
        axis.ticks = element_line(color = "white", size  =  0.2),  
        axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
        axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
        axis.ticks.length = unit(0.3, "lines"),   
        # Specify legend options
        legend.background = element_rect(color = NA, fill = "black"),  
        legend.key = element_rect(color = "white",  fill = "black"),  
        legend.key.height = NULL,  
        legend.key.width = NULL,      
        legend.text = element_text(size = 12, color = "white"),  
        legend.text.align = NULL,  
        legend.title.align = NULL,  
        legend.direction = "vertical",  
        legend.box = NULL, 
        # Specify panel options
        panel.background = element_rect(fill = "black", color  =  NA),  
        panel.border = element_rect(fill = NA, color = "white"),  
        panel.grid.major = element_line(color = "grey35"),  
        panel.grid.minor = element_line(color = "grey20"),  
        panel.margin = unit(0.5, "lines"),   
        # Specify facetting options
        strip.background = element_rect(fill = "grey30", color = "grey10"),  
        strip.text.x = element_text(size = 12, color = "white"),  
        strip.text.y = element_text(size = 12, color = "white",angle = -90),  
        # Specify plot options
        plot.background = element_rect(color = "black", fill = "black"),  
        #plot.title = element_text(size = base_size*1.2, color = "white"),  
        plot.margin = unit(rep(1, 4), "lines")) +
  facet_wrap("animal")

p <- baba.plot +
  transition_time(yday) +
  labs(title = "Julian day: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)
anim_save("./figures/BaB_AllAnimals.gif", p)


#################################
########## PCA plots ############
#################################

all_pca <- read_csv("./result/BaBA/BaBA_all_pca.csv") 
# pca_loadings <-  readRDS("./result/BaBA/BaBA_all_loadings.RDS" )

atx <- 0.122*min(all_pca$PC1)
aty <- 0.806*min(all_pca$PC2)
avx <- 0.985*max(all_pca$PC1)
avy <- 0
qcx <- 0.125*min(all_pca$PC1)
qcy <- 0.591*max(all_pca$PC1)

arrows  <- cbind(rbind(c(atx,aty), c(avx,avy), c(qcx,qcy)), c(0,0,0), c(0,0,0))

tiff("figures/materials/FigS2-baba-pca.tiff", units="mm", width=220, height=180, res=600)
ggplot(all_pca, aes(PC1, PC2, color = spp)) +
  geom_point(alpha = .5, size = .8) +
  stat_ellipse(size = 1, level = .95) +
  geom_segment(aes(x = 0, y = 0, xend = arrows[1,1], yend = arrows[1,2]),
               arrow = arrow(length = unit(0.4, "cm")), color = gray(.5), size = 1) +
  geom_segment(aes(x = 0, y = 0, xend = arrows[2,1], yend = arrows[2,2]),
               arrow = arrow(length = unit(0.4, "cm")), color = gray(.5), size = 1) +
  geom_segment(aes(x = 0, y = 0, xend = arrows[3,1], yend = arrows[3,2]),
               arrow = arrow(length = unit(0.4, "cm")), color = gray(.5), size = 1) +
  scale_color_manual(values=c("#FAb90f", "#995c84")) +
  xlab("PC1") +
  ylab("PC2") +
  theme_ipsum(base_size = 15, axis_title_size = 18) +
  theme(
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=16)) 
dev.off()

### dark mode 

base_size = 16
tiff("figures/materials/dark-FigS2-baba-pca-noarrow.tiff", units="mm", width=220, height=180, res=300)
ggplot(all_pca, aes(PC1, PC2, color = spp)) +
  geom_point(alpha = .5, size = .8) +
  stat_ellipse(size = 1, level = .95) +
  #geom_segment(aes(x = 0, y = 0, xend = arrows[1,1], yend = arrows[1,2]),
  #             arrow = arrow(length = unit(0.4, "cm")), color = gray(.8), size = 1) +
  #geom_segment(aes(x = 0, y = 0, xend = arrows[2,1], yend = arrows[2,2]),
  #             arrow = arrow(length = unit(0.4, "cm")), color = gray(.8), size = 1) +
  #geom_segment(aes(x = 0, y = 0, xend = arrows[3,1], yend = arrows[3,2]),
  #             arrow = arrow(length = unit(0.4, "cm")), color = gray(.8), size = 1) +
  scale_color_manual(values=c("#FAb90f", "#995c84")) +
  xlab(" ") +
  ylab(" ") +
  theme_ipsum(base_size = 15, axis_title_size = 18) +
  theme(
    legend.key.size = unit(1, 'cm'),
    legend.title = element_blank(),
    legend.text = element_text(size=16)) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        # Specify axis options
        axis.line = element_blank(),  
        axis.text.x = element_text(size = 14, color = "white", lineheight = 0.9),  
        axis.text.y = element_text(size = 14, color = "white", lineheight = 0.9),  
        axis.ticks = element_line(color = "white", size  =  0.2),  
        axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
        axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
        axis.ticks.length = unit(0.3, "lines"),   
        # Specify legend options
        legend.background = element_rect(color = NA, fill = "black"),  
        legend.key = element_rect(color = "white",  fill = "black"),  
        legend.key.height = NULL,  
        legend.key.width = NULL,      
        legend.text = element_text(size = 14, color = "white"),  
        legend.text.align = NULL,  
        legend.title.align = NULL,  
        legend.direction = "horizontal",  
        legend.box = NULL, 
        # Specify panel options
        panel.background = element_rect(fill = "black", color  =  NA),  
        panel.border = element_rect(fill = NA, color = "white"),  
        panel.grid.major = element_line(color = "grey35"),  
        panel.grid.minor = element_line(color = "grey20"),  
        panel.margin = unit(0.5, "lines"),   
        # Specify facetting options
        strip.background = element_rect(fill = "grey30", color = "grey10"),  
        strip.text.x = element_text(size = 14, color = "white"),  
        strip.text.y = element_text(size = 14, color = "white",angle = -90),  
        # Specify plot options
        plot.background = element_rect(color = "black", fill = "black"),  
        #plot.title = element_text(size = base_size*1.2, color = "white"),  
        plot.margin = unit(rep(1, 4), "lines")) 
dev.off()
