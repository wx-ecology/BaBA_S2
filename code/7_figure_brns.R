library(tidyverse);library(patchwork);library(ggbeeswarm);library(hrbrthemes)

# brn figure using Raph's method 
pronghorn.baba <- read_csv("./result/prong_df_monthly.csv") %>% 
  mutate(mo = as.numeric(mo), yr = as.numeric(yr), fence_density = fence_density*1000)
deer.baba <- pronghorn <- read_csv("./result/deer_df_monthly.csv") %>% 
  mutate(mo = as.numeric(mo), yr = as.numeric(yr), fence_density = fence_density*1000)

intPC1.p = lm(PC1 ~ fence_density, data = pronghorn.baba)$coefficients[1]
slopePC1.p = lm(PC1 ~ fence_density, data = pronghorn.baba)$coefficients[2]
intPC2.p = lm(PC2 ~ fence_density, data = pronghorn.baba)$coefficients[1]
slopePC2.p = lm(PC2 ~ fence_density, data = pronghorn.baba)$coefficients[2]

intPC1.d = lm(PC1 ~ fence_density, data = deer.baba)$coefficients[1]
slopePC1.d = lm(PC1 ~ fence_density, data = deer.baba)$coefficients[2]
intPC2.d = lm(PC2 ~ fence_density, data = deer.baba)$coefficients[1]
slopePC2.d = lm(PC2 ~ fence_density, data = deer.baba)$coefficients[2]
# 
# brn.prong.pc1.pt <- pronghorn.baba %>% ggplot(.,aes(x = fence_density, y = PC1, group = id)) +
#   geom_point(alpha = .2, color = "#facb0f") +
#   ylab("PC1 (average movement)") + xlab("fence density (km/km2)") +
#   ylim(-4,2) +
#   xlim(0,6) +
#   theme(legend.position = "none") +
#   theme_ipsum()
# brn.prong.pc2.pt<- pronghorn.baba %>% ggplot(.,aes(x = fence_density, y = PC2, group = id)) +
#   geom_point(alpha = .2, color = "#facb0f") +
#   ylim(-4,2) +
#   xlim(0,6) +
#   ylab("PC2 (altered movement - quick cross)") + xlab("fence density (km/km2)") +
#   theme(legend.position = "none") +
#   theme_ipsum()

# brn.deer.pc1.pt<- deer.baba %>% ggplot(.,aes(x = fence_density, y = PC1, group = id)) + 
#   geom_point(alpha = .2, color = "#ba8caa") +
#   ylab("PC1 (average movement)") + xlab("fence density (km/km2)") +
#   ylim(-4,2) +
#   xlim(0,6) +
#   theme(legend.position = "none") +
#   theme_ipsum()
# brn.deer.pc2.pt<- deer.baba %>% ggplot(.,aes(x = fence_density, y = PC2, group = id)) + 
#   geom_point(alpha = .2, color = "#ba8caa") +
#   ylim(-4,2) +
#   xlim(0,6) +
#   ylab("PC2 (altered movement - quick cross)") + xlab("fence density (km/km2)") +
#   theme(legend.position = "none") +
#   theme_ipsum()


brn.prong.pc1 <- pronghorn.baba %>% ggplot(.,aes(x = fence_density, y = PC1, group = id)) +
  geom_line(stat="smooth", method = "lm",
            alpha = 0.5, color="#FAB90F", size=.6) +
  #geom_abline(intercept = intPC1.p, slope = slopePC1.p, size = .8, color = "#4C4C4C") +
  ylab("PC1") + xlab(paste("Fence density," , expression(km/km^2))) +
  ylim(-4,2) +
  xlim(0,6) +
  theme(legend.position = "none") +
  theme_ipsum(base_size = 15, axis_title_size = 18)

brn.prong.pc2 <- pronghorn.baba %>% ggplot(.,aes(x = fence_density, y = PC2, group = id)) +
  geom_line(stat="smooth", method = "lm",
            alpha = 0.5, color="#FAB90F", size=.6) +
  #geom_abline(intercept = intPC2.p, slope = slopePC2.p, size = .8, color = "#4C4C4C") +
  ylim(-4,2) +
  xlim(0,6) +
  ylab("PC2") + xlab(paste("Fence density," , expression(km/km^2))) +
  theme(legend.position = "none") +
  theme_ipsum(base_size = 15, axis_title_size = 18)

brn.deer.pc1 <- deer.baba %>% ggplot(.,aes(x = fence_density, y = PC1, group = id)) + 
  geom_line(stat="smooth", method = "lm", 
            alpha = 0.5, color="#995C84", size=.6) +
  #geom_abline(intercept = intPC1.d, slope = slopePC1.d, size = .8, color = "#4C4C4C") +
  ylab("PC1") + xlab(paste("Fence density," , expression(km/km^2))) +
  ylim(-4,2) +
  xlim(0,6) +
  theme(legend.position = "none") +
  theme_ipsum(base_size = 15, axis_title_size = 18)

brn.deer.pc2 <- deer.baba %>% ggplot(.,aes(x = fence_density, y = PC2, group = id)) + 
  geom_line(stat="smooth", method = "lm", 
            alpha = 0.5, color="#995C84", size=.6) +
  #geom_abline(intercept = intPC2.d, slope = slopePC2.d, size = .8, color = "#4C4C4C") +
  ylim(-4,2) +
  xlim(0,6) +
  ylab("PC2") + xlab(paste("Fence density," , expression(km/km^2))) +
  theme(legend.position = "none") +
  theme_ipsum(base_size = 15, axis_title_size = 18)

tiff("./figures/materials/Figure2-BRNs-noline.tiff", units="mm", width=330, height=270, res=600)
((brn.prong.pc1 + ggtitle("A"))+(brn.prong.pc2 + ggtitle("B"))) /
  ((brn.deer.pc1 + ggtitle("C")) + (brn.deer.pc2 + ggtitle("D")))
dev.off()

##### DARK MODE ##########

brn.prong.pc1 <- pronghorn.baba %>% ggplot(.,aes(x = fence_density, y = PC1, group = id)) +
  geom_line(stat="smooth", method = "lm",
            alpha = 0.5, color="#FAB90F", size=.6) +
  #geom_abline(intercept = intPC1.p, slope = slopePC1.p, size = .8, color = "#4C4C4C") +
  ylab(" ") + xlab(paste("Fence density," , expression(km/km^2))) +
  ggtitle('Unaltered Behavior Propensity') +
  ylim(-4,2) +
  xlim(0,6) +
  theme(legend.position = "none") +
  theme_ipsum(base_size = 15, axis_title_size = 18) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        # Specify axis options
        axis.line = element_blank(),
        axis.text.x = element_text(size = 16, color = "white", lineheight = 0.9),
        axis.text.y = element_text(size = 16, color = "white", lineheight = 0.9),
        axis.ticks = element_line(color = "white", size  =  0.2),
        axis.title.x = element_text(size = 14, color = "white", margin = margin(0, 10, 0, 0)),
        axis.title.y = element_text(size =14, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
        axis.ticks.length = unit(0.3, "lines"),
        # Specify legend options
        legend.background = element_rect(color = NA, fill = "black"),
        legend.key = element_rect(color = "white",  fill = "black"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = 16, color = "white"),
        legend.text.align = NULL,
        legend.title.align = NULL,
        legend.direction = "vertical",
        legend.box = NULL,
        # Specify panel options
        panel.background = element_rect(fill = "black", color  =  NA),
        panel.border = element_rect(fill = NA, color = "black"),
        panel.grid.major = element_line(color = "grey35"),
        panel.grid.minor = element_line(color = "grey20"),
        #panel.margin = unit(0.5, "lines"),
        # Specify facetting options
        strip.background = element_rect(fill = "grey30", color = "grey10"),
        strip.text.x = element_text(size = 16, color = "white"),
        strip.text.y = element_text(size = 16, color = "white",angle = -90),
        # Specify plot options
        plot.background = element_rect(color = "black", fill = "black"),
        #plot.title = element_text(size = base_size*1.2, color = "white"),
        plot.margin = unit(rep(1, 4), "lines"))

brn.prong.pc2 <- pronghorn.baba %>% ggplot(.,aes(x = fence_density, y = PC2, group = id)) +
  geom_line(stat="smooth", method = "lm",
            alpha = 0.5, color="#FAB90F", size=.6) +
  #geom_abline(intercept = intPC2.p, slope = slopePC2.p, size = .8, color = "#4C4C4C") +
  ylim(-4,2) +
  xlim(0,6) +
  ylab("  ") + xlab(paste("Fence density," , expression(km/km^2))) +
  ggtitle('Quick Cross Tendency') +
  theme(legend.position = "none") +
  theme_ipsum(base_size = 15, axis_title_size = 18) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        # Specify axis options
        axis.line = element_blank(),
        axis.text.x = element_text(size = 16, color = "white", lineheight = 0.9),
        axis.text.y = element_text(size = 16, color = "white", lineheight = 0.9),
        axis.ticks = element_line(color = "white", size  =  0.2),
        axis.title.x = element_text(size = 14, color = "white", margin = margin(0, 10, 0, 0)),
        axis.title.y = element_text(size =14, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
        axis.ticks.length = unit(0.3, "lines"),
        # Specify legend options
        legend.background = element_rect(color = NA, fill = "black"),
        legend.key = element_rect(color = "white",  fill = "black"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = 16, color = "white"),
        legend.text.align = NULL,
        legend.title.align = NULL,
        legend.direction = "vertical",
        legend.box = NULL,
        # Specify panel options
        panel.background = element_rect(fill = "black", color  =  NA),
        panel.border = element_rect(fill = NA, color = "black"),
        panel.grid.major = element_line(color = "grey35"),
        panel.grid.minor = element_line(color = "grey20"),
        #panel.margin = unit(0.5, "lines"),
        # Specify facetting options
        strip.background = element_rect(fill = "grey30", color = "grey10"),
        strip.text.x = element_text(size = 16, color = "white"),
        strip.text.y = element_text(size = 16, color = "white",angle = -90),
        # Specify plot options
        plot.background = element_rect(color = "black", fill = "black"),
        #plot.title = element_text(size = base_size*1.2, color = "white"),
        plot.margin = unit(rep(1, 4), "lines"))
tiff("./figures/materials/dark-Fig2-pronghorn-BRNs.tiff", units="mm", width=330, height=230, res=300)
grid.arrange(brn.prong.pc1, brn.prong.pc2, ncol = 2)
dev.off()
