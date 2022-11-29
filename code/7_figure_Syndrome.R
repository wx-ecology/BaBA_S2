library(tidyverse)
library(ggplot2)
library(cowplot)
library(GGally)
library(hrbrthemes)
library(gridExtra)

mcmc_cor_prong <- read_csv("./result/midproduct/behaviorsyndrome_prong.csv")
mcmc_cor_deer <- read_csv("./result/midproduct/behaviorsyndrome_deer.csv")

# extract legend from one ggplot
g_legend <- function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}

a <- mcmc_cor_prong %>% 
  mutate(
    Trait1 = str_split(mcmc_cor_prong$Traits, ", ", simplify =  T)[,1],
    Trait2 = str_split(mcmc_cor_prong$Traits, ", ", simplify =  T)[,2],
    mod = ifelse(mod == "lo", "low", ifelse(
      mod == "me", "medium", ifelse(
        mod == "hi", "high", "very high"))),
    mod = factor(mod, levels = c("low", "medium", "high", "very high"))
  ) %>% 
  ggplot(aes(y = Estimate, x = mod, ymin = Lower, ymax = Upper, color = Trait2)) + 
  geom_pointrange(position = position_dodge(width = 0.5), stat = "identity") +
  geom_hline( yintercept = 0, linetype = "dashed") +
  facet_wrap("Trait1") +
#  ggtitle('Pronghorn') +
  scale_color_manual(values=c("#FC766A", "#184A45")) +
  xlab(paste("Fence density")) +
  ylab("trait correlation") +
  theme_ipsum(base_size = 15, axis_title_size = 18) + 
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=16))

b <- mcmc_cor_deer %>% 
  mutate(
    Trait1 = str_split(mcmc_cor_deer$Traits, ", ", simplify =  T)[,1],
    Trait2 = str_split(mcmc_cor_deer$Traits, ", ", simplify =  T)[,2],
    mod = ifelse(mod == "lo", "low", ifelse(
      mod == "me", "medium", ifelse(
        mod == "hi", "high", "very high"))),
    mod = factor(mod, levels = c("low", "medium", "high", "very high"))
  ) %>%
  ggplot(aes(y = Estimate, x = mod, ymin = Lower, ymax = Upper, color = Trait2)) + 
  geom_pointrange(position = position_dodge(width = 0.5), stat = "identity") +
  geom_hline( yintercept = 0, linetype = "dashed") +
  facet_wrap("Trait1") +
#  ggtitle('Mule deer') +
  scale_color_manual(values=c("#FC766A","#184A45")) +
  xlab(paste("Fence density" )) +
  ylab("trait correlation") +
  theme_ipsum(base_size = 15, axis_title_size = 18) + 
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=16))

tiff("figures/materials/Figure4-syndrome.tiff", units="mm", width=330, height=270, res=600)
grid.arrange(arrangeGrob(a + theme(legend.position="none") + theme(plot.margin = unit(c(1,1,.1,.5), "cm")),
                         b + theme(legend.position="none") + theme(plot.margin = unit(c(1,1,.1,.5), "cm")),
                         nrow=2),
             g_legend(a), nrow=2,heights=c(10, 1), padding = unit(0.5, "line"))
dev.off()


