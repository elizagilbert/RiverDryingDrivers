#Read me ####
#The purpose of this script is to produce figures for the
#manuscript

#Libraries #####
library(tidyverse)
library(gridExtra)
library(cowplot)
library(MARSS)
library(RColorBrewer)

#Model data ####
mod_dry_3states <- readRDS("ModelOutput/MileDays/MD_3states_dry_uncon_BFGS.rds")
mod_div_2states <- readRDS("ModelOutput/MileDays/MD_2states_div_uncon_BFGS.rds")
mod_1river <- readRDS("ModelOutput/MileDays/MD_1River_BFGS.rds")
load("ModelOutput/Random100/ActualFitted100Samples.RData")
mod_10random_fits <-model_dfs

#Fitted versus actual ####
conf_marss1 <- fitted(mod_dry_3states, type = "ytT", interval = "confidence")
pred_marss1 <- fitted(mod_dry_3states, type = "ytT", interval = "prediction")
df1 <- cbind(conf_marss1, pred_marss1[, c(".lwr", ".upr")]) %>% 
  rename(Reach = 1)

conf_marss2 <- fitted(mod_div_2states, type = "ytT", interval = "confidence")
pred_marss2 <- fitted(mod_div_2states, type = "ytT", interval = "prediction")
df2 <- cbind(conf_marss2, pred_marss2[, c(".lwr", ".upr")]) %>% 
  rename(Reach = 1)

conf_marss3 <- fitted(mod_1river, type = "ytT", interval = "confidence")
pred_marss3 <- fitted(mod_1river, type = "ytT", interval = "prediction")
df3 <- cbind(conf_marss3, pred_marss3[, c(".lwr", ".upr")]) %>% 
  rename(Reach = 1) %>% 
  mutate(across("Reach", str_replace, "1", "R1"))

#Plotting ####
#one reach
pl_river <-  ggplot(df3, aes(x = t, y = y, color = Reach))+
  geom_line(size = 1)+
  ggtitle(bquote("One reach"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-1,8), breaks = c(0, 4, 8))+
  ylab("z score")+
  xlab("")+
  theme(legend.position = c(0.8, 0.99), legend.direction = "horizontal", axis.text.x = element_blank())+
  scale_color_manual(values = c("#969696"), name = "")

#two reach
pl_diverison <-  ggplot(df2, aes(x = t, y = y, color = Reach))+
  geom_line(size = 1)+
  ggtitle(bquote("Two reaches"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-1,8), breaks = c(0, 4, 8))+
  ylab("")+
  xlab("")+
  theme(legend.position = c(0.8, 0.99), legend.direction = "horizontal", axis.text.x = element_blank())+
  scale_color_manual(values = c("#969696", "#252525"), name = "")

#three reaches
pl_drying <- ggplot(df1, aes(x = t, y = y, color = Reach))+
  geom_line(size = 1)+
  ggtitle(bquote("Three reaches"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-1,8), breaks = c(0, 4, 8))+
  ylab("z score")+
  xlab("Time point")+
  theme(legend.position = c(0.75, 0.99), legend.direction = "horizontal")+
  scale_color_manual(values = c("#969696", "#252525", "#D9D9D9"), name = "")

#ten reaches
df_ran1 <- as.data.frame(mod_10random_fits[[1]]) %>% 
  mutate(across("Reach", str_replace, "Y", "R"))

df_ran1$Reach <- factor(df_ran1$Reach, levels = c("R1", "R2", "R3", "R4", "R5",
                                                  "R6", "R7", "R8", "R9", "R10"))

pl_random1 <- ggplot(df_ran1, aes(x = t, y = y, color = Reach))+
  geom_line(size = 1)+
  ggtitle(bquote("Ten reaches"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-1,8), breaks = c(0, 4, 8))+
  ylab("")+
  xlab("Time point")+
  theme(legend.position = c(0.55, 0.95), legend.direction = "horizontal")+
  scale_color_manual(values = c("#969696", "#252525", "#D9D9D9",
                                "#525252" ,"#BDBDBD", "#737373", "#A6CEE3", 
                                "#1F78B4", "#B2DF8A", "#33A02C"), name = "")
pl_random1

# tiff("Figures/RawMMD.jpg", units="in", width=7.5, height=6, res=300)
# ggdraw() +
#   draw_plot(pl_river, x = 0, y = 0.7, width = .5, height = .30) +
#   draw_plot(pl_diverison, x = 0, y = 0.39, width = .5, height = .31) +
#   draw_plot(pl_drying, x = 0, y = 0.0, width = .5, height = .38)+
#   draw_plot(pl_random1, x = .5, y = 0, width = .5, height = 1.0)
# dev.off()

tiff("Figures/RawMMD.jpg", units="in", width=7.5, height=6, res=300)
grid.arrange(pl_river, pl_diverison, pl_drying, pl_random1)
dev.off()
