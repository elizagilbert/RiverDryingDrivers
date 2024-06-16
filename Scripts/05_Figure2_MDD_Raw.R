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

#data to add dates
#data to add dates
dat <- read.csv("Data/Processed/2010_2021_WetDryTenths.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  select(!X)
dat2 <- dat %>% filter(Date >= "2010-01-01") %>% 
  filter(between(month(Date), 4, 10)) %>% 
  distinct(Date) %>% 
  mutate(t = seq(1,2568,1))

#Fitted versus actual ####
conf_marss1 <- fitted(mod_dry_3states, type = "ytT", interval = "confidence")
pred_marss1 <- fitted(mod_dry_3states, type = "ytT", interval = "prediction")
df1 <- cbind(conf_marss1, pred_marss1[, c(".lwr", ".upr")]) %>% 
  rename(Reach = 1)%>% 
  mutate(across("Reach", str_replace, "R", "SR"))%>% 
  left_join(dat2, by = "t") %>% 
  mutate(Year = year(Date),
         Group = cumsum(month(Date) == 4))

conf_marss2 <- fitted(mod_div_2states, type = "ytT", interval = "confidence")
pred_marss2 <- fitted(mod_div_2states, type = "ytT", interval = "prediction")
df2 <- cbind(conf_marss2, pred_marss2[, c(".lwr", ".upr")]) %>% 
  rename(Reach = 1)%>% 
  mutate(across("Reach", str_replace, "R", "SR")) %>% 
  left_join(dat2, by = "t")%>% 
  mutate(Year = year(Date),
         Group = cumsum(month(Date) == 4))

conf_marss3 <- fitted(mod_1river, type = "ytT", interval = "confidence")
pred_marss3 <- fitted(mod_1river, type = "ytT", interval = "prediction")
df3 <- cbind(conf_marss3, pred_marss3[, c(".lwr", ".upr")]) %>% 
  rename(Reach = 1)%>% 
  mutate(across("Reach", str_replace, "1", "Reach")) %>% 
  cbind(dat2$Date) %>% 
  rename(Date = 10)%>% 
  mutate(Year = year(Date),
         Group = cumsum(month(Date) == 4))

#Plotting ####
#one reach
pl_river <-  ggplot(df3, aes(x = Date, y = y, color = Reach, group = Group))+
  geom_line(size = 1)+
  ggtitle(bquote("One reach"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-1,10), breaks = c(0, 5, 10))+
  ylab("Cumulative km-days dry")+
  xlab("")+
  theme(legend.position = c(0.8, 0.99), legend.direction = "horizontal", axis.text.x = element_blank())+
  scale_color_manual(values = c("#969696"), name = "")

#two reach
pl_diverison <-  ggplot(df2, aes(x = Date, y = y, color = Reach, group = Group))+
  geom_line(size = 1)+
  ggtitle(bquote("Two subreaches"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-1,10), breaks = c(0, 5, 10))+
  ylab("")+
  xlab("")+
  theme(legend.position = c(0.8, 0.99), legend.direction = "horizontal", axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  scale_color_manual(values = c("#969696", "#252525"), name = "")

#three reaches
pl_drying <- ggplot(df1, aes(x = Date, y = y, color = Reach, group = Group))+
  geom_line(size = 1)+
  ggtitle(bquote("Three subreaches"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-1,10), breaks = c(0, 5, 10))+
  ylab("Cumulative km-days dry")+
  xlab("")+
  theme(legend.position = c(0.75, 0.99), legend.direction = "horizontal")+
  scale_color_manual(values = c("#969696", "#252525", "#D9D9D9"), name = "")

#ten reaches
df_ran1 <- as.data.frame(mod_10random_fits[[1]]) %>% 
  mutate(across("Reach", str_replace, "Y", "SR"))%>% 
  left_join(dat2, by = "t")%>% 
  mutate(Year = year(Date),
         Group = cumsum(month(Date) == 4))

df_ran1$Reach <- factor(df_ran1$Reach, levels = c("SR1", "SR2", "SR3", "SR4", "SR5",
                                                  "SR6", "SR7", "SR8", "SR9", "SR10"))

pl_random1 <- ggplot(df_ran1, aes(x = Date, y = y, color = Reach, group = Group))+
  geom_line(size = 1)+
  ggtitle(bquote("Ten subreaches"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-1,10), breaks = c(0, 5, 10))+
  ylab("")+
  xlab("")+
  theme(legend.position = c(0.55, 0.95), legend.direction = "horizontal", axis.text.y = element_blank())+
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
