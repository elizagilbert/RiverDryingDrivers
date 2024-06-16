#Read me ####
#The purpose of this script is to produce figures for the
#manuscript

#Libraries #####
library(tidyverse)
library(gridExtra)
library(cowplot)
library(MARSS)
library(scales)

#Model data ####
mod_dry_3states <- readRDS("ModelOutput/MileDays/MD_3states_dry_uncon_BFGS.rds")
mod_div_2states <- readRDS("ModelOutput/MileDays/MD_2states_div_uncon_BFGS.rds")
mod_1river <- readRDS("ModelOutput/MileDays/MD_1River_BFGS.rds")
mod_1river_null <- readRDS("ModelOutput/MileDays/MD_1River_Null_BFGS.rds")
load("ModelOutput/Random100/ActualFitted100Samples.RData")
mod_10random_fits <-model_dfs

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
         Group = cumsum(month(Date) == 4),
         DifActFit = abs(y - .fitted)+0.0000000000000000000000000000001)

conf_marss2 <- fitted(mod_div_2states, type = "ytT", interval = "confidence")
pred_marss2 <- fitted(mod_div_2states, type = "ytT", interval = "prediction")
df2 <- cbind(conf_marss2, pred_marss2[, c(".lwr", ".upr")]) %>% 
  rename(Reach = 1)%>% 
  mutate(across("Reach", str_replace, "R", "SR")) %>% 
  left_join(dat2, by = "t")%>% 
  mutate(Year = year(Date),
         Group = cumsum(month(Date) == 4),
         DifActFit = abs(y - .fitted)+0.0000000000000000000000000000001)

conf_marss3 <- fitted(mod_1river_null, type = "ytT", interval = "confidence")
pred_marss3 <- fitted(mod_1river_null, type = "ytT", interval = "prediction")
df3 <- cbind(conf_marss3, pred_marss3[, c(".lwr", ".upr")]) %>% 
  rename(Reach = 1) %>% 
  mutate(across("Reach", str_replace, "1", "Reach")) %>% 
  cbind(dat2$Date) %>% 
  rename(Date = 10)%>% 
  mutate(Year = year(Date),
         Group = cumsum(month(Date) == 4),
         DifActFit = abs(y - .fitted)+0.0000000000000000000000000000001)

# Create a list to store the data frames for each model
# model_dfs <- list()
# 
# # Loop through the 100 models to get fitted values
# for (i in 1:100) {
#   conf_marss <- fitted(results_list[[i]], type = "ytT", interval = "confidence")
#   pred_marss <- fitted(results_list[[i]], type = "ytT", interval = "prediction")
#   
#   df <- cbind(conf_marss, pred_marss[, c(".lwr", ".upr")]) %>% rename(Reach = 1)
#   
#   # Store the dataframe in the list
#   model_dfs[[i]] <- df
# }

#save(model_dfs, file = "ModelOutput/Random100/ActualFitted100Samples.RData")

#Plotting fitted v actual ####

#drying reaches
pl_drying2 <- ggplot(df1, aes(x = Date, y = DifActFit), group = Group)+
  geom_line()+
  facet_grid(vars(Reach))+
  ggtitle(bquote("Three subreaches"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(trans = "log10", labels = scientific)+
  ylab("Actual - fitted")+
  xlab("")

pl_drying <- ggplot(df1, aes(x = Date, y = y, group = Group))+
  geom_line(linewidth = 1.25)+
  geom_point(aes(x = Date, y = .fitted), color = "grey", size = 0.75)+
  facet_grid(vars(Reach))+
  ggtitle(bquote("Three subreaches"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(trans = "log2", labels = scientific)+
  ylab("Km-days dry")+
  xlab("")

#diversion reaches
pl_diversion2 <- ggplot(df2, aes(x = Date, y = DifActFit), group = Group)+
  geom_line()+
  facet_grid(vars(Reach))+
  ggtitle(bquote("Two subreaches"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(trans = "log10", labels = scientific)+
  ylab("Actual - fitted")+
  xlab("")

pl_diverison <- ggplot(df2, aes(x = Date, y = y, group = Group))+
  geom_line(linewidth = 1)+
  geom_point(aes(x = Date, y = .fitted), color = "grey", size = 0.75)+
  facet_grid(vars(Reach))+
  ggtitle(bquote("Two subreaches"))+
  theme_classic()+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-1,8), breaks = c(0, 4, 8))+
  ylab("Km-days dry")

#study area
pl_river2 <- ggplot(df3, aes(x = Date, y = DifActFit), group = Group)+
  geom_line()+
  facet_grid(vars(Reach))+
  ggtitle(bquote("One reach"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(trans = "log10", labels = scientific)+
  ylab("Actual - fitted")+
  xlab("")

pl_river <-  ggplot(df3, aes(x = Date, y = y, group = Group)) +
  geom_line(aes(color = "actual"), linewidth = 1) +
  geom_point(aes(x = Date, y = .fitted, color = "fitted"), size = 0.75) +
  ggtitle("One reach") +
  facet_grid(vars(Reach))+
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        legend.position = c(0.80, 0.97), plot.title = element_text(size = 12)) +
  scale_y_continuous(limits = c(-1, 8), breaks = c(0, 4, 8)) +
  scale_color_manual(values = c("actual" = "black", "fitted" = "grey"), 
                     labels = c("Actual", "Predicted")) +
  labs(color = "") +
  ylab("Km-days dry")

#random 1
df_ran1 <- as.data.frame(mod_10random_fits[[1]]) %>% 
  mutate(across("Reach", str_replace, "Y", "SR"))%>% 
  left_join(dat2, by = "t")%>% 
  mutate(Year = year(Date),
         Group = cumsum(month(Date) == 4),
         DifActFit = abs(y - .fitted)+0.0000000000000000000000000000001)

df_ran1$Reach <- factor(df_ran1$Reach, levels = c("SR1", "SR2", "SR3", "SR4", "SR5",
                                                  "SR6", "SR7", "SR8", "SR9", "SR10"))

pl_random2 <- ggplot(df_ran1, aes(x = Date, y = DifActFit), group = Group)+
  geom_line()+
  facet_grid(vars(Reach))+
  ggtitle(bquote("Ten subreach"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(trans = "log10", labels = scientific)+
  ylab("")+
  xlab("")

pl_random1 <- ggplot(df_ran1, aes(x = Date, y = y, group = Group))+
  geom_line(linewidth = 1)+
  geom_point(aes(x = Date, y = .fitted), color = "grey", size = 0.75)+
  facet_grid(vars(Reach))+
  ggtitle("Ten subreaches")+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-2,8), breaks = c(0, 4, 8))+
  ylab("")+
  xlab("")

tiff("Figures/FittedActual.jpg", units="in", width=7.5, height=6, res=300)
ggdraw() +
  draw_plot(pl_river2, x = 0, y = 0.7, width = .5, height = .30) +
  draw_plot(pl_diversion2, x = 0, y = 0.39, width = .5, height = .31) +
  draw_plot(pl_drying2, x = 0, y = 0.0, width = .5, height = .38)+
  draw_plot(pl_random2, x = .5, y = 0, width = .5, height = 1.0)
dev.off()

#RMSE #####
MD_3_RMSE <- sqrt(mean(df1$y - df1$.fitted)^2)
MD_2_RMSE <- sqrt(mean(df2$y - df2$.fitted)^2)
MD_1_RMSE <- sqrt(mean(df3$y - df3$.fitted)^2)

# model_dfs now contains a list of dataframes, one for each model
calculate_RMSE <- function(df) {
  RMSE <- sqrt(mean((df$y - df$.fitted)^2))
  return(RMSE)
}

RMSE_list <- lapply(model_dfs, calculate_RMSE)
mean_RMSE <- mean(unlist(RMSE_list), na.rm = T)
sd_RMSE <- sd(unlist(RMSE_list), na.rm = T)

average_RMSE
sd_RMSE

