#Read me ####
#The purpose of this script is to produce figures for the
#manuscript

#Libraries #####
library(tidyverse)
library(gridExtra)
library(cowplot)
library(MARSS)

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
pl_drying <- ggplot(df1, aes(x = t, y = y))+
  geom_line(linewidth = 1.5)+
  geom_point(aes(x = t, y = .fitted), color = "grey", size = 1.5)+
  facet_grid(vars(Reach))+
  ggtitle(bquote("3 drying reaches (RMSE = 1.1 x" ~ 10^-14 * ")"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-1,8), breaks = c(0, 4, 8))+
  ylab("z score")+
  xlab("time point")

#diversion reaches
pl_diverison <- ggplot(df2, aes(x = t, y = y))+
  geom_line(linewidth = 1.5)+
  geom_point(aes(x = t, y = .fitted), color = "grey", size = 1.5)+
  facet_grid(vars(Reach))+
  ggtitle(bquote("2 diversion reaches (RMSE = 9.4 x" ~ 10^-17 * ")"))+
  theme_classic()+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-1,8), breaks = c(0, 4, 8))+
  ylab("z score")

#study area
pl_river <-  ggplot(df3, aes(x = t, y = y)) +
  geom_line(aes(color = "actual"), linewidth = 1.5) +
  geom_point(aes(x = t, y = .fitted, color = "fitted"), size = 1.5) +
  facet_grid(vars(Reach)) +
  ggtitle(bquote("1 river reach (RMSE = 2.5 x" ~ 10^-18 * ")")) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        legend.position = c(0.87, 0.97), plot.title = element_text(size = 12)) +
  scale_y_continuous(limits = c(-1, 8), breaks = c(0, 4, 8)) +
  scale_color_manual(values = c("actual" = "black", "fitted" = "grey")) +
  labs(color = "") +
  ylab("z score")

#random 1
df_ran1 <- as.data.frame(mod_10random_fits[[1]]) %>% 
  mutate(across("Reach", str_replace, "Y", "S"))

df_ran1$Reach <- factor(df_ran1$Reach, levels = c("S1", "S2", "S3", "S4", "S5",
                                                  "S6", "S7", "S8", "S9", "S10"))

pl_random1 <- ggplot(df_ran1, aes(x = t, y = y))+
  geom_line(linewidth = 1.5)+
  geom_point(aes(x = t, y = .fitted), color = "grey", size = 1.5)+
  facet_grid(vars(Reach))+
  ggtitle(bquote("10 4.4 mile sections (RMSE = 2.2 x" ~ 10^-11 * ")"))+
  theme_classic()+
  theme(plot.title = element_text(size = 12))+
  scale_y_continuous(limits = c(-2,8), breaks = c(0, 4, 8))+
  ylab("")+
  xlab("time point")

tiff("Figures/FittedActual.jpg", units="in", width=7.5, height=6, res=300)
ggdraw() +
  draw_plot(pl_river, x = 0, y = 0.7, width = .5, height = .30) +
  draw_plot(pl_diverison, x = 0, y = 0.39, width = .5, height = .31) +
  draw_plot(pl_drying, x = 0, y = 0.0, width = .5, height = .38)+
  draw_plot(pl_random1, x = .5, y = 0, width = .5, height = 1.0)
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

