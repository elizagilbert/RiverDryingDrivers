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
df_ran1 <- as.data.frame(mod_10random[[1]]) %>% 
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

#Plotting covariate estimates ####

#1 river
mod_1river_params <- MARSSparamCIs(mod_1river)
para_1river <- as.data.frame(broom::tidy(mod_1river_params)) %>% 
  filter(str_detect(term, pattern = "C")) %>% 
  rename(Covariate_name = 1) %>% 
  mutate(Covariate_name = recode(Covariate_name, "C.dis" = "Discharge", "C.div" = "Diversion", "C.precip" = "Precipitation",
                       "C.ret" = "Returns", "C.temp" = "Temperature"))

para_1river$Covariate_name <- factor(para_1river$Covariate_name, levels = c("Discharge", "Diversion", "Returns", "Precipitation", "Temperature"))

pl_river_parms <- ggplot(para_1river, aes(x = Covariate_name, y = estimate))+
  geom_point()+
  geom_errorbar(aes(x = Covariate_name, ymin = conf.low, ymax = conf.up))+
  theme_classic()+ ylab("Estimate") + xlab("")+
  geom_abline(intercept = 0, slope = 0, linetype = "dashed")+
  theme(axis.text.x = element_blank())+
  ggtitle("1 study area")


#2 diversion
mod_2div_params <- MARSSparamCIs(mod_div_2states)
para_2div <- as.data.frame(broom::tidy(mod_2div_params)) %>% 
  filter(str_detect(term, pattern = "C")) %>% 
  rename(Covariate_name = 1) %>% 
  mutate(Covariate_name = recode(Covariate_name, "C.dis" = "Discharge", "C.div" = "Diversion", "C.precip" = "Precipitation",
                                 "C.ret" = "Returns", "C.temp" = "Temperature"))

para_2div$Covariate_name <- factor(para_2div$Covariate_name, levels = c("Discharge", "Diversion", "Returns", "Precipitation", "Temperature"))

pl_div_parms <- ggplot(para_2div, aes(x = Covariate_name, y = estimate))+
  geom_point()+
  geom_errorbar(aes(x = Covariate_name, ymin = conf.low, ymax = conf.up))+
  theme_classic()+ ylab("Estimate") + xlab("")+
  geom_abline(intercept = 0, slope = 0, linetype = "dashed")+
  theme(axis.text.x = element_blank())+
  ggtitle("2 diversion reaches")+ ylab("")

#3 drying
mod_dry_3states <- MARSSparamCIs(mod_dry_3states)
para_3dry<- as.data.frame(broom::tidy(mod_dry_3states)) %>% 
  filter(str_detect(term, pattern = "C")) %>% 
  rename(Covariate_name = 1) %>% 
  mutate(Covariate_name = recode(Covariate_name, "C.dis" = "Discharge", "C.div" = "Diversion", "C.precip" = "Precipitation",
                                 "C.ret" = "Returns", "C.temp" = "Temperature"))

para_3dry$Covariate_name <- factor(para_3dry$Covariate_name, levels = c("Discharge", "Diversion", "Returns", "Precipitation", "Temperature"))

pl_dry_parms <- ggplot(para_3dry, aes(x = Covariate_name, y = estimate))+
  geom_point()+
  geom_errorbar(aes(x = Covariate_name, ymin = conf.low, ymax = conf.up))+
  theme_classic()+ ylab("Estimate") + xlab("")+
  geom_abline(intercept = 0, slope = 0, linetype = "dashed")+
  ggtitle("3 drying reaches")+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(limits = c(-0.02, 0.04), breaks = c(-0.02, 0.00, 0.02, 0.04))

#random models
#calculate mean and CIs ####
# load("ModelOutput/Random100/ResultsList5Samples.RData")

# model_params <- lapply(results_list, MARSSparamCIs)
# save(model_params, file = "ModelOutput/Random100/ModelParams100samples.RData")
load("ModelOutput/Random100/ModelParams100samples.RData")

model_coefs <- data.frame(lapply(model_params, function (x) `[`(x, c('coef'))))

para_mean_95CI <- model_coefs %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = coef:coef.4,
               names_to = "Parameter",
               values_to = "ML_est") %>% 
  group_by(rowname) %>% 
  summarise(mean.coef = mean(ML_est, na.rm = TRUE),
            sd.coef = sd(ML_est, na.rm = TRUE),
            n.coef = n()) %>%
  mutate(se.coef = sd.coef / sqrt(n.coef),
         lower.ci.coef = mean.coef - qt(1 - (0.05 / 2), n.coef - 1) * se.coef,
         upper.ci.coef = mean.coef + qt(1 - (0.05 / 2), n.coef - 1) * se.coef) %>% 
  filter(str_detect(rowname, pattern = "C")) %>% 
  rename(Covariate_name = 1) %>% 
  mutate(Covariate_name = recode(Covariate_name, "C.dis" = "Discharge", "C.div" = "Diversion", "C.precip" = "Precipitation",
                                 "C.ret" = "Returns", "C.temp" = "Temperature"))

para_mean_95CI$Covariate_name <- factor(para_mean_95CI$Covariate_name, levels = c("Discharge", "Diversion", "Returns", "Precipitation", "Temperature"))

pl_rand_parms <- ggplot(para_mean_95CI, aes(x = Covariate_name, y = mean.coef))+
  geom_point()+
  geom_errorbar(aes(x = Covariate_name, ymin = lower.ci.coef, ymax = upper.ci.coef))+
  theme_classic()+ ylab("Estimate") + xlab("")+
  geom_abline(intercept = 0, slope = 0, linetype = "dashed")+
  ggtitle("10 4.4 river mile sections")+
  theme(axis.text.x = element_text(angle = 90))+ ylab("")

tiff("Figures/ModelParameters.jpg", units="in", width=6, height=6, res=300)
grid.arrange(pl_river_parms, pl_div_parms, pl_dry_parms, pl_rand_parms, ncol=2)
dev.off()
