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
tenreachcoeffs <- read.csv("Data/Processed/TenReach95CI_booted.csv")

#Plotting covariate estimates ####

#1 river
mod_1river_params <- MARSSparamCIs(mod_1river)
para_1river <- as.data.frame(broom::tidy(mod_1river_params)) %>% 
  filter(str_detect(term, pattern = "C")) %>% 
  rename(Covariate_name = 1) %>% 
  mutate(Covariate_name = recode(Covariate_name, "C.dis" = "Discharge", "C.div" = "Diversion", "C.precip" = "Precipitation",
                       "C.ret" = "Returns", "C.temp" = "Temperature"))

para_1river$Covariate_name <- factor(para_1river$Covariate_name, levels = c("Discharge", "Returns", "Diversion", "Precipitation", "Temperature"))

pl_river_parms <- ggplot(para_1river, aes(x = estimate, y = Covariate_name))+
  geom_point()+
  geom_errorbar(aes(x = estimate, xmin = conf.low, xmax = conf.up))+
  theme_classic()+ ylab("") + xlab("")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  ggtitle("One reach")+
  scale_x_continuous(limits = c(-1.0, 1.0), breaks = seq(-1.0, 1.0, 0.5))


#2 diversion
mod_2div_params <- MARSSparamCIs(mod_div_2states)
para_2div <- as.data.frame(broom::tidy(mod_2div_params)) %>% 
  filter(str_detect(term, pattern = "C")) %>% 
  rename(Covariate_name = 1) %>% 
  mutate(Covariate_name = recode(Covariate_name, "C.dis" = "Discharge", "C.div" = "Diversion", "C.precip" = "Precipitation",
                                 "C.ret" = "Returns", "C.temp" = "Temperature"))

para_2div$Covariate_name <- factor(para_2div$Covariate_name, levels = c("Discharge", "Returns", "Diversion", "Precipitation", "Temperature"))

pl_div_parms <- ggplot(para_2div, aes(x =estimate , y = Covariate_name ))+
  geom_point()+
  geom_errorbar(aes(x = estimate, xmin = conf.low, xmax = conf.up))+
  theme_classic()+ ylab("Parameter estimate") + xlab("")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme(axis.text.y = element_blank())+
  ggtitle("Two reaches")+ ylab("")

#3 drying
mod_dry_3states <- MARSSparamCIs(mod_dry_3states)
para_3dry<- as.data.frame(broom::tidy(mod_dry_3states)) %>% 
  filter(str_detect(term, pattern = "C")) %>% 
  rename(Covariate_name = 1) %>% 
  mutate(Covariate_name = recode(Covariate_name, "C.dis" = "Discharge", "C.div" = "Diversion", "C.precip" = "Precipitation",
                                 "C.ret" = "Returns", "C.temp" = "Temperature"))

para_3dry$Covariate_name <- factor(para_3dry$Covariate_name, levels = c("Discharge", "Returns", "Diversion", "Precipitation", "Temperature"))

pl_dry_parms <- ggplot(para_3dry, aes(x = estimate, y = Covariate_name))+
  geom_point()+
  geom_errorbar(aes(x = estimate, xmin = conf.low, xmax = conf.up))+
  theme_classic()+ ylab("") + xlab("Parameter estimate")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  ggtitle("Three reaches")+
  scale_x_continuous(limits = c(-0.02, 0.04), breaks = c(-0.02, 0.00, 0.02, 0.04))

#random models
#calculate mean and CIs ####
# load("ModelOutput/Random100/ResultsList5Samples.RData")

# model_params <- lapply(results_list, MARSSparamCIs)
# save(model_params, file = "ModelOutput/Random100/ModelParams100samples.RData")
# load("ModelOutput/Random100/ModelParams100samples.RData")
# 
# model_coefs <- data.frame(lapply(model_params, function (x) `[`(x, c('coef'))))
# 
# para_mean_95CI <- model_coefs %>% 
#   rownames_to_column() %>% 
#   pivot_longer(cols = coef:coef.4,
#                names_to = "Parameter",
#                values_to = "ML_est") %>% 
#   group_by(rowname) %>% 
#   summarise(mean.coef = mean(ML_est, na.rm = TRUE),
#             sd.coef = sd(ML_est, na.rm = TRUE),
#             n.coef = n()) %>%
#   mutate(se.coef = sd.coef / sqrt(n.coef),
#          lower.ci.coef = mean.coef - qt(1 - (0.05 / 2), n.coef - 1) * se.coef,
#          upper.ci.coef = mean.coef + qt(1 - (0.05 / 2), n.coef - 1) * se.coef) %>% 
#   filter(str_detect(rowname, pattern = "C")) %>% 
#   rename(Covariate_name = 1) %>% 
  # mutate(Covariate_name = recode(Covariate_name, "C.dis" = "Discharge", "C.div" = "Diversion", "C.precip" = "Precipitation",
  #                                "C.ret" = "Returns", "C.temp" = "Temperature"))
# 

# para_mean_95CI$Covariate_name <- factor(para_mean_95CI$Covariate_name, levels = c("Discharge", "Returns", "Diversion", "Precipitation", "Temperature"))
tenreachcoeffs <- tenreachcoeffs %>% 
  mutate(Covariate_name = recode(Coefficient, "C.dis" = "Discharge", "C.div" = "Diversion", "C.precip" = "Precipitation",
                                 "C.ret" = "Returns", "C.temp" = "Temperature"))


tenreachcoeffs$Covariate_name <- factor(tenreachcoeffs$Covariate_name, levels = c("Discharge", "Returns", "Diversion", "Precipitation", "Temperature"))

pl_rand_parms <- ggplot(tenreachcoeffs, aes(x = mean, y =  Covariate_name))+
  geom_point()+
  geom_errorbar(aes(x = mean, xmin = lower_ci, xmax = upper_ci))+
  theme_classic()+ ylab("") + xlab("Parameter estimate")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  ggtitle("Ten reaches")+
  theme(axis.text.y = element_blank())+
  scale_x_continuous(limits = c(-0.02, 0.04), breaks = c(-0.02, 0.00, 0.02, 0.04))


tiff("Figures/ModelParameters.jpg", units="in", width=8, height=6, res=300)
ggdraw()+
  draw_plot(pl_river_parms, 0, 0.5, 0.5, 0.5)+
  draw_plot(pl_dry_parms, 0, 0, 0.5, 0.5)+
  draw_plot(pl_div_parms, 0.5, 0.5, 0.4, 0.5)+
  draw_plot(pl_rand_parms, 0.5, 0, 0.4, 0.5)
dev.off()
