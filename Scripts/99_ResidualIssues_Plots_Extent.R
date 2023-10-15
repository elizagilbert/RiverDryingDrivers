#read me ####
#the purpose of this script is to better understand the poor residuals
#best Extent Dry model - 2 states diversion subreaches

#libraries ####
library(tidyverse)
library(lubridate)
library(gridExtra)

#read data ####
dat <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d" )) %>% 
  #filter(between(month(Date), 4, 10)) %>% 
  group_by(Reach) %>% 
  mutate(t = row_number()) %>% 
  ungroup()

resids <- read.csv("Data/Processed/PredsExtDiv.csv") %>% 
  rename(Reach = 1)

dat2 <- read.csv("Data/Processed/DryingSubreachData.csv", header=T) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(month(Date), 4, 10))

#wrangle data ####
temp <- dat %>% 
  full_join(resids)

#plots ####
  
  #change from day before against residuals
ggplot(temp, aes(x = ExtentChng, y = Resid))+
  geom_point()+
  facet_grid(rows= vars(Reach))+
  ylab("Extent mod residuals")+
  xlab("Change in extent for those residuals")

  #extent by covariates
dat %>% 
  select(Date, Extent, Precip_mm, Temp_C, Discharge_cfs, Diversion_cfs, Returns_cfs) %>% 
  pivot_longer(Precip_mm:Returns_cfs, names_to = "covs", values_to = "pred_values") %>% 
  ggplot(aes(x = pred_values, y = Extent))+
  geom_point()+
  facet_wrap(covs ~ Reach, scales = "free_x")

   #lead extent by covariates
dat %>%  
  select(Date, Extent, Precip_mm, Temp_C, Discharge_cfs, Diversion_cfs, Returns_cfs) %>% 
  mutate(leadExtent = lead(Extent, 1))%>% #applied 1 - 14 days some response by discharge/return 
  pivot_longer(Precip_mm:Returns_cfs, names_to = "covs", values_to = "pred_values") %>% 
  ggplot(aes(x = pred_values, y = leadExtent))+
  geom_point()+
  facet_wrap(covs ~ Reach, scales = "free_x")

#random
dat %>% 
  select(Date, Extent, Discharge_cfs) %>% 
  ggplot(aes(x = Discharge_cfs, y=Extent))+
  geom_point()+
  facet_wrap(~Reach)+
  scale_x_continuous(limits = (c(0, 750)))

dat %>% 
  select(Date, Extent, Discharge_cfs) %>% 
  pivot_longer(Extent:Discharge_cfs, names_to = "Variables", values_to = "values") %>% 
  ggplot(aes(x = Date, y = values))+
  geom_line()+
  facet_wrap(~Variables, scales = "free_y", ncol = 1)+
  ggtitle("Entire Year")

dat %>% 
  filter(Reach == "R2") %>% 
  ggplot(aes(x = Discharge_cfs, y = Extent))+
  geom_point()+
  geom_path()+
  #facet_wrap(~Reach)+
  scale_x_continuous(limits = (c(0, 100)))

dat %>% 
  ggplot(aes(x = Diversion_cfs, y = Returns_cfs))+
  geom_point()



#change in extent by change in covariates
dat %>% 
  select(Date, Reach, ExtentChng, PrecipChng_mm, TempChng_C, DischargeChng_cfs, DiversionChng_cfs, ReturnsChng_cfs) %>% 
  pivot_longer(PrecipChng_mm:ReturnsChng_cfs, names_to = "covs", values_to = "pred_values") %>% 
  ggplot(aes(x = pred_values, y = ExtentChng))+
  geom_point()+
  facet_wrap(covs ~ Reach, scales = "free_x")

#cumulative variables ####
Pl_Dis <- dat2 %>% 
  select(Date, Reach, MileDays, DischargeCum_cfs) %>% 
  mutate_at(c("MileDays", "DischargeCum_cfs"), scale) %>%
  pivot_longer(MileDays:DischargeCum_cfs, names_to = "Variables", values_to = "values") %>% 
  ggplot(aes(x = Date, y = values, group = Variables, color = Variables))+
  geom_line(size = 1)+
  facet_grid(~Reach, scales = "free_y")+
  scale_color_manual(values = c("Dark Blue", "Red"), name = "", labels = c("Discharge", "Cummulative Drying")) +
  ylab("zscore") +
  ggtitle("Cumulative Drying v Discharge") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

Pl_Div <- dat2 %>% 
  select(Date, Reach, MileDays, DiversionCum_cfs) %>% 
  mutate_at(c("MileDays", "DiversionCum_cfs"), scale) %>%
  pivot_longer(MileDays:DiversionCum_cfs, names_to = "Variables", values_to = "values") %>% 
  ggplot(aes(x = Date, y = values, group = Variables, color = Variables))+
  geom_line(size = 1)+
  facet_grid(~Reach, scales = "free_y")+
  scale_color_manual(values = c("Black", "Red"), name = "", labels = c("Diversion", "Cummulative Drying")) +
  ylab("zscore") +
  ggtitle("Cumulative Drying v Diversion")+
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

Pl_Ret <- dat2 %>% 
  select(Date, Reach, MileDays, ReturnsCum_cfs) %>% 
  mutate_at(c("MileDays", "ReturnsCum_cfs"), scale) %>%
  pivot_longer(MileDays:ReturnsCum_cfs, names_to = "Variables", values_to = "values") %>% 
  ggplot(aes(x = Date, y = values, group = Variables, color = Variables))+
  geom_line(size = 1)+
  facet_grid(~Reach, scales = "free_y")+
  scale_color_manual(values = c("Blue", "Red"), name = "", labels = c("Returns", "Cummulative Drying")) +
  ylab("zscore") +
  ggtitle("Cumulative Drying v Returns")+
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

Pl_Temp <- dat2 %>% 
  select(Date, Reach, MileDays, TempCum_C) %>% 
  mutate_at(c("MileDays", "TempCum_C"), scale) %>%
  pivot_longer(MileDays:TempCum_C, names_to = "Variables", values_to = "values") %>% 
  ggplot(aes(x = Date, y = values, group = Variables, color = Variables))+
  geom_line(size = 1)+
  facet_grid(~Reach, scales = "free_y")+
  scale_color_manual(values = c("Dark Gray", "Red"), name = "", labels = c("Temperature", "Cummulative Drying")) +
  ylab("zscore") +
  ggtitle("Cumulative Drying v Temperature")+
  theme(legend.position = "none")

Pl_Precip <- dat2 %>% 
  select(Date, Reach, MileDays, PrecipCum_mm) %>% 
  mutate_at(c("MileDays", "PrecipCum_mm"), scale) %>%
  pivot_longer(MileDays:PrecipCum_mm, names_to = "Variables", values_to = "values") %>% 
  ggplot(aes(x = Date, y = values, group = Variables, color = Variables))+
  geom_line(size = 1)+
  facet_grid(~Reach, scales = "free_y")+
  scale_color_manual(values = c("Turquoise", "Red"), name = "", labels = c("Rain", "Cummulative Drying")) +
  ylab("zscore") +
  ggtitle("Cumulative Drying v Precipictation")+
  theme(legend.position = "none")

grid.arrange(Pl_Dis, Pl_Div, Pl_Ret, Pl_Temp, Pl_Precip)
  
