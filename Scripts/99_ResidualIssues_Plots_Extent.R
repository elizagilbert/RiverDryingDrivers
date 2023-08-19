#read me ####
#the purpose of this script is to better understand the poor residuals
#best Extent Dry model - 2 states diversion subreaches

#libraries ####
library(tidyverse)

#read data ####
dat <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d" )) %>% 
  filter(between(month(Date), 4, 10)) %>% 
  group_by(Reach) %>% 
  mutate(t = row_number())

resids <- read.csv("Data/Processed/PredsExtDiv.csv") %>% 
  rename(Reach = 1)

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

  #change in extent by change in covariates
dat %>% 
  select(Date, ExtentChng, PrecipChng_mm, TempChng_C, DischargeChng_cfs, DiversionChng_cfs, ReturnsChng_cfs) %>% 
  pivot_longer(PrecipChng_mm:ReturnsChng_cfs, names_to = "covs", values_to = "pred_values") %>% 
  ggplot(aes(x = pred_values, y = ExtentChng))+
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
  ggplot(aes(x = Discharge_cfs, y = Diversion_cfs))+
  geom_point()

dat %>% 
  ggplot(aes(x = Diversion_cfs, y = Returns_cfs))+
  geom_point()
             