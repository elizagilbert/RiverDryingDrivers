#Read Me ####
#The purpose of this script is to use time series analysis
#to explore daily river drying

#Libraries ####
library(tidyverse)
library(fpp3)

#data ####
dat <- read.csv("Data/Processed/2002_2021_WetDryTenths.csv") %>% 
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

DailyDry <- dat %>% 
  mutate(RM = floor(RMTenthDry)) %>% 
  filter(DryRM == 0) %>% 
  group_by(Date) %>% 
  summarise(NumberDry = sum(DryRM == 0)/10) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(NumberDry = replace_na(NumberDry, 0), Year = year(Date)) %>% 
  relocate(Year, .before = Date) %>% 
  as_tsibble(index = Date)

DailyDry_Change <- dat %>% 
  mutate(RM = floor(RMTenthDry)) %>% 
  filter(DryRM == 0) %>% 
  group_by(Date) %>% 
  summarise(NumberDry = sum(DryRM == 0)/10) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(NumberDry = replace_na(NumberDry, 0), Year = year(Date),
         DiffDry = NumberDry - lag(NumberDry, default = NumberDry[1])) %>% 
  relocate(Year, .before = Date)%>% 
  as_tsibble(index = Date)

#ACF plot daily number of dry river miles and change in distance dry
jpeg("Figures/DailyDistanceDry_ACF.jpeg", units="in", width=8, height=5, res=300)
DailyDry %>%
  ACF(NumberDry, lag_max = 168) %>%
  autoplot() +
  labs(title="Distance Rio Grande dry")+
  theme_classic()
dev.off()

DailyDry_Change %>%
  ACF(DiffDry, lag_max = 168) %>%
  autoplot() +
  labs(title="Distance Rio Grande dry")+
  theme_classic()


#Trend and seasonal decomposition plot daily number of dry river miles and change in distance dry
jpeg("Figures/DailyDistanceDry_STLdecomposition.jpeg", units="in", width=8, height=6, res=300)
DailyDry %>% 
  model(
    STL(NumberDry ~ trend() + season(),
        robust = TRUE)) %>% 
  components() %>% 
  autoplot()+
  labs(title = "Seasonal and Trend decomposition using Loess with seasonal window default = 13")+
  theme_classic()
dev.off()

DailyDry_Change %>% 
  model(
    STL(DiffDry ~ trend() + season(),
        robust = TRUE)) %>% 
  components() %>% 
  autoplot()+
  labs(title = "Seasonal and Trend decomposition using Loess with seasonal window default = 13")+
  theme_classic()
