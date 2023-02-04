#Read me ###
#The purpose of this script is to explore the spatial-temporal nature of
#the RiverEyes Rio Grande river eye drying from the book
#Spatio-temporal statistics with R Wilke et al 2019

#Libraries ###
library(tidyverse)
library(lubridate)
library(sp)
library(spacetime)
library(gstat)

#data ####
dat_drying <- read.csv("Data/Processed/2002_2021_WetDryTenths.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  select(!X)

dat_loc <- read.csv("Data/Raw/WholeRiverMiles_LatLong.csv")

#data wrangle ####
dat2 <- dat_drying %>% 
  filter(between(Date, as.Date("2012-01-01"), as.Date("2012-12-31")))

MnRMDry <- dat_drying %>% 
  mutate(RM = floor(RMTenthDry)) %>% 
  group_by(Date, RM) %>% 
  mutate(MnDryRM = mean(DryRM),
         RM = as.integer(RM)) %>% 
  distinct(Date, RM, MnDryRM) %>% 
  ungroup() 

Cum_DryRM<- dat2 %>% 
  mutate(DryRM2 = case_when(DryRM == 1 ~ 0,
                            DryRM == 0 ~ 1),
         RM = floor(RMTenthDry)) %>% 
  select(-DryRM, -RMTenthDry) %>% 
  group_by(Date, RM) %>% 
  mutate(MnDryRM = round(mean(DryRM2))) %>% 
  distinct(Date, RM, MnDryRM) %>%  
  group_by(RM) %>% 
  mutate(cum_sum = cumsum(MnDryRM)) %>% 
  ungroup()

Loc_cleaned <- dat_loc %>% 
  filter(Name == " " & between(RMNum, 54, 170)) %>% 
  rename(RM = RMNum)

#make space time structure ####
spat_part <- SpatialPoints(coords = Loc_cleaned[, c("Longitude", "Latitude")])
temp_part <- unique(MnRMDry$Date)
STObj_DryRM <- STFDF(sp = spat_part,
                     time = temp_part,
                     data = MnRMDry)


temp_part2 <- unique(Cum_DryRM$Date)
STObj_CumDryRM <- STFDF(sp = spat_part,
                     time = temp_part2,
                     data = Cum_DryRM)
class(STObj_CumDryRM)

#make and plot semi-variogram ####
vv <- variogram(object = cum_sum ~ 1 + Latitude,
                data = STObj_CumDryRM,
                width = 1,
                cutoff = 20,
                tlags = 0.01:15.01)
plot(vv)
