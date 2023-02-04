#Read me ###
#The purpose of this script is to understand the 
#linearity and dimensions of the river drying data

#Libraries ####
library(rEDM)
library(tidyverse)

#data ####
dat <- read.csv("Data/Processed/2002_2021_WetDryTenths.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  select(!X)

DailyDry <- dat %>% 
  mutate(RM = floor(RMTenthDry)) %>% 
  filter(DryRM == 0) %>% 
  group_by(Date) %>% 
  summarise(NumberDry = sum(DryRM == 0)/10) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(NumberDry = replace_na(NumberDry, 0))

#EDM ####

#Is not monotonic but peak dimension ~ 13
rho_E <- EmbedDimension(dataFrame = DailyDry, lib = "1 2000",
                        pred = "3001 5500", columns = "NumberDry", target = "NumberDry",
                        maxE = 30)
#Train predictions
simplex_out <- Simplex(dataFrame = DailyDry, lib = "1 2000",
                       pred = "3001 5500", columns = "NumberDry", target = "NumberDry", 
                       E=13)
#Get Pearson correlation, MAE and RMSE errors between
#forecast Observations and Predication
#An MAE is return on same scale as target
#score can be interpreted as the distance from the 
#true value
ComputeError(simplex_out$Observations, simplex_out$Predictions)

#prediction decay - this goes down and indicates
#short-term predictions are possible but predictive stat of
#system diluted over time, hindering long-term forecast
#the decline in forecast indicates the system may be
#chaotic
rho_Tp <- PredictInterval(dataFrame = DailyDry, lib = "1 2500",
                          pred = "3001 5500", columns = "NumberDry", target = "NumberDry", 
                          E=13)

#distinguish between red noise and nonlinear
#deterministic behavior using S-maps. If forecasts
#increase then suggestive of nonlinear dynamics wherein
#better forecasts achieved when local linear map can change
#depending on location in state-space (it is a better
#description of state-dependent behavior)
rho_theta <- PredictNonlinear(dataFrame = DailyDry, lib = "1 2500",
                              pred = "3001 5500", columns = "NumberDry", target = "NumberDry", E=13)
