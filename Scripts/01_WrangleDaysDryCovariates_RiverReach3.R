#Read me ####
#The purpose of this script is to create a time series and covariates for Alex
#to test in MARSS

#Libraries ####
library(tidyverse)
library(lubridate)
library(zoo)

#data ####
dat_drying <- read.csv("Data/Processed/2002_2021_WetDryTenths.csv") 
dat_TempPrecip <- read.csv("Data/Raw/TempPrecip_AllOtherLocations.csv") #I downloaded this as metric data so no conversion needed
dat_discharge <- read.csv("Data/Processed/USGS_discharge.csv") 
dat_diversions <- read.csv("Data/Processed/MRGCD_diversion.csv")
dat_returns <- read.csv("Data/Processed/MRGCD_returns.csv")

#drying ####
#filter to the upper most dry reach of river

#daily river miles dry (extent - # river miles)
DailyDry_RiverReach3 <- dat_drying %>%
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(DryRM == 0 & between(RMTenthDry, 74, 130)) %>% 
  group_by(Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  rename(RiverReach2_Date = Date) %>% 
  filter(RiverReach2_Date >= "2010-01-01") %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0),
         DiffExtentDry = ExtentDry - lag(ExtentDry, default = ExtentDry[1]))

#daily days dry (duration)
DaysDry_RiverReach3 <- dat_drying %>% 
  select(!X) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry, 74, 130)) %>% 
  arrange(RMTenthDry) %>% 
  group_by(RMTenthDry, grp = with(rle(DryRM), rep(seq_along(lengths), lengths))) %>% 
  mutate(Counter = 1:n()) %>% 
  mutate(Counter = as.numeric(Counter)) %>% 
  ungroup() %>% 
  mutate(Counter2 = case_when(DryRM == 1 ~ 0,
                              TRUE ~ Counter)) %>% 
  select(-grp, - Counter) %>% 
  group_by(Date) %>% 
  summarise(sum(Counter2)) %>% 
  rename(SumDaysDry = "sum(Counter2)") %>% 
  mutate(DiffSumDaysDry = SumDaysDry - lag(SumDaysDry, default = SumDaysDry[1])) %>% 
  arrange(Date) %>% 
  filter(Date >= "2010-01-01") %>% 
  select(-Date)

#daily most upstream dry river mile (location)
DailyLocationDry_RiverReach3 <- dat_drying %>%
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry, 74, 130))%>% 
  arrange(Date, desc(RMTenthDry))  %>% 
  group_by(Date, DryRM) %>% 
  summarise_all(list(~max(RMTenthDry))) %>% 
  as.data.frame() %>% 
  filter(DryRM == 0) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-311"), by = "day")) %>% 
  mutate(UpperLocationDry = RMTenthDry,
         DiffUpperLocationDry = UpperLocationDry - lag(UpperLocationDry, default = UpperLocationDry[1])) %>% 
  arrange(Date) %>% 
  filter(Date >= "2010-01-01") %>% 
  select(UpperLocationDry, DiffUpperLocationDry)

#covariates ####
  #Reach 3 is Socorro 20 N, Socorro, and Bosque Del Apache NOAA stations
   
  #Need to average stations
Temp_Reach3 <- dat_TempPrecip %>% 
  filter(STATION == "USW00003048" | STATION == "USC00298387" | STATION == "USC00291138") %>% 
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>% 
  filter(DATE <= "2021-12-31") %>% 
  select(STATION, NAME, DATE, TMAX) %>% 
  group_by(DATE) %>% 
  mutate(MeanTemp = mean(TMAX, na.rm = T)) %>% 
  distinct(DATE, MeanTemp) %>% 
  dplyr::rename(Date = DATE) %>% 
  ungroup() %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  filter(Date >= "2010-01-01")

  #Need to sum stations
Precip_Reach3 <- dat_TempPrecip %>% 
  filter(STATION == "USW00003048" | STATION == "USC00298387" | STATION == "USC00291138") %>% 
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>% 
  filter(DATE <= "2021-12-31") %>% 
  select(STATION, NAME, DATE, PRCP) %>% 
  group_by(DATE) %>% 
  mutate(SumPrecip = sum(PRCP, na.rm = T)) %>% 
  distinct(DATE, SumPrecip) %>% 
  dplyr::rename(Date = DATE) %>% 
  ungroup() %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  filter(Date >= "2010-01-01")

#   #linear interpolate missing data
# TempLosLunas_red <- TempPrecip_LosLunas_temp %>% 
#   select(Date, TempMax_LosLunas_C) %>% 
#   mutate(TempMax_LosLunas_C = na.approx(TempMax_LosLunas_C, na.rm = F)) %>% 
#   select(TempMax_LosLunas_C)
# 
#  #mean of julian day for missing data
# PrecipLosLunas_red <- TempPrecip_LosLunas_temp %>% 
#   select(Date, Precip_LosLunas_mm) %>% 
#   mutate(JulDay = yday(Date)) %>% 
#   group_by(JulDay) %>% 
#   mutate(Precip_LosLunas_mm = ifelse(is.na(Precip_LosLunas_mm), mean(Precip_LosLunas_mm, na.rm = T), Precip_LosLunas_mm)) %>% 
#   ungroup() %>% 
#   select(Precip_LosLunas_mm)

#There are 3 or possible 4 that I could use and I haven't decided....
Discharge_Reach3 <- dat_discharge %>% 
  mutate(dateTime = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(site_no == "8354900" | site_no == "8355050" | site_no == "8355490" &
           dateTime <= "2021-12-31") %>% 
  complete(dateTime = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  filter(dateTime >= "2010-01-01") %>%
  group_by(dateTime) %>% 
  mutate(MnDischarge_Reach3 = mean(Discharge_cfs)) %>% 
  ungroup() %>% 
  distinct(dateTime, MnDischarge_Reach3) %>% 
  select(MnDischarge_Reach3)

#Diversions river reach 3
Diversions_SanAcaica <- dat_diversions %>% 
  filter(DivName == "SNA02") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  group_by(Date) %>% 
  mutate(TempDiv = round(sum(MnDischarge_cfs, na.rm = T),2)) %>% 
  mutate(Diversion_Isleta_Totalcfs = case_when(Month == 1 | Month == 2 | Month == 12 ~ 0,
                                         TRUE ~ TempDiv)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  distinct(Date, Diversion_Isleta_Totalcfs) %>% 
  filter(Date >= "2010-01-01") %>% 
  select(Diversion_Isleta_Totalcfs)

#Returns river reach 3 (LSJDR, SFRDR, NCPPS, NBYPS) three are not gaged in this reach (NBAWW)
Returns_RiverReach3 <- dat_returns %>% 
  filter(DivName == "LSJDR" | DivName == "SFRDR" |DivName == "NCPPS" |
           DivName == "NBYPS") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  group_by(Date) %>% 
  mutate(TempReturn = sum(MnDischarge_cfs, na.rm = T)) %>% 
  mutate(Returns_Totalcfs = case_when(Month == 1 | Month == 2 | Month == 11 | Month == 12 ~ 0,
                                              TRUE ~ TempReturn)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  distinct(Date, Returns_Totalcfs) %>% 
  filter(Date >= "2010-01-01") %>%
  select(Returns_Totalcfs)


TS_DailyDryReach3_Cov <- as.data.frame(cbind(DailyDry_RiverReach3, DaysDry_RiverReach3, DailyLocationDry_RiverReach3,
                                             Temp_Reach3, Precip_Reach3, Discharge_Reach3, Diversions_SanAcaica,
                                            Returns_RiverReach3))

write.csv(TS_DailyDryReach3_Cov, "Data/Processed/Reach3_ResponseAndCovariates.csv", row.names = F)

#count NAs
Temp <- TempPrecip_Reach2 %>% 
  filter(Date > "2007-12-31") 
sum(is.na(Temp$Precip_Bernardo_mm)) #345
sum(is.na(Temp$TempMax_Bernardo_C)) #507



