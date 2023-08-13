#Read me ####
#The purpose of this data wrangle Response and Predictors for 
#Drying subreaches: R1 = > river mile 150, R2 = 130.1 to 150, R2 = 74 to 130
#Dry = 0, Wet = 1
#There are rounding errors after 2019 in the response variables
#Climate variables interpolated

#Libraries ####
library(tidyverse)
library(lubridate)
library(zoo)

#data ####
dat_drying <- read.csv("Data/Processed/2010_2021_WetDryTenths.csv") 

dat_TempPrecip_LosLunas <- read.csv("Data/Raw/TempPrecip_LosLunas_GHCNDUSC00295150.csv") 
dat_TempPrecip_AllOtherLocs <- read.csv("Data/Raw/TempPrecip_AllOtherLocations.csv") # I downloaded this as metric data so no conversion needed

dat_diversions <- read.csv("Data/Processed/MRGCD_diversion.csv")
dat_returns <- read.csv("Data/Processed/MRGCD_returns.csv")
dat_discharge <- read.csv("Data/Processed/USGS_discharge.csv") 

#Reach 1 ####

#Extent and Extent Change

Ext_ExtChng_R1 <- dat_drying %>%
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(DryRM == 0 & RMTenthDry >150) %>% 
  group_by(Date) %>% 
  summarise(Extent = sum(DryRM == 0)/10) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(Extent = replace_na(Extent, 0),
         ExtentChng = Extent - lag(Extent, default = Extent[1])) %>% 
  filter(Date >= "2010-01-01") %>%  arrange(Date)

#Mile Days 
MileDays_R1 <- dat_drying %>% 
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(RMTenthDry >150) %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup(Date) %>% 
  select(!Date)


#Climate covariates

TempPrecip_R1 <- dat_TempPrecip_LosLunas %>% 
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>% 
  filter(DATE <= "2021-12-31") %>% 
  select(STATION, NAME, DATE, PRCP, TMAX, TMIN) %>% 
  rename(TempPrecipStation = STATION, StationName = NAME, Date = DATE,
         Precip_LosLunas_inch = PRCP, TempMax_LosLunas_F = TMAX, TempMin_LosLunas_F = TMIN) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(Precip_LosLunas_mm = round((Precip_LosLunas_inch*25.4),2), 
         TempMax_LosLunas_C1 = TempMax_LosLunas_F-32,
         TempMax_LosLunas_C2 = TempMax_LosLunas_C1*5,
         TempMax_LosLunas_C = TempMax_LosLunas_C2/9,
         TempMax_LosLunas_C = round(TempMax_LosLunas_C,2)) %>% 
  mutate(Precip_LosLunas2_mm = ifelse(is.na(Precip_LosLunas_mm), mean(Precip_LosLunas_mm, na.rm = T), Precip_LosLunas_mm),
         TempMax_LosLunas2_C = na.approx(TempMax_LosLunas_C, na.rm = F)) %>% 
  select(Date, Precip_LosLunas2_mm, TempMax_LosLunas2_C) %>% 
  rename(Precip_mm = Precip_LosLunas2_mm, Temp_C = TempMax_LosLunas2_C) %>% 
  filter(Date >= "2010-01-01") %>% 
  group_by(year(Date)) %>% 
  mutate(PrecipCum_mm = cumsum(Precip_mm), TempCum_C = cumsum(Temp_C)) %>% 
  mutate(PrecipChng_mm = Precip_mm - lag(Precip_mm, default = Precip_mm[1]),
    TempChng_C = Temp_C - lag(Temp_C, default = Temp_C[1])) %>%   
  ungroup() %>% 
  select(Precip_mm, Temp_C, PrecipCum_mm, PrecipChng_mm, TempCum_C, TempChng_C)


#Human covariates 

#Diversions 
Diversions_R1 <- dat_diversions %>% 
  filter(DivName != "SNA02") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  group_by(Date) %>% 
  mutate(TempDiv = round(sum(MnDischarge_cfs, na.rm = T),2)) %>% 
  mutate(Diversion_Isleta_Totalcfs = case_when(is.na(TempDiv) & Month == 1 | Month == 2 | Month == 12 ~ 0,
                                               TRUE ~ TempDiv)) %>% 
  ungroup() %>% 
  filter(Date >= "2010-01-01") %>% 
  arrange(Date) %>% 
  distinct(Date, Diversion_Isleta_Totalcfs) %>% 
  group_by(year(Date)) %>% 
  mutate(DiversionCum_cfs = cumsum(Diversion_Isleta_Totalcfs),
         DiversionChng_cfs = Diversion_Isleta_Totalcfs - lag(Diversion_Isleta_Totalcfs, default = Diversion_Isleta_Totalcfs[1])) %>% 
  ungroup() %>% 
  select(Diversion_Isleta_Totalcfs, DiversionCum_cfs, DiversionChng_cfs) %>% 
  rename(Diversion_cfs = Diversion_Isleta_Totalcfs)


#Returns (ALJWW, 240WW, PERWW) LCZWW, isn't gaged till August 2016
Returns_R1 <- dat_returns %>% 
  filter(DivName == "ALJWW" | DivName == "240WW" |DivName == "PERWW") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  filter(Date >= "2010-01-01") %>% 
  group_by(Date) %>% 
  mutate(TempReturn = sum(MnDischarge_cfs, na.rm = T)) %>% 
  mutate(Returns_cfs = case_when(Month == 1 | Month == 2 | Month == 11 | Month == 12 ~ 0,
                                 TRUE ~ TempReturn)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  distinct(Date, Returns_cfs) %>% 
  group_by(year(Date)) %>% 
  mutate(ReturnsCum_cfs = cumsum(Returns_cfs),
         ReturnsChng_cfs = Returns_cfs - lag(Returns_cfs, default = Returns_cfs[1])) %>% 
  ungroup() %>% 
  select(Returns_cfs, ReturnsCum_cfs, ReturnsChng_cfs)

#Discharge
Discharge_R1 <- dat_discharge %>% 
  mutate(dateTime = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(site_name == "BosqueFarms", dateTime <= "2021-12-31") %>% 
  complete(dateTime = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  filter(dateTime >= "2010-01-01") %>% 
  group_by(year(dateTime)) %>% 
  mutate(DischargeCum_cfs = cumsum(Discharge_cfs),
         DischargeChng_cfs = Discharge_cfs - lag(Discharge_cfs, default = Discharge_cfs[1])) %>% 
  ungroup() %>% 
  select(Discharge_cfs, DischargeCum_cfs, DischargeChng_cfs) 


#R1 combine all data frames 
Reach1 <- as.data.frame(cbind(Ext_ExtChng_R1, MileDays_R1, TempPrecip_R1, 
                              Discharge_R1, Diversions_R1, Returns_R1)) %>% 
  mutate(Reach = "R1")

#Reach 2 ####

#Extent and Extent Change
Ext_ExtChng_R2 <- dat_drying %>%
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(DryRM == 0 & between(RMTenthDry, 130.1, 150)) %>% 
  group_by(Date) %>% 
  summarise(Extent = sum(DryRM == 0)/10) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(Extent = replace_na(Extent, 0),
         ExtentChng = Extent - lag(Extent, default = Extent[1])) %>% 
  filter(Date >= "2010-01-01") %>%  arrange (Date)

#Mile Days 
MileDays_R2 <- dat_drying %>% 
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry, 130, 150)) %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup(Date) %>% 
  select(!Date)

#Climate covariates 

TempPrecip_R2 <- dat_TempPrecip_AllOtherLocs %>% 
  filter(NAME == "BERNARDO, NM US") %>% 
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>% 
  filter(DATE <= "2021-12-31") %>% 
  select(STATION, NAME, DATE, PRCP, TMAX) %>% 
  rename(TempPrecipStation = STATION, StationName = NAME, Date = DATE,
         Precip_mm = PRCP, Temp_C = TMAX) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(Precip_mm = ifelse(is.na(Precip_mm), mean(Precip_mm, na.rm = T), Precip_mm),
         Temp_C = na.approx(Temp_C, na.rm = F)) %>% 
  filter(Date >= "2010-01-01") %>% 
  group_by(year(Date)) %>% 
  mutate(PrecipCum_mm = cumsum(Precip_mm), TempCum_C = cumsum(Temp_C),
         PrecipChng_mm = Precip_mm - lag(Precip_mm, default = Precip_mm[1]),
         TempChng_C = Temp_C - lag(Temp_C, default = Temp_C[1])) %>% 
  ungroup() %>% 
  select(Precip_mm, Temp_C, PrecipCum_mm, PrecipChng_mm, TempCum_C, TempChng_C)


#Diversions (At Isleta)
Diversions_R2 <- Diversions_R1

#Returns river reach 2 (LP1DR, BELDR, LP2DR, FD3WW, STYWW, SABDR) one wasteway not gaged in this reach (NBAWW)
Returns_R2 <- dat_returns %>% 
  filter(DivName == "LP1DR" | DivName == "BELDR" |DivName == "LP2DR" |
           DivName == "FD3WW" | DivName == "STYWW" |DivName == "SABDR" ) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  group_by(Date) %>% 
  mutate(TempReturn = sum(MnDischarge_cfs, na.rm = T)) %>% 
  mutate(Returns_cfs = case_when(Month == 1 | Month == 2 | Month == 11 | Month == 12 ~ 0,
                                      TRUE ~ TempReturn)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  distinct(Date, Returns_cfs) %>% 
  filter(Date >= "2010-01-01") %>% arrange(Date) %>% 
  group_by(year(Date)) %>% 
  mutate(ReturnsCum_cfs = cumsum(Returns_cfs),
         ReturnsChng_cfs = Returns_cfs - lag(Returns_cfs, default = Returns_cfs[1])) %>% 
  ungroup() %>% 
  select(Returns_cfs, ReturnsCum_cfs, ReturnsChng_cfs)

#Returns (gage called At State Hwy 346 near Bosque Farms)
Discharge_R2 <- dat_discharge %>% 
  mutate(dateTime = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(site_no == "8331510" | site_no == "8332010" & dateTime <= "2021-12-31") %>% 
  complete(dateTime = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  filter(dateTime >= "2010-01-01") %>%
  group_by(dateTime) %>% 
  mutate(Discharge_cfs = mean(Discharge_cfs)) %>% 
  ungroup() %>% 
  distinct(dateTime, Discharge_cfs) %>% 
  group_by(year(dateTime)) %>% 
  mutate(DischargeCum_cfs = cumsum(Discharge_cfs),
         DischargeChng_cfs = Discharge_cfs - lag(Discharge_cfs, default = Discharge_cfs[1])) %>% 
  ungroup() %>% 
  select(Discharge_cfs, DischargeCum_cfs, DischargeChng_cfs)


#R2 combine all data frames 
Reach2 <- as.data.frame(cbind(Ext_ExtChng_R2, MileDays_R2, TempPrecip_R2, 
                              Discharge_R2, Diversions_R2, Returns_R2)) %>% 
  mutate(Reach = "R2")

#Reach 3 ####

#Extent and Extent Change
Ext_ExtChng_R3 <- dat_drying %>%
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(DryRM == 0 & between(RMTenthDry, 74, 130)) %>% 
  group_by(Date) %>% 
  summarise(Extent = sum(DryRM == 0)/10) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  filter(Date >= "2010-01-01") %>% 
  mutate(Extent = replace_na(Extent, 0),
         ExtentChng = Extent - lag(Extent, default = Extent[1]))

#Mile Days
MileDays_R3 <- dat_drying %>% 
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry, 74, 130)) %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup() %>% 
  select(!Date)

#Climate covariates 

#Need to average stations
Temp_R3 <- dat_TempPrecip_AllOtherLocs %>% 
  filter(NAME != "BERNARDO, NM US") %>% 
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>% 
  filter(DATE <= "2021-12-31") %>% 
  select(STATION, NAME, DATE, TMAX) %>% 
  group_by(DATE) %>% 
  mutate(Temp_C = mean(TMAX, na.rm = T)) %>% 
  distinct(DATE, Temp_C) %>% 
  dplyr::rename(Date = DATE) %>% 
  ungroup() %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  arrange(Date) %>% 
  mutate(Temp_C = na.approx(Temp_C, na.rm = F)) %>% 
  group_by(year(Date)) %>% 
  mutate(TempCum_C = cumsum(Temp_C),
         TempChng_C = Temp_C - lag(Temp_C, default = Temp_C[1])) %>% 
  ungroup() %>% 
  select(!"year(Date)") %>% 
  filter(Date >= "2010-01-01") %>% arrange(Date) %>% select(!Date)


#Need to sum stations
Precip_R3 <- dat_TempPrecip_AllOtherLocs %>% 
  filter(NAME != "BOSQUE DEL APACHE, NM US") %>% 
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>% 
  filter(DATE <= "2021-12-31") %>% 
  select(STATION, NAME, DATE, PRCP) %>% 
  group_by(DATE) %>% 
  mutate(Precip_mm = sum(PRCP, na.rm = T)) %>% 
  distinct(DATE, Precip_mm) %>% 
  dplyr::rename(Date = DATE) %>% 
  ungroup() %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(Precip_mm = ifelse(is.na(Precip_mm), mean(Precip_mm, na.rm = T), Precip_mm)) %>%
  group_by(year(Date)) %>% 
  mutate(PrecipCum_mm = cumsum(Precip_mm),
         PrecipChng_mm = Precip_mm - lag(Precip_mm, default = Precip_mm[1])) %>% 
  ungroup() %>% 
  filter(Date >= "2010-01-01") %>%  arrange(Date) %>% 
  select(Precip_mm, PrecipCum_mm, PrecipChng_mm)

TempPrecip_R3 <- cbind(Precip_R3, Temp_R3)

#Diversions (San Acadia)
Diversions_R3 <- dat_diversions %>% 
  filter(DivName == "SNA02") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  group_by(Date) %>% 
  mutate(TempDiv = round(sum(MnDischarge_cfs, na.rm = T),2)) %>% 
  mutate(Diversion_cfs = case_when(Month == 1 | Month == 2 | Month == 12 ~ 0,
                                               TRUE ~ TempDiv)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  distinct(Date, Diversion_cfs) %>% 
  filter(Date >= "2010-01-01") %>% arrange(Date) %>% 
  group_by(year(Date)) %>% 
  mutate(DiversionCum_cfs = cumsum(Diversion_cfs),
         DiversionChng_cfs = Diversion_cfs - lag(Diversion_cfs, default = Diversion_cfs[1])) %>% 
  ungroup() %>% 
  select(Diversion_cfs,DiversionCum_cfs, DiversionChng_cfs)

#Returns river reach 3 (LSJDR, SFRDR, NCPPS, NBYPS) three are not gaged in this reach (LJYDR, 9 Mile, NCPDV)
Returns_R3 <- dat_returns %>% 
  filter(DivName == "LSJDR" | DivName == "SFRDR" |DivName == "NCPPS" |
           DivName == "NBYPS") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  group_by(Date) %>% 
  mutate(TempReturn = sum(MnDischarge_cfs, na.rm = T)) %>% 
  mutate(Returns_cfs = case_when(Month == 1 | Month == 2 | Month == 11 | Month == 12 ~ 0,
                                      TRUE ~ TempReturn)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  distinct(Date, Returns_cfs) %>% 
  filter(Date >= "2010-01-01") %>% arrange(Date) %>% 
  group_by(year(Date)) %>% 
  mutate(ReturnsCum_cfs = cumsum(Returns_cfs),
         ReturnsChng_cfs = Returns_cfs - lag(Returns_cfs, default = Returns_cfs[1])) %>% 
  ungroup() %>% 
  select(Returns_cfs, ReturnsCum_cfs, ReturnsChng_cfs)


#There are 3 or possible 4 that I could use and I haven't decided....
Discharge_R3 <- dat_discharge %>% 
  mutate(dateTime = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(site_no == "8354900" | site_no == "8355050" | site_no == "8355490" &
           dateTime <= "2021-12-31") %>% 
  complete(dateTime = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  filter(dateTime >= "2010-01-01") %>% arrange(dateTime) %>%  
  group_by(dateTime) %>% 
  mutate(Discharge_cfs = mean(Discharge_cfs)) %>% 
  ungroup() %>% 
  distinct(dateTime, Discharge_cfs) %>% 
  group_by(year(dateTime)) %>% 
  mutate(DischargeCum_cfs = cumsum(Discharge_cfs),
         DischargeChng_cfs = Discharge_cfs - lag(Discharge_cfs, default = Discharge_cfs[1])) %>% 
  ungroup() %>% 
  select(Discharge_cfs, DischargeCum_cfs, DischargeChng_cfs)
  
#R3 combine all data frames 
Reach3 <- as.data.frame(cbind(Ext_ExtChng_R3, MileDays_R3, TempPrecip_R3, 
                              Discharge_R3, Diversions_R3, Returns_R3)) %>% 
  mutate(Reach = "R3")

#Final dataframe
DryingSubreachData <- rbind(Reach1, Reach2, Reach3)
write.csv(DryingSubreachData, "Data/Processed/DryingSubreachData.csv", row.names = F)
