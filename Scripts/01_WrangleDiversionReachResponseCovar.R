#Read me ####
#The purpose of this data wrangle Response and Predictors for 
#Diversion reaches: R1 = > river mile 116, R2 = 116 to 74
#Dry = 0, Wet = 1

#Libraries ####
library(tidyverse)
library(lubridate)
library(zoo)

#data ####
dat_drying <- read.csv("Data/Processed/2010_2021_WetDryTenths.csv")

dat_TempPrecip_LosLunas <- read.csv("Data/Raw/TempPrecip_LosLunas_GHCNDUSC00295150.csv") %>%
  select("STATION", "NAME", "DATE", "PRCP", "TMAX","TMIN") %>% 
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>% 
  filter(DATE <= "2021-12-31") %>% 
  select(STATION, NAME, DATE, PRCP, TMAX, TMIN) %>% 
  rename(TempPrecipStation = STATION, Date = DATE,
         Precip_LosLunas_inch = PRCP, TempMax_LosLunas_F = TMAX, TempMin_LosLunas_F = TMIN) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(Precip_LosLunas_mm = round((Precip_LosLunas_inch*25.4),2), 
         TempMax_LosLunas_C1 = TempMax_LosLunas_F-32,
         TempMax_LosLunas_C2 = TempMax_LosLunas_C1*5,
         TempMax_LosLunas_C = TempMax_LosLunas_C2/9,
         TempMax_LosLunas_C = round(TempMax_LosLunas_C,2)) %>% 
  select(Date, NAME, Precip_LosLunas_mm, TempMax_LosLunas_C) %>% 
  rename(Precip_mm = Precip_LosLunas_mm, Temp_C = TempMax_LosLunas_C) %>% 
  filter(Date >= "2010-01-01") 

dat_TempPrecip_AllOtherLocs <- read.csv("Data/Raw/TempPrecip_AllOtherLocations.csv") %>%  #This is R2 and I downloaded this as metric data so no conversion needed
  select("STATION", "NAME", "DATE", "PRCP", "TMAX","TMIN")%>% 
  select(DATE, NAME, PRCP, TMAX) %>% 
  rename(Date = DATE, Precip_mm = PRCP, Temp_C = TMAX)

dat_TempPrecip_All <- rbind(dat_TempPrecip_LosLunas,
                            dat_TempPrecip_AllOtherLocs)

dat_diversions <- read.csv("Data/Processed/MRGCD_diversion.csv")
dat_returns <- read.csv("Data/Processed/MRGCD_returns.csv")
dat_discharge <- read.csv("Data/Processed/USGS_discharge.csv")

#Reach 1 ####

#Extent and Extent Change

Ext_ExtChng_R1 <- dat_drying %>%
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(DryRM == 0 & RMTenthDry >116) %>% 
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
  filter(RMTenthDry >116, Date >= "2010-01-01") %>% 
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

#Need to average stations
Temp_R1 <- dat_TempPrecip_All %>% 
  filter(NAME == "SOCORRO 20 N, NM US" | NAME == "LOS LUNAS 3 SSW, NM US" | NAME == "BERNARDO, NM US") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  group_by(NAME) %>% 
  mutate(Temp_C0 = na.approx(Temp_C, na.rm = F)) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(Temp_C1 = mean(Temp_C0, na.rm = T)) %>% 
  ungroup() %>% 
  mutate_at("Temp_C1", ~ifelse(is.nan(.), NA, .)) %>% 
  distinct(Date, Temp_C1) %>% 
  arrange(Date) %>% 
  mutate(Temp_C = na.approx(Temp_C1)) %>% 
  distinct(Date, Temp_C) %>% 
  dplyr::rename(Date = Date) %>% 
  filter(Date >= "2010-01-01") %>% arrange(Date) %>% 
  group_by(year(Date)) %>% 
  mutate(TempCum_C = cumsum(Temp_C),
         TempChng_C = Temp_C - lag(Temp_C, default = Temp_C[1])) %>% 
  ungroup() %>% 
  select(Temp_C, TempCum_C, TempChng_C)


#Need to sum stations
Precip_R1 <- dat_TempPrecip_All %>% 
  filter(NAME == "SOCORRO 20 N, NM US" | NAME == "LOS LUNAS 3 SSW, NM US" | NAME == "BERNARDO, NM US") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Date <= "2021-12-31") %>% 
  group_by(Date) %>% 
  mutate(Precip_mm1 = sum(Precip_mm, na.rm = T)) %>% 
  distinct(Date, Precip_mm1) %>% 
  ungroup() %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  arrange(Date) %>% 
  mutate(Precip_mm =na.approx(Precip_mm1)) %>% 
  select(!Precip_mm1) %>% 
  filter(Date >= "2010-01-01") %>%  arrange(Date) %>% 
  group_by(year(Date)) %>% 
  mutate(PrecipCum_mm = cumsum(Precip_mm),
         PrecipChng_mm = Precip_mm - lag(Precip_mm, default = Precip_mm[1])) %>% 
  ungroup() %>% 
  select(Precip_mm, PrecipCum_mm, PrecipChng_mm)


TempPrecip_R1 <- cbind(Precip_R1, Temp_R1)

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


#Returns 
Returns_R1 <- dat_returns %>% 
  filter(DivName != "NCPPS" | DivName != "NBYPS" |DivName != "FCRPS") %>% 
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
  filter(site_name == "BosqueFarms" | site_name == "StateHwy346" | site_name == "Bernardo"
         & dateTime <= "2021-12-31") %>% 
  complete(dateTime = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  filter(dateTime >= "2010-01-01") %>%
  group_by(dateTime) %>% 
  mutate(Discharge_cfs = mean(Discharge_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(dateTime, Discharge_cfs) %>% 
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
  filter(DryRM == 0 & between(RMTenthDry,74,116)) %>% 
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
  filter(between(RMTenthDry,74,116), Date >= "2010-01-01") %>% 
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
Temp_R2 <- dat_TempPrecip_All %>% 
  filter(NAME == "BOSQUE DEL APACHE, NM US" | NAME == "SOCORRO, NM US") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  group_by(NAME) %>% 
  mutate(Temp_C0 = na.approx(Temp_C, na.rm = F)) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(Temp_C1 = mean(Temp_C0, na.rm = T)) %>% 
  ungroup() %>% 
  mutate_at("Temp_C1", ~ifelse(is.nan(.), NA, .)) %>% 
  distinct(Date, Temp_C1) %>% 
  arrange(Date) %>% 
  mutate(Temp_C = na.approx(Temp_C1)) %>% 
  distinct(Date, Temp_C) %>% 
  dplyr::rename(Date = Date) %>% 
  filter(Date >= "2010-01-01") %>% arrange(Date) %>%
  group_by(year(Date)) %>% 
  mutate(TempCum_C = cumsum(Temp_C),
         TempChng_C = Temp_C - lag(Temp_C, default = Temp_C[1])) %>% 
  ungroup() %>% 
  select(Temp_C, TempCum_C, TempChng_C)
  

#Need to sum stations
Precip_R2  <- dat_TempPrecip_All %>% 
  filter(NAME == "BOSQUE DEL APACHE, NM US" | NAME == "SOCORRO, NM US") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  group_by(NAME) %>% 
  mutate(Precip0 = na.approx(Precip_mm, na.rm = F)) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(Preci1 = sum(Precip0, na.rm = T)) %>% 
  ungroup() %>% 
  mutate_at("Preci1", ~ifelse(is.nan(.), NA, .)) %>% 
  distinct(Date, Preci1) %>% 
  arrange(Date) %>% 
  mutate(Precip_mm= na.approx(Preci1)) %>% 
  distinct(Date, Precip_mm) %>% 
  dplyr::rename(Date = Date) %>% 
  filter(Date >= "2010-01-01") %>% arrange(Date) %>%
  group_by(year(Date)) %>% 
  mutate(PrecipCum_mm = cumsum(Precip_mm),
         PrecipChng_mm = Precip_mm - lag(Precip_mm, default = Precip_mm[1])) %>% 
  ungroup() %>% 
  select(Precip_mm, PrecipCum_mm,PrecipChng_mm)
  
TempPrecip_R2 <- cbind(Precip_R2, Temp_R2)


#Diversions (At Isleta)
Diversions_R2 <- dat_diversions %>% 
  filter(DivName == "SNA02") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  group_by(Date) %>% 
  mutate(TempDiv = round(sum(MnDischarge_cfs, na.rm = T),2)) %>% 
  mutate(Diversion_cfs = case_when(Month == 1 | Month == 2 | Month == 12 ~ 0,
                                               TRUE ~ TempDiv)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  filter(Date >= "2010-01-01") %>% 
  distinct(Date, Diversion_cfs) %>% 
  arrange(Date) %>% 
  group_by(year(Date)) %>% 
  mutate(DiversionCum_cfs = cumsum(Diversion_cfs),
         DiversionChng_cfs = Diversion_cfs - lag(Diversion_cfs, default = Diversion_cfs[1])) %>% 
  ungroup() %>% 
  select(Diversion_cfs, DiversionCum_cfs, DiversionChng_cfs)

#Returns river reach 2 
Returns_R2 <- dat_returns %>% 
  filter(DivName == "NCPPS" | DivName == "NBYPS" |DivName == "FCRPS") %>% 
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

#Discharge (gage called At State Hwy 346 near Bosque Farms)
Discharge_R2 <- dat_discharge %>% 
  mutate(dateTime = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(site_name == "San Acacia" | site_name == "Escondida" | site_name == "SanAntonio" 
         & dateTime <= "2021-12-31") %>% 
  complete(dateTime = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  filter(dateTime >= "2010-01-01") %>%
  group_by(site_name) %>% 
  mutate(Discharge_cfs0 = na.approx(Discharge_cfs, na.rm = F)) %>%
  ungroup() %>% 
  group_by(dateTime) %>% 
  mutate(Discharge_cfs2 = mean(Discharge_cfs0)) %>% 
  ungroup() %>% 
  distinct(dateTime, Discharge_cfs2) %>% 
  rename(Discharge_cfs = Discharge_cfs2) %>% 
  group_by(year(dateTime)) %>% 
  mutate(DischargeCum_cfs = cumsum(Discharge_cfs),
         DischargeChng_cfs = Discharge_cfs - lag(Discharge_cfs, default = Discharge_cfs[1])) %>% 
  ungroup() %>% 
  select(Discharge_cfs, DischargeCum_cfs, DischargeChng_cfs)

#R2 combine all data frames 
Reach2 <- as.data.frame(cbind(Ext_ExtChng_R2, MileDays_R2, TempPrecip_R2, 
                              Discharge_R2, Diversions_R2, Returns_R2)) %>% 
  mutate(Reach = "R2")

#Final dataframe
DiversionSubreachData <- rbind(Reach1, Reach2)
write.csv(DiversionSubreachData, "Data/Processed/DiversionSubreachData.csv", row.names = F)
