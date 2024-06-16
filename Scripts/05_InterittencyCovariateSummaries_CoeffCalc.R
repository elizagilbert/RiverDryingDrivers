#Read me ####
#The purpose of this script is calculate the general
#summaries of cumulative flow intermittency and 
#covariates to report in the manuscript

#Libraries #####
library(tidyverse)
library(lubridate)
library(zoo)

#cummulative mile days 1 river ####
dat_drying <- read.csv("Data/Processed/2010_2021_WetDryTenths.csv")

MileDays_df <- dat_drying %>% 
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(RMTenthDry >74, Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD), Reach = 1) %>% 
  mutate(MileDays = MileDays*1.6) %>%  #turns this into KilometerDays
  distinct(Date, MileDays, Reach) %>% 
  ungroup(Date) 

MD_annual_max <- MileDays_df %>% 
  group_by(year(Date)) %>% 
  summarise(max(MileDays)) %>% 
  rename(MaxMD = 2)

MD_mn_sd <- MD_annual_max %>% 
  summarise(mean(MaxMD), sd(MaxMD))

#temp/precip 1 river ####
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
  filter(Date >= "2010-01-01") %>% 
  replace_na(list(NAME = "LOS LUNAS 3 SSW, NM US"))

dat_TempPrecip_AllOtherLocs <- read.csv("Data/Raw/TempPrecip_AllOtherLocations.csv") %>%  #This is R2 and I downloaded this as metric data so no conversion needed
  select("STATION", "NAME", "DATE", "PRCP", "TMAX","TMIN")%>% 
  select(DATE, NAME, PRCP, TMAX) %>% 
  rename(Date = DATE, Precip_mm = PRCP, Temp_C = TMAX) %>% 
  filter(Date >= "2010-01-01") 

dat_TempPrecip_All <- rbind(dat_TempPrecip_LosLunas, dat_TempPrecip_AllOtherLocs)

Temp_Precip <- dat_TempPrecip_All %>% 
  arrange(Date) %>% 
  group_by(NAME) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(Temp_Approx = ifelse(is.na(Temp_C), mean(Temp_C, na.rm = T), Temp_C),
         Precip_Approx = ifelse(is.na(Precip_mm), mean(Precip_mm, na.rm = T), Precip_mm)) %>% 
  mutate(TempAvg = mean(Temp_Approx),
         PrecipAvg = sum(Precip_Approx)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  distinct(Date, TempAvg, PrecipAvg) %>% 
  filter(between (month(Date), 4,10)) %>% 
  group_by(year(Date)) %>% 
  mutate(TempCum_C = cumsum(TempAvg),
         PrecipCum_mm = cumsum(PrecipAvg)) %>% 
  ungroup() 

Temp_sum <- Temp_Precip %>%
  rename(Year = 4) %>% 
  select(Year, TempCum_C) %>% 
  group_by(Year) %>%
  summarise(max(TempCum_C)) %>% 
  rename(MaxT = 2)

Temp_mn_sd <- Temp_sum %>% 
  summarise(mean(MaxT), sd(MaxT))

Precip_sum <- Temp_Precip %>%
  rename(Year = 4) %>% 
  select(Year, PrecipCum_mm) %>% 
  group_by(Year) %>%
  summarise(max(PrecipCum_mm)) %>% 
  rename(MaxP = 2)

Precip_mn_sd <- Precip_sum %>% 
  summarise(mean(MaxP), sd(MaxP))

#diversions 1 river ####
dat_diversions <- read.csv("Data/Processed/MRGCD_diversion.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  mutate(MnDischarge_cfs = MnDischarge_cfs*0.0283) #converts to cms

#Diversion 1 river
Diversions_1River <- dat_diversions %>% 
  filter(between (Date, as.Date("2010-01-01"), as.Date("2021-12-31"))) %>% 
  filter(between (month(Date), 4,10)) %>% 
  group_by(DivName) %>% 
  mutate(Diversion_Approx = na.approx(MnDischarge_cfs, na.rm = F)) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(DiversionTot = sum(MnDischarge_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  distinct(Date, DiversionTot) %>% 
  group_by(year(Date)) %>% 
  mutate(DiversionCum = cumsum(DiversionTot)) %>% 
  ungroup() %>% 
  select(Date, DiversionCum)

Div_sum <- Diversions_1River %>%
  group_by(year(Date)) %>%
  summarise(max(DiversionCum)) %>% 
  rename(MaxDiv = 2)

Div_mn_sd <- Div_sum %>% 
  summarise(mean(MaxDiv), sd(MaxDiv))

#returns 1 river ####
dat_returns <- read.csv("Data/Processed/MRGCD_returns.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% 
  mutate(MnDischarge_cfs = MnDischarge_cfs*0.0283) #converts to cms

#Returns 1 river  ####
Returns_1River <- dat_returns %>% 
  filter(Date >= "2010-01-01") %>% 
  filter(between (month(Date), 4,10)) %>% 
  group_by(Date) %>% 
  mutate(ReturnTot = sum(MnDischarge_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  distinct(Date, ReturnTot) %>% 
  group_by(year(Date)) %>% 
  mutate(ReturnsCum_cfs = cumsum(ReturnTot)) %>% 
  ungroup()

Ret_sum <- Returns_1River %>%
  rename(Year = 3) %>% 
  select(Year, ReturnsCum_cfs) %>% 
  group_by(Year) %>%
  summarise(max(ReturnsCum_cfs)) %>% 
  rename(MaxRet = 2)

Ret_mn_sd <- Ret_sum %>% 
  summarise(mean(MaxRet), sd(MaxRet))

Ret_daily_range <- dat_returns %>% 
  filter(Date >= "2010-01-01") %>% 
  filter(between(month(Date), 4, 10)) %>% 
  group_by(DivName, year(Date)) %>% 
  summarise(minD = min(MnDischarge_cfs, na.rm = TRUE), maxD = max(MnDischarge_cfs, na.rm = TRUE),
            meanD = mean(MnDischarge_cfs, na.rm = TRUE))


#discharge 1 river #####
dat_discharge <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  mutate(Date = as.Date(dateTime, format = "%Y-%m-%d"), Month = month(Date)) %>%
  filter(site_name != "Bernardo" & site_name != "LfccSanMarcial" & site_name != "RioPuerco" &
           site_name !="SanAntonio") %>% 
  mutate(Discharge_cfs = Discharge_cfs*0.0283) #converts to cms

#Discharge 1 river 
Discharge_1River <- dat_discharge %>% 
  filter(between (Date, as.Date("2010-01-01"), as.Date("2021-12-31"))) %>% 
  group_by(site_name) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(Discharge_Approx = na.approx(Discharge_cfs, na.rm = F)) %>% 
  ungroup() %>% 
  mutate(DischargeAvg = mean(Discharge_Approx)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  distinct(Date, DischargeAvg) %>% 
  group_by(year(Date)) %>% 
  mutate(DischargeCum_cfs = cumsum(DischargeAvg)) %>% 
  filter(between (month(Date), 4,10)) %>% 
  ungroup() %>% 
  select(Date, DischargeCum_cfs) 


Dis_sum <- Discharge_1River %>%
  group_by(year(Date)) %>%
  summarise(max = max(DischargeCum_cfs)) %>%
  arrange(max)

Div_mn_sd <- Dis_sum %>% 
  summarise(mean(max), sd(max))

#percent returned of diverted 1 river ####
Per_ret <- as.data.frame(Ret_sum$MaxRet/Div_sum$MaxDiv)

#coeff effect 2 subreaches ####
#data
dat_DivR <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T)

#cacl standard deviation because coefficents represent 1 change in sd
twosubreach_sd <- dat_DivR %>% 
  select(MileDays, Date, Reach) %>% 
  group_by(Reach) %>% 
  summarise(sd(MileDays)) %>% 
  rename(AnnualSdMileDays = 2, Year = 1) %>% 
  ungroup() %>% 
  summarise(mean(AnnualSdMileDays))

Mn_sd_div_MileDays <- mean(twosubreach_sd$`mean(AnnualSdMileDays)`)
DivCoeff <-  0.005
DischargeCoeff <- -0.011

OrigScale_DivCoeff <- Mn_sd_div_MileDays*DivCoeff
OrigScale_DisCoef <- Mn_sd_div_MileDays*DischargeCoeff

#increase discharge by 1 m^3/s decreases cum days dry
#by 6.9

#descrease diversion by 1 m^3/s decreases cumd ays dry
#by 3.12

6.9+3.12 #is 10.2 km and if multiply this by 10
#this is 102 km

#coeff effect 10 subreaches
#Load Data ####
load("Data/Processed/Random100/matrices_list_MD100.RData")
