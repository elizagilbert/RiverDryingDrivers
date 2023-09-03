#Read me ####
#The purpose of this script is to wrangle Response and Covariates for 
#to see if there is smaller scale spatial structuring within the
#Drying Reaches by 3rds

#R1 is 170 to 150.1; R2 is 150 to 130.1; R3 is 130 to 74
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

#Reach 1 Mile Days ####
#Drying in R1 is 160.8 to 150.1 = 10.7 river miles and 1/3 = 3.6

  #Top RM to estimate is 157.2 to 160.8
MileDays_R1_Tp <- dat_drying %>%      
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry,157.2, 160.8), Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup(Date) 

  #Middle RM to estimate is 153.7 to 157.1
MileDays_R1_Mid <- dat_drying %>% 
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry,153.7, 157.1), Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup(Date) 

   #Bottom RM to estimate is 150.1 to 153.7
MileDays_R1_Btm <- dat_drying %>% 
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry,150.1, 153.7), Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup(Date) 

#Reach 1 TempPrecip ####
  #Only station is Los Lunas
TempPrecip_R1_Top_Btm <- dat_TempPrecip_LosLunas %>% 
  mutate(Precip_LosLunas2_mm = ifelse(is.na(Precip_mm), 
                                      mean(Precip_mm, na.rm = T), Precip_mm),
         TempMax_LosLunas2_C = na.approx(Temp_C, na.rm = F)) %>% 
  select(Date, Precip_LosLunas2_mm, TempMax_LosLunas2_C) %>% 
  rename(Precip_mm = Precip_LosLunas2_mm, Temp_C = TempMax_LosLunas2_C) %>% 
  filter(Date >= "2010-01-01") %>% 
  group_by(year(Date)) %>% 
  mutate(PrecipCum_mm = cumsum(Precip_mm), TempCum_C = cumsum(Temp_C)) %>% 
  ungroup() %>% 
  select(PrecipCum_mm, TempCum_C)

#Reach 1 Diversion ####
Diversions_R1_Top_Btm <- dat_diversions %>% 
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
  select(DiversionCum_cfs)

#Reach 1 Returns ####
  #Top 157.2 to 160.8 and used ALJWW at 166.6 and 240WW at 165.2
Returns_R1_Tp <- dat_returns %>% 
  filter(DivName == "ALJWW" | DivName == "240WW") %>% 
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
  select(ReturnsCum_cfs)

  #Middle is 153.7 to 157.2 and used ALJWW, 240WW, and LCZWW (although not gaged till August 2016) at 156.7
Returns_R1_Mid <- dat_returns %>% 
  filter(DivName == "ALJWW" | DivName == "240WW" |DivName == "LCZWW") %>% 
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
  select(ReturnsCum_cfs)

  #Bottom is 150.1 to 153.6 and used ALJWW, 240WW, LCZWW, and PERWW at 152.5
Returns_R1_Btm <- dat_returns %>% 
  filter(DivName == "ALJWW" | DivName == "240WW" |DivName == "LCZWW"| DivName == "PERWW") %>% 
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
  mutate(ReturnsCum_cfs = cumsum(Returns_cfs)) %>% 
  ungroup() %>% 
  select(ReturnsCum_cfs)

#Reach 1 Discharge ####
Discharge_R1_Top_Btm <- dat_discharge %>% 
  mutate(dateTime = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(site_name == "BosqueFarms", dateTime <= "2021-12-31") %>% 
  complete(dateTime = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  filter(dateTime >= "2010-01-01") %>% 
  group_by(year(dateTime)) %>% 
  mutate(DischargeCum_cfs = cumsum(Discharge_cfs),
         DischargeChng_cfs = Discharge_cfs - lag(Discharge_cfs, default = Discharge_cfs[1])) %>% 
  ungroup() %>% 
  select(DischargeCum_cfs) 

#Reach 1 data frames ####
TopR1_MD <- cbind(MileDays_R1_Tp, TempPrecip_R1_Top_Btm, Discharge_R1_Top_Btm,
                  Diversions_R1_Top_Btm, Returns_R1_Tp) %>% 
  mutate(Reach = "R1")%>% 
  filter(between(month(Date), 4, 10))

MidR1_MD <- cbind(MileDays_R1_Mid, TempPrecip_R1_Top_Btm, Discharge_R1_Top_Btm,
                  Diversions_R1_Top_Btm, Returns_R1_Mid) %>% 
  mutate(Reach = "R2")%>% 
  filter(between(month(Date), 4, 10))

BtmR1_MD <- cbind(MileDays_R1_Btm, TempPrecip_R1_Top_Btm, Discharge_R1_Top_Btm,
                  Diversions_R1_Top_Btm, Returns_R1_Btm) %>% 
  mutate(Reach = "R3")%>% 
  filter(between(month(Date), 4, 10))

#Write Data
tempR1 <- rbind(TopR1_MD, MidR1_MD, BtmR1_MD)

write.csv(tempR1, "Data/Processed/SmallScale/SmScl_R1MD_RespCov.csv", row.names = F)


#Reach 2 Mile Days ####
#Drying in R2 occurred in the entire reach but only up to 150.0 in 2013
#So drying from 130.1 to 150.0 = 19.9 river miles and 1/3 = 6.6

#Top RM to estimate is 143.4 to 150.0
MileDays_R2_Tp <- dat_drying %>%      
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry,143.4, 150.0), Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup(Date) 

#Middle RM to estimate is 136.8 to 143.3
MileDays_R2_Mid <- dat_drying %>% 
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry, 136.8, 143.3), Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup(Date) 

#Bottom RM to estimate is 130.1 to 136.7
MileDays_R2_Btm <- dat_drying %>% 
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry,130.1, 136.7), Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup(Date) 

#Reach 2 TempPrecip ####
#Only station is Bernardo
TempPrecip_R2_Top_Btm <- dat_TempPrecip_AllOtherLocs %>% 
  filter(NAME == "BERNARDO, NM US") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Date <= "2021-12-31") %>% 
  select(NAME, Date, Precip_mm, Temp_C) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(Precip_mm = ifelse(is.na(Precip_mm), mean(Precip_mm, na.rm = T), Precip_mm),
         Temp_C = na.approx(Temp_C, na.rm = F)) %>% 
  filter(Date >= "2010-01-01") %>% 
  group_by(year(Date)) %>% 
  mutate(PrecipCum_mm = cumsum(Precip_mm), TempCum_C = cumsum(Temp_C),
         PrecipChng_mm = Precip_mm - lag(Precip_mm, default = Precip_mm[1]),
         TempChng_C = Temp_C - lag(Temp_C, default = Temp_C[1])) %>% 
  ungroup() %>% 
  select(PrecipCum_mm, TempCum_C)

#Reach 2 Diversion ####
Diversions_R2_Top_Btm <- dat_diversions %>% 
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
  select(DiversionCum_cfs)

#Reach 2 Returns ####
#Top 143.4 to 150.0 and used LP1DR at 149.5 and BELDR at 148 and LP2DR at 144.7
Returns_R2_Tp <- dat_returns %>% 
  filter(DivName == "LP1DR" | DivName == "BELDR" | DivName == "LP2DR") %>% 
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
  select(ReturnsCum_cfs)

#Middle is 136.8 to 143.3 and used LP1DR, BELDR, LP2DR, FD3WW at 142.7, STYWW at 140.1 and SABDR at 137.9
Returns_R2_Mid <- dat_returns %>% 
  filter(DivName == "LP1DR" | DivName == "BELDR" | DivName == "LP2DR" |
         DivName == "FD3WW" | DivName == "STYWW" | DivName == "SABDR") %>% 
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
  select(ReturnsCum_cfs)

#Bottom is 130.1 to 136.7 and used LP1DR, BELDR, LP2DR, FD3WW, STYWW, and SABDR 
Returns_R2_Btm <- dat_returns %>% 
  filter(DivName == "LP1DR" | DivName == "BELDR" | DivName == "LP2DR"|
         DivName == "FD3WW" | DivName == "STYWW" | DivName == "SABDR") %>% 
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
  mutate(ReturnsCum_cfs = cumsum(Returns_cfs)) %>% 
  ungroup() %>% 
  select(ReturnsCum_cfs)

#Reach 2 Discharge ####
Discharge_R2_Top_Btm <- dat_discharge %>% 
  mutate(dateTime = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(site_name == "StateHwy346", dateTime <= "2021-12-31") %>% 
  complete(dateTime = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  filter(dateTime >= "2010-01-01") %>% 
  group_by(year(dateTime)) %>% 
  mutate(DischargeCum_cfs = cumsum(Discharge_cfs),
         DischargeChng_cfs = Discharge_cfs - lag(Discharge_cfs, default = Discharge_cfs[1])) %>% 
  ungroup() %>% 
  select(DischargeCum_cfs) 

#Reach 2 data frames ####
TopR2_MD <- cbind(MileDays_R2_Tp, TempPrecip_R2_Top_Btm, Discharge_R2_Top_Btm,
                  Diversions_R2_Top_Btm, Returns_R2_Tp) %>% 
  mutate(Reach = "R1")%>% 
  filter(between(month(Date), 4, 10))

MidR2_MD <- cbind(MileDays_R2_Mid, TempPrecip_R2_Top_Btm, Discharge_R2_Top_Btm,
                  Diversions_R2_Top_Btm, Returns_R2_Mid) %>% 
  mutate(Reach = "R2")%>% 
  filter(between(month(Date), 4, 10))

BtmR2_MD <- cbind(MileDays_R2_Btm, TempPrecip_R2_Top_Btm, Discharge_R2_Top_Btm,
                  Diversions_R2_Top_Btm, Returns_R2_Btm) %>% 
  mutate(Reach = "R3")%>% 
  filter(between(month(Date), 4, 10))

#Write Data
tempR2 <- rbind(TopR2_MD, MidR2_MD, BtmR2_MD)

write.csv(tempR2, "Data/Processed/SmallScale/SmScl_R2MD_RespCov.csv", row.names = F)

test <- read.csv("Data/Processed/SmallScale/SmScl_R2MD_RespCov.csv")
#Reach 3 Mile Days ####
#Drying in R3 is 74 to 105.5 = 31.5 river miles and 1/3 = 10.5

#Top RM to estimate is 95 to 105.5 
MileDays_R3_Tp <- dat_drying %>%      
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry,95, 105.5), Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup(Date) 

#Middle RM to estimate is 84.5 to 94.9
MileDays_R3_Mid <- dat_drying %>% 
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry,84.5, 94.9), Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup(Date) 

#Bottom RM to estimate is 74 to 84.4
MileDays_R3_Btm <- dat_drying %>% 
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(between(RMTenthDry,74, 84.4), Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  mutate(MileDays = max(MD)) %>% 
  distinct(Date, MileDays) %>% 
  ungroup(Date) 

#Reach 3 TempPrecip ####

TempPrecip_R3_Top_Mid <- dat_TempPrecip_AllOtherLocs %>% 
  filter(NAME == "SOCORRO 20 N, NM US") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Date >= "2010-01-01") %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(Precip_mm = ifelse(is.na(Precip_mm), mean(Precip_mm, na.rm = T), Precip_mm),
         Temp_C = na.approx(Temp_C, na.rm = F)) %>% 
  select(Date, Precip_mm, Temp_C) %>% 
  group_by(year(Date)) %>% 
  mutate(PrecipCum_mm = cumsum(Precip_mm), TempCum_C = cumsum(Temp_C)) %>% 
  ungroup() %>% 
  select(PrecipCum_mm, TempCum_C)

TempPrecip_R3_Btm <- dat_TempPrecip_AllOtherLocs %>% 
  filter(NAME == "SOCORRO 20 N, NM US" | NAME == "BOSQUE DEL APACHE, NM US") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Date >= "2010-01-01") %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  group_by(Date) %>% 
  mutate(Precip_mm0 = sum(Precip_mm, na.rm = T),
         Temp_C0 = mean(Temp_C, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Precip_mm1 = ifelse(is.na(Precip_mm0),mean(Precip_mm0, na.rm = T), Precip_mm0),
         Temp_C1 = na.approx(Temp_C0, na.rm = F)) %>% 
  select(Date, Precip_mm1, Temp_C1) %>% 
  rename(Precip_mm = Precip_mm1, Temp_C = Temp_C1) %>% 
  filter(Date >= "2010-01-01") %>% 
  distinct(Date, .keep_all = T) %>% 
  group_by(year(Date)) %>% 
  mutate(PrecipCum_mm = cumsum(Precip_mm), TempCum_C = cumsum(Temp_C)) %>% 
  ungroup() %>% 
  select(PrecipCum_mm, TempCum_C)

#Reach 3 Diversion ####
Diversions_R3_Top_Btm <- dat_diversions %>% 
  filter(DivName == "SNA02") %>% 
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
  select(DiversionCum_cfs)

#Reach 3 Returns ####
#Tp is 95 to 105.5 and there are no returns upstream or within this section closest is 
#Neil Cupp at 90.2 so I inserted a 0 for all days to have a dummy variable

Returns_R3_Tp <- dat_returns %>% 
  filter(DivName == "ALJWW") %>% 
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
  select(ReturnsCum_cfs) %>% 
  mutate(ReturnsCum_cfs = 0)

#Middle is 84.5 to 94.9 and return is Neil Cupp at 90.2
Returns_R3_Mid <- dat_returns %>% 
  filter(DivName == "NCPPS") %>% 
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
  select(ReturnsCum_cfs)

#Bottom is 74 to 84.4 and returans are NBYPS at 84 and SBYPS at 73.8
Returns_R3_Btm <- dat_returns %>% 
  filter(DivName == "NBYPS" | DivName == "SBYPS") %>% 
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
  mutate(ReturnsCum_cfs = cumsum(Returns_cfs)) %>% 
  ungroup() %>% 
  select(ReturnsCum_cfs)

#Reach 3 Discharge ####
#used Ecsondida at river mile 104
Discharge_R3_Top_Btm <- dat_discharge %>% 
  mutate(dateTime = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(site_name == "Escondida", dateTime <= "2021-12-31") %>% 
  complete(dateTime = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  filter(dateTime >= "2010-01-01") %>% 
  group_by(year(dateTime)) %>% 
  mutate(DischargeCum_cfs = cumsum(Discharge_cfs),
         DischargeChng_cfs = Discharge_cfs - lag(Discharge_cfs, default = Discharge_cfs[1])) %>% 
  ungroup() %>% 
  select(DischargeCum_cfs) 

#Reach 3 data frames ####
TopR3_MD <- cbind(MileDays_R3_Tp, TempPrecip_R3_Top_Mid, Discharge_R3_Top_Btm,
                  Diversions_R3_Top_Btm, Returns_R3_Tp) %>% 
  mutate(Reach = "R1")%>% 
  filter(between(month(Date), 4, 10))

MidR3_MD <- cbind(MileDays_R3_Mid, TempPrecip_R3_Top_Mid, Discharge_R3_Top_Btm,
                  Diversions_R3_Top_Btm, Returns_R3_Mid) %>% 
  mutate(Reach = "R2")%>% 
  filter(between(month(Date), 4, 10))

BtmR3_MD <- cbind(MileDays_R3_Btm, TempPrecip_R3_Btm, Discharge_R3_Top_Btm,
                  Diversions_R3_Top_Btm, Returns_R3_Btm) %>% 
  mutate(Reach = "R3")%>% 
  filter(between(month(Date), 4, 10))

#Write Data
tempR3 <- rbind(TopR3_MD, MidR3_MD, BtmR3_MD)

write.csv(tempR3, "Data/Processed/SmallScale/SmScl_R3MD_RespCov.csv", row.names = F)

