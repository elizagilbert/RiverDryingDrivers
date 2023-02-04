#Read me ####
#The purpose of this script is to create a time series and covariates for Alex
#to test in MARSS

#Libraries ####
library(tidyverse)
library(lubridate)
library(zoo)

#data ####
dat_drying <- read.csv("Data/Processed/2002_2021_WetDryTenths.csv") 
dat_TempPrecip_LosLunas <- read.csv("Data/Raw/TempPrecip_LosLunas_GHCNDUSC00295150.csv") 
dat_discharge <- read.csv("Data/Processed/USGS_discharge.csv") 
dat_diversions <- read.csv("Data/Processed/MRGCD_diversion.csv")
dat_returns <- read.csv("Data/Processed/MRGCD_returns.csv")

#drying ####
#filter to the upper most dry reach of river
#using 2010 to 2021 because 

#daily change in river miles dry (extent - # river miles)
DailyChngDry_RiverReach1 <- dat_drying %>%
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(DryRM == 0 & RMTenthDry >150) %>% 
  group_by(Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  rename(RiverReach1_Date = Date) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0),
         ChngExtentDry = ExtentDry - lag(ExtentDry, default = ExtentDry[1])) %>% 
  filter(RiverReach1_Date >= "2010-01-01")

ggplot(DailyChngDry_RiverReach1, aes(x = RiverReach1_Date, y = ChngExtentDry))+
  geom_point()+
  xlab("") + ylab("Daily change amonout of drying - miles") + ggtitle("")+
  theme_classic()+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")

#daily days dry (duration)
DaysDry_RiverReach1 <- dat_drying %>% 
  select(!X) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(RMTenthDry >150, Date >= "2010-01-01") %>% 
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
  select(-Date)

#for plotting
dat_drying %>% 
  select(!X) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(RMTenthDry >150) %>% 
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
  filter(Date >= "2010-01-01") %>% 
  ggplot(aes(x=Date, y = SumDaysDry))+
  geom_point()+
  xlab("") + ylab("Daily duration of drying - days ") + ggtitle("")+
  theme_classic()+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")

#daily most upstream dry river mile (location)
DailyLocationDry_RiverReach1 <- dat_drying %>%
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(RMTenthDry >150) %>% 
  arrange(Date, desc(RMTenthDry))  %>% 
  group_by(Date, DryRM) %>% 
  summarise_all(list(~max(RMTenthDry))) %>% 
  as.data.frame() %>% 
  filter(DryRM == 0) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-311"), by = "day")) %>% 
  filter(Date >= "2010-01-01") %>% 
  mutate(UpperLocationDry = RMTenthDry,
         DiffUpperLocationDry = UpperLocationDry - lag(UpperLocationDry, default = UpperLocationDry[1])) %>% 
  arrange(Date) %>% 
  select(UpperLocationDry, DiffUpperLocationDry)

#for plotting
dat_drying %>%
  select(!X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(RMTenthDry >150) %>% 
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
  mutate(UpperLocDry2 = case_when(is.na(UpperLocationDry) ~ 150,
                                  TRUE ~ UpperLocationDry),
         ForPlotting = case_when(UpperLocDry2 == 150 ~ 0,
                                 TRUE ~ 1)) %>% 
  ggplot(aes(x=Date, y = UpperLocDry2, color = as.factor(ForPlotting)))+
  geom_point(size = 0.75)+
  xlab("") + ylab("Upper extent of drying - river mile") + 
  theme_classic()+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
  scale_color_manual(values = c("red", "black"), labels = c("no drying", "drying"), 
                     name = "")+
  ylim(c(150, 180))

#climate covariates ####
TempPrecip_LosLunas_temp <- dat_TempPrecip_LosLunas %>% 
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>% 
  filter(DATE <= "2021-12-31") %>% 
  select(STATION, NAME, DATE, PRCP, TMAX, TMIN) %>% 
  rename(TempPrecipStation = STATION, StationName = NAME, Date = DATE,
         Precip_LosLunas_inch = PRCP, TempMax_LosLunas_F = TMAX, TempMin_LosLunas_F = TMIN) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(Precip_LosLunas_mm = round((Precip_LosLunas_inch*25.4),2), 
         TempMax_LosLunas_C1 = TempMax_LosLunas_F-32,
         TempMax_LosLunas_C2 = TempMax_LosLunas_C1*5,
         TempMax_LosLunas_C = TempMax_LosLunas_C2/9,
         TempMax_LosLunas_C = round(TempMax_LosLunas_C,2)) %>% 
  mutate(Precip_LosLunas2_mm = ifelse(is.na(Precip_LosLunas_mm), mean(Precip_LosLunas_mm, na.rm = T), Precip_LosLunas_mm),
         TempMax_LosLunas2_C = na.approx(TempMax_LosLunas_C, na.rm = F)) %>% 
  select(Date, Precip_LosLunas2_mm, TempMax_LosLunas2_C) %>% 
  filter(Date >= "2010-01-01")

#plotting
#precip - NEED to run the dataframe above without the last four lines
TempPrecip_LosLunas_temp %>% 
  mutate(Precip_LosLunas2 = case_when(is.na(Precip_LosLunas_mm) ~ -1,
                                     TRUE ~ Precip_LosLunas_mm),
         PrecipPlotting = case_when(Precip_LosLunas2 < 0 ~ 0,
                                    TRUE ~ 1)) %>% 
  ggplot(aes(x = Date, y = Precip_LosLunas2, color = as.factor(PrecipPlotting)))+
  geom_point()+
  theme_classic() + xlab("")+ ylab("Precipitation - mm")+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
  scale_color_manual(values = c("grey", "black"), labels = c("missing data", "data"),
                     name = "")

 #mean of julian day for missing data
TempPrecip_LosLunas_temp %>%
  select(Date, Precip_LosLunas_mm) %>%
  mutate(JulDay = yday(Date)) %>%
  group_by(JulDay) %>%
  mutate(Precip_LosLunas2 = ifelse(is.na(Precip_LosLunas_mm), mean(Precip_LosLunas_mm, na.rm = T), Precip_LosLunas_mm),
         PrecipPlotting = case_when(is.na(Precip_LosLunas_mm) ~ 0,
                                    TRUE ~ 1)) %>% 
  ggplot(aes(x = Date, y = Precip_LosLunas2, color = as.factor(PrecipPlotting)))+
  geom_point(size = 0.75)+
  theme_classic() + xlab("")+ ylab("Precipitation - mm")+ 
  ggtitle("")+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
  scale_color_manual(values = c("red", "black"), labels = c("interpolated", "measured"),
                     name = "")+
  theme(legend.position = "none", plot.title = element_text(size = 10))


#temperature
TempPrecip_LosLunas_temp %>%
  select(Date, TempMax_LosLunas_C) %>%
  arrange(Date) %>% 
  mutate(TempMax_LosLunas2 = na.approx(TempMax_LosLunas_C, na.rm = F),
         TempMax_plotting = case_when(is.na(TempMax_LosLunas_C) ~ 0,
                                      TRUE ~ 1))%>% 
  ggplot(aes(x = Date, y = TempMax_LosLunas2, color = as.factor(TempMax_plotting)))+
  geom_point(size = 0.75)+
  theme_classic() + xlab("")+ ylab("Temperature - C")+ 
  ggtitle("")+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
  scale_color_manual(values = c("red", "black"), labels = c("interpolated", "measured"),
                     name = "")+
  theme(legend.position = "top", plot.title = element_text(size = 10))

#human covariates ####
#This gage starts in 2007
Discharge_BosqueFarms_2007 <- dat_discharge %>% 
  mutate(dateTime = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(site_name == "BosqueFarms", dateTime <= "2021-12-31") %>% 
  complete(dateTime = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  filter(dateTime >= "2010-01-01") %>% 
  select(Discharge_cfs) %>% 
  rename(RiverDischarge_BosqueFarms_cfs = Discharge_cfs)


#plotting
dat_discharge %>% 
  mutate(dateTime = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(site_name == "BosqueFarms", dateTime <= "2021-12-31") %>% 
  complete(dateTime = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  filter(dateTime >= "2010-01-01") %>% 
  ggplot(aes(x = dateTime, y = Discharge_cfs))+
  geom_point()+
  theme_classic() + xlab("")+ ylab("River discharge - cfs")+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")

#Diversions river reach 1
Diversions_AtIsleta <- dat_diversions %>% 
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
  select(Diversion_Isleta_Totalcfs)

#plotting
DiversionNames <- as_labeller(c("BELCN" = "Belen Highline", "CACCN" = "Old Cacique",
                                "CHACN" = "Upper Chical", "CHICN" = "Chical Lateral",
                                "PERCN" = "Peralta Main"))

dat_diversions %>% 
  filter(DivName != "SNA02") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  group_by(Date) %>% 
  mutate(TempDiv = round(sum(MnDischarge_cfs, na.rm = T),2)) %>% 
  mutate(Diversion_Isleta_Totalcfs = case_when(is.na(TempDiv) & Month == 1 | Month == 2 | Month == 12 ~ 0,
                                               TRUE ~ TempDiv)) %>% 
  filter(Date > "2010-01-01") %>% 
  mutate(DiversionPlotting = case_when(is.na(MnDischarge_cfs) & between(Month, 3, 11) ~ 1, 
                                       TRUE ~ 0)) %>% 
  ggplot(aes(x = Date, y = Diversion_Isleta_Totalcfs, color = as.factor(DiversionPlotting)))+
  geom_point(size = 0.75)+
  facet_grid(vars(DivName), labeller = DiversionNames)+
  theme_classic() + xlab("")+ ylab("Irrigation diversion - cfs")+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
  scale_color_manual(values = c("black", "red"), 
                     labels = c("Sum of gages with\nDec-Feb NAs filled as 0", "NAs for specific gage Mar-Nov"),
                     name = "")+
  theme(legend.position = c(0.8, 0.98),
        axis.text.x=element_text(angle=60, hjust = 1))



#Returns river reach 1 (ALJWW, 240WW, LCZWW, PERWW)...maybe not LCZWW, it doesn't start till August 2016
Returns_RiverReach1 <- dat_returns %>% 
  filter(DivName == "ALJWW" | DivName == "240WW" |DivName == "PERWW") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  filter(Date >= "2010-01-01") %>% 
  group_by(Date) %>% 
  mutate(TempReturn = sum(MnDischarge_cfs, na.rm = T)) %>% 
  mutate(Returns_Totalcfs = case_when(Month == 1 | Month == 2 | Month == 11 | Month == 12 ~ 0,
                                              TRUE ~ TempReturn)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  distinct(Date, Returns_Totalcfs) %>% 
  select(Returns_Totalcfs)

#plotting returns
ReturnNames <- as_labeller(c("ALJWW" = "Alejandro", "240WW" = "240",
                             "PERWW" = "Peralta"))
dat_returns %>% 
  filter(DivName == "ALJWW" | DivName == "240WW" |DivName == "PERWW") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  group_by(Date) %>% 
  mutate(TempReturn = sum(MnDischarge_cfs, na.rm = T)) %>% 
  mutate(Returns_Final = case_when(is.na(TempReturn) & Month == 1 | Month == 2 | Month == 12 ~ 0,
                                    TRUE ~ TempReturn)) %>% 
  filter(Date > "2010-01-01") %>% 
  mutate(ReturnPlotting = case_when(is.na(MnDischarge_cfs) & between(Month, 3, 11) ~ 1, 
                                       TRUE ~ 0)) %>% 
  ggplot(aes(x = Date, y = Returns_Final, color = as.factor(ReturnPlotting)))+
  geom_point(size = 0.75)+
  facet_grid(vars(DivName), labeller = ReturnNames)+
  theme_classic() + xlab("")+ ylab("River returns - cfs")+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
  scale_color_manual(values = c("black", "red"), 
                     labels = c("Sum of gages with\nDec-Feb NAs filled as 0", "NAs for specific gage Mar-Nov"),
                     name = "")+
  theme(legend.position = "none")


#combine all data frames ####
TS_DailyDryReach1_Cov <- as.data.frame(cbind(DailyChngDry_RiverReach1, DaysDry_RiverReach1, DailyLocationDry_RiverReach1,
                                             TempPrecip_LosLunas_temp, Discharge_BosqueFarms_2007, Diversions_AtIsleta,
                                            Returns_RiverReach1)) %>% 
  select(!ExtentDry & !DiffSumDaysDry & !DiffUpperLocationDry & !Date)

write.csv(TS_DailyDryReach1_Cov, "Data/Processed/Reach1_ResponseAndCovariates.csv", row.names = F)

names(TS_DailyDryReach1_Cov)

#plot data to make sure it looks good
ggplot(TS_DailyDryReach1_Cov, aes(x = Date, y = TempMin))+
  geom_line()

#random ####
ggplot(DailyLocationDry_RiverReach1, aes(x = Date, y = RMTenthDry, fill = as.factor(DryRM))) +
  geom_tile()+
  scale_fill_manual(name = "", values = c("#000000", "gray94"), labels = c("dry", "wet"))+
  xlab("") + ylab("River mile")+
  scale_x_date(limits = as.Date(c("2012-08-10","2012-08-30")))
theme_classic()

#count NAs
sum(is.na(TS_DailyDryReach1_Cov$Precip_inches)) #158
sum(is.na(TS_DailyDryReach1_Cov$TempMax))       #144
sum(is.na(TS_DailyDryReach1_Cov$TempMin))       #140

dat_returns %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         DischForPlotting = case_when(is.na(MnDischarge_cfs) ~ -1,
         TRUE ~ MnDischarge_cfs),
         PlotColor = case_when(DischForPlotting < 0 ~ 0,
                               TRUE ~1)) %>% 
  filter(DivName == "LCZWW",
         Date >= "2016-08-24") %>% 
  ggplot(aes(x=Date, y = DischForPlotting, color = as.factor(PlotColor)))+
  geom_point()+
  xlab("") + ylab("Discharge at Los Chavez return - Reach 1")+
  theme_classic()+
  scale_color_manual(values = c("red", "black"), labels = c("NA", "Gage measurement"),
                 name = "")+
  theme(legend.position = "top")+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")

temp <- dat_returns %>% 
  filter(DivName == "LCZWW",Date >= "2016-08-24") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         new_bin = cut(MnDischarge_cfs, breaks = c(0,10,20,30,40,50))) %>% 
  rename(BinnedDat = 4) %>% 
  group_by(BinnedDat) %>% 
  count()

                   