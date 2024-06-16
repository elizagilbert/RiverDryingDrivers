#Read me ####
#The purpose of this script is to data wrangle Response and Predictors for 
#a model of the river as one spatial extent
#Dry = 0, Wet = 1

#Libraries ####
library(tidyverse)
library(lubridate)
library(zoo)
library(tidyr)
library(MARSS)
library(beepr)

#data response ####
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


MileDays_df %>%
  group_by(year(Date)) %>%
  summarise(max = max(MileDays)) %>%
  arrange(max)

#scaling and data wrangle function 
predictor_func <- function(data, predictor){
  result <- 
    as.matrix(data %>% 
                mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
                filter(between(month(Date), 4,10)) %>% 
                select(Date, {{predictor}}, Reach) %>% 
                group_by(Reach) %>% 
                mutate(zExtent = ({{predictor}} - mean({{predictor}}, na.rm=TRUE)) / sd({{predictor}}, na.rm=TRUE)) %>% 
                select(!{{predictor}}) %>% 
                pivot_wider(names_from = Date, values_from = zExtent) %>% 
                column_to_rownames(var = "Reach"))
  
  return(result)
}

MileDays_dat<- predictor_func(MileDays_df, MileDays)

#data covariates ####
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

dat_diversions <- read.csv("Data/Processed/MRGCD_diversion.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), Month = month(Date)) %>% 
  mutate(MnDischarge_cfs = MnDischarge_cfs*0.0283) #converts to cms

dat_returns <- read.csv("Data/Processed/MRGCD_returns.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% 
  mutate(MnDischarge_cfs = MnDischarge_cfs*0.0283) #converts to cms


dat_discharge <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  mutate(Date = as.Date(dateTime, format = "%Y-%m-%d"), Month = month(Date)) %>%
  filter(site_name != "Bernardo" & site_name != "LfccSanMarcial" & site_name != "RioPuerco" &
           site_name !="SanAntonio") %>% 
  mutate(Discharge_cfs = Discharge_cfs*0.0283) #converts to cms


#Temp Precip 1 river ####

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
  ungroup() %>% 
  select(TempCum_C, PrecipCum_mm)

# Temp_Precip %>%
#   group_by(year(Date)) %>%
#   summarise(max = max(PrecipCum_mm)) %>%
#   arrange(max)

Precip_1River <- Temp_Precip %>% 
  select(PrecipCum_mm)

Temp_1River <- Temp_Precip %>% 
  select(TempCum_C)

#Discharge 1 river ####
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


# Discharge_1River %>%
#   group_by(year(Date)) %>%
#   summarise(max = max(DischargeCum_cfs)) %>%
#   arrange(max)

#Diversion 1 river ####
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
  ungroup() %>% 
  select(ReturnsCum_cfs)

#Combine 1 state diverison ####
temp_df <- cbind(Discharge_1River, Diversions_1River, Temp_1River, Returns_1River, Precip_1River)

Cov_matrix <- as.matrix(temp_df %>% 
  pivot_longer(cols = DischargeCum_cfs:PrecipCum_mm, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor) %>% 
  mutate(NewRowName = paste0("1StateDiv_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName"))

#MARSS ####
#C matrix
C_1state <- matrix(0,1,5)  
diag1 <- function(x) {
  C_1dis <- matrix(list(0),1,1); diag(C_1dis) <- "dis"
  C_1div <- matrix(list(0),1,1); diag(C_1div) <- "div"
  C_1precip <- matrix(list(0),1,1); diag(C_1precip) <- "precip"
  C_1ret <- matrix(list(0),1,1); diag(C_1ret) <- "ret"
  C_1temp <- matrix(list(0),1,1); diag(C_1temp) <- "temp"
  C_1state <- cbind(C_1dis, C_1div,C_1precip, C_1ret, C_1temp)
  C_1state
}
C_1state <- diag1(C_1state)

#model list

mod_1state <- list(B = matrix(1), U = matrix(1), Q = "diagonal and equal",
                   c=Cov_matrix, C=C_1state, Z = matrix(1), A = matrix(1), 
                   R = "diagonal and equal", x0 = "equal", tinitx = 0)

mod_1state_null <- list(B = matrix(1), U = matrix(1), Q = "diagonal and equal",
                                   Z = matrix(1), A = matrix(1), 
                                   R = "diagonal and equal", x0 = "equal", tinitx = 0)

#model

start.time <- Sys.time()

MD_1state_div_ <- MARSS(MileDays_dat, model = mod_1state, 
                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                  conv.test.slope.tol = 0.09), fit = T) 
MD_1state_BFGS <- MARSS(y = MileDays_dat, model = mod_1state, control = list(maxit = 5000), 
                        method = "BFGS", inits = MD_1state_div_$par) 


beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))


start.time <- Sys.time()

MD_1River_Null <- MARSS(MileDays_dat, model = mod_1state_null, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
MD_1River_Null_BFGS <- MARSS(y = MileDays_dat, model = mod_1state_null, control = list(maxit = 5000), 
                        method = "BFGS", inits = MD_1River_Null$par) 


beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))

#save model
saveRDS(MD_1state_BFGS, "ModelOutput/MileDays/MD_1River_BFGS.rds")
MD_1state_BFGS <- readRDS("ModelOutput/MileDays/MD_1River_BFGS.rds")

saveRDS(MD_1River_Null_BFGS, "ModelOutput/MileDays/MD_1River_Null_BFGS.rds")
MD_1River_Null_BFGS <- readRDS("ModelOutput/MileDays/MD_1River_Null_BFGS.rds")

#residuals
autoplot.marssMLE(MD_1state_BFGS) #good
MARSSparamCIs(MD_1state_BFGS)

autoplot.marssMLE(MD_1River_Null_BFGS) #good
MARSSparamCIs(MD_1River_Null_BFGS)


#RMSE
conf_marss1 <- fitted(MD_1state_BFGS, type = "ytT", interval = "confidence")
pred_marss1 <- fitted(MD_1state_BFGS, type = "ytT", interval = "prediction")
df1 <- cbind(conf_marss1, pred_marss1[, c(".lwr", ".upr")]) %>% 
  rename(Reach = 1)

MD_1_RMSE <- sqrt(mean(df1$y - df1$.fitted)^2)
MD_1_RMSE

ggplot(df3, aes(x = t, y = y))+
  geom_line(linewidth = 2)+
  geom_point(aes(x = t, y = .fitted), color = "blue")+
  theme(legend.position = "right")+
  ggtitle("1 study area")+
  theme_classic()

#R sqaured
Sum_MD_1_resids <- sum((df1$y - df1$.fitted)^2)
TotalSUm_MD_1_resids <- sum((df1$y - mean(df1$y))^2)
Rsquared_MD_1 <- 1-(Sum_MD_1_resids/TotalSUm_MD_1_resids)


