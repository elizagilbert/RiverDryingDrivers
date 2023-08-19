#read Me ####
#The purpose of this script is to wrangle the covariates for each of the 
#different spatial MARSS hypothesis for both drying and diversion reaches

#library ####
library(MARSS)
library(tidyverse)
library(lubridate)
library(zoo)


#data ####
dat_DryR0 <- read.csv("Data/Processed/DryingSubreachData.csv", header = T) 
dat_DivR0 <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T)

#use when reducing months 4-10
dat_DryR <- dat_DryR0 %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d" )) %>% 
  filter(between(month(Date), 4, 10)) 

dat_DivR <- dat_DivR0 %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d" )) %>% 
  filter(between(month(Date), 4, 10))

#3 states drying ####

#zscore for 3 states#
Pred_Dry_3states <-dat_DryR %>% 
  select(-c(contains(c("Cum", "Chng_")))) %>% 
  select(Date, Precip_mm:Reach) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = Precip_mm:Returns_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

PredCum_Dry_3states <- dat_DryR %>% 
  select(contains(c("Date","Reach", "Cum"))) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = PrecipCum_mm:ReturnsCum_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

PredChng_Dry_3states <- dat_DryR %>% 
  select(contains(c("Date","Reach", "Chng_"))) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = PrecipChng_mm:ReturnsChng_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

#reduced to months 4 - 10
# write.csv(Pred_Dry_3states, "Data/Processed/MARSS_Covariates/Reduced/Pred_Dry_3statesReduced.csv", row.names = T)
# write.csv(PredCum_Dry_3states, "Data/Processed/MARSS_Covariates/Reduced/PredCum_Dry_3statesReduced.csv", row.names = T)
# write.csv(PredChng_Dry_3states, "Data/Processed/MARSS_Covariates/Reduced/PredChng_Dry_3statesReduced.csv", row.names = T)

#check z-scoring
apply(PredChng_Dry_3states, 1, var)

#2 states drying ####

#zscore for 2 states#

#combine R1 and R2 and leave R3
#total precip; average temp; diversion = diversion because only one diversion; total returns; discharge at top of R1
  
  #for extent
temp1 <- dat_DryR %>% 
  select(-c(contains(c("Cum", "Chng_")))) %>% 
  select(Date, Precip_mm:Reach) %>%
  filter(Reach == "R3")

temp2 <- dat_DryR %>% 
  select(-c(contains(c("Cum", "Chng_")))) %>% 
  select(Date, Precip_mm:Reach) %>% 
  filter(Reach == "R1" | Reach == "R2") %>% 
  group_by(Date) %>% 
  mutate(Precip_mm_R1_2 = sum(Precip_mm, na.rm = T),
         Temp_C_R1_2 = mean(Temp_C, na.rm = T),
         Discharge_cfs_Rtemp = mean(Discharge_cfs, na.rm = T),
         Diversion_cfs_R1_2 = Diversion_cfs,
         Returns_cfs_R1_2 = sum(Returns_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(Discharge_cfs_R1_2 = Discharge_cfs) %>% 
  select(!Discharge_cfs_Rtemp) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R1_2")) %>% 
  select(Date, Reach, Precip_mm_R1_2:Discharge_cfs_R1_2) %>% 
  rename(Precip_mm = Precip_mm_R1_2, Temp_C = Temp_C_R1_2,
         Discharge_cfs = Discharge_cfs_R1_2, Diversion_cfs = Diversion_cfs_R1_2,
         Returns_cfs = Returns_cfs_R1_2) %>% 
  rbind(temp1)

Pred_Dry_2states <- temp2 %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = Precip_mm:Discharge_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>%
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

    #for Mile Days - cumulative values
tempcum1 <- dat_DryR %>% 
  select(contains(c("Date","Reach", "Cum"))) %>% 
  filter(Reach == "R3") %>% 
  rename(PrecipCum_mm_R1_2 = PrecipCum_mm, TempCum_C_R1_2 = TempCum_C,
         DischargeCum_cfs_R1_2 = DischargeCum_cfs, DiversionCum_cfs_R1_2 = DiversionCum_cfs,
         ReturnsCum_cfs_R1_2 = ReturnsCum_cfs)

tempcum2 <- dat_DryR %>% 
  select(contains(c("Date","Reach", "Cum"))) %>%   
  filter(Reach == "R1" | Reach == "R2") %>% 
  group_by(Date) %>% 
  mutate(PrecipCum_mm_R1_2 = sum(PrecipCum_mm, na.rm = T),
         TempCum_C_R1_2 = mean(TempCum_C, na.rm = T),
         DiversionCum_cfs_R1_2 = DiversionCum_cfs,
         ReturnsCum_cfs_R1_2 = sum(ReturnsCum_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(DischargeCum_cfs_R1_2 = DischargeCum_cfs) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R1_2")) %>% 
  select(Date, PrecipCum_mm_R1_2:DischargeCum_cfs_R1_2, Reach) %>% 
  rbind(tempcum1)

PredCum_Dry_2states <- tempcum2 %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = PrecipCum_mm_R1_2:DischargeCum_cfs_R1_2, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

    #for extent change
tempchng1 <- dat_DryR %>% 
  select(contains(c("Date","Reach", "Chng_"))) %>% 
  filter(Reach == "R3") %>% 
  rename(PrecipChng_mm_R1_2 = PrecipChng_mm, TempChng_C_R1_2 = TempChng_C,
         DischargeChng_cfs_R1_2 = DischargeChng_cfs, DiversionChng_cfs_R1_2 = DiversionChng_cfs,
         ReturnsChng_cfs_R1_2 = ReturnsChng_cfs)

tempchng2 <- dat_DryR %>% 
  select(contains(c("Date","Reach", "Chng_"))) %>%   
  filter(Reach == "R1" | Reach == "R2") %>% 
  group_by(Date) %>% 
  mutate(PrecipChng_mm_R1_2 = sum(PrecipChng_mm, na.rm = T),
         TempChng_C_R1_2 = mean(TempChng_C, na.rm = T),
         DischargeChng_cfs_R1_2 = mean(DischargeChng_cfs, na.rm = T),
         DiversionChng_cfs_R1_2 = DiversionChng_cfs,
         ReturnsChng_cfs_R1_2 = sum(ReturnsChng_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R1_2")) %>% 
  select(Date, Reach, PrecipChng_mm_R1_2:ReturnsChng_cfs_R1_2) %>%
  rbind(tempchng1)

PredChng_Dry_2states <- tempchng2 %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = PrecipChng_mm_R1_2:ReturnsChng_cfs_R1_2, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

#reduced to months 4 - 10
# write.csv(Pred_Dry_2states, "Data/Processed/MARSS_Covariates/Reduced/Pred_Dry_2statesReduced.csv", row.names = T)
# write.csv(PredCum_Dry_2states, "Data/Processed/MARSS_Covariates/Reduced/PredCum_Dry_2statesReduced.csv", row.names = T)
# write.csv(PredChng_Dry_2states, "Data/Processed/MARSS_Covariates/Reduced/PredChng_Dry_2statesReduced.csv", row.names = T)

#check z-scoring
apply(PredChng_Dry_2states, 1, var)

#1 state drying ####

#zscore for 1 state#
#combine R1-R3
#total precip; average temp; total diversion; total returns; R1 discharge

  #extent
t1 <- dat_DryR %>% 
  select(-c(contains(c("Cum", "Chng_")))) %>% 
  select(Date, Precip_mm:Reach) %>% 
  group_by(Date) %>% 
  mutate(Diversion_cfs0 = case_when(Reach == "R2" ~ 0,
                                    TRUE ~ Diversion_cfs)) %>% 
  mutate(Precip_mm_R123 = sum(Precip_mm, na.rm = T),
         Temp_C_R123 = mean(Temp_C, na.rm = T),
         Diversion_cfs_R123 = sum(Diversion_cfs0, na.rm = T),
         Returns_cfs_R123 = sum(Returns_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(Discharge_cfs_R123 = Discharge_cfs) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R123")) %>% 
  select(Date, Reach, Precip_mm_R123:Discharge_cfs_R123) %>% 
  rename(Precip_mm = Precip_mm_R123, Temp_C = Temp_C_R123,
         Discharge_cfs = Discharge_cfs_R123, Diversion_cfs = Diversion_cfs_R123,
         Returns_cfs = Returns_cfs_R123)

Pred_Dry_1state <- t1 %>% 
  pivot_longer(cols = Precip_mm:Discharge_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>%
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

  #Mile days - cumulative values
tCum1 <- dat_DryR %>% 
  select(contains(c("Date","Reach", "Cum"))) %>%   
  group_by(Date) %>% 
  mutate(DiversionCum_cfs0 = case_when(Reach == "R2" ~ 0,
                                    TRUE ~ DiversionCum_cfs)) %>% 
  mutate(Precip_mm = sum(PrecipCum_mm, na.rm = T),
         Temp_C = mean(TempCum_C, na.rm = T),
         Diversion_cfs = sum(DiversionCum_cfs0, na.rm = T),
         Returns_cfs = sum(ReturnsCum_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>%
  mutate(Discharge_cfs = DischargeCum_cfs) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R123")) %>% 
   select(Date, Reach, Precip_mm:Discharge_cfs) #okay I renamed under mutate


PredCum_Dry_1state <- tCum1 %>% 
  rename(PrecipCum_mm = Precip_mm, TempCum_C = Temp_C, DischargeCum_cfs = Discharge_cfs,
         DiversionCum_cfs = Diversion_cfs, ReturnsCum_cfs = Returns_cfs) %>% 
  pivot_longer(cols = PrecipCum_mm:DischargeCum_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>%
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

  #Extent change - change from prior day
tChng1 <- dat_DryR %>% 
  select(contains(c("Date","Reach", "Chng_"))) %>%   
  group_by(Date) %>% 
  mutate(DiversionChng_cfs0 = case_when(Reach == "R2" ~ 0,
                                       TRUE ~ DiversionChng_cfs)) %>% 
  mutate(Precip_mm = sum(PrecipChng_mm, na.rm = T),
         Temp_C = mean(TempChng_C, na.rm = T),
         Diversion_cfs = sum(DiversionChng_cfs0, na.rm = T),
         Returns_cfs = sum(ReturnsChng_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  filter(Reach == "R1") %>% 
  mutate(Discharge_cfs = DischargeChng_cfs) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R123")) %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  select(Date, Reach, Precip_mm:Discharge_cfs) #okay I renamed under mutate


PredChng_Dry_1state <- tChng1 %>% 
  rename(PrecipChng_mm = Precip_mm, TempChng_C = Temp_C, DischargeChng_cfs = Discharge_cfs,
         DiversionChng_cfs = Diversion_cfs, ReturnsChng_cfs = Returns_cfs) %>% 
  pivot_longer(cols = PrecipChng_mm:DischargeChng_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>%
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

# reduced to months 4-10 
# write.csv(Pred_Dry_1state, "Data/Processed/MARSS_Covariates/Reduced/Pred_Dry_1stateReduced.csv", row.names = T)
# write.csv(PredCum_Dry_1state, "Data/Processed/MARSS_Covariates/Reduced/PredCum_Dry_1stateReduced.csv", row.names = T)
# write.csv(PredChng_Dry_1state, "Data/Processed/MARSS_Covariates/Reduced/PredChng_Dry_1stateReduced.csv", row.names = T)

#check z-scoring
apply(Pred_Dry_1state, 1, var)

#2 states diversion####

#zscore for 2 states#
Pred_Div_2states <- dat_DivR %>% 
  select(-c(contains(c("Cum", "Chng_")))) %>% 
  select(Date, Precip_mm:Reach) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  group_by(Reach) %>% 
  pivot_longer(cols = Precip_mm:Returns_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>%
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

PredCum_Div_2states <- dat_DivR %>% 
  select(contains(c("Date","Reach", "Cum"))) %>%   
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = PrecipCum_mm:ReturnsCum_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

PredChng_Div_2states <- dat_DivR %>% 
  select(contains(c("Date","Reach", "Chng_"))) %>%   
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = PrecipChng_mm:ReturnsChng_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")


# reduced to months 4-10
# write.csv(Pred_Div_2states, "Data/Processed/MARSS_Covariates/Reduced/Pred_Div_2statesReduced.csv", row.names = T)
# write.csv(PredCum_Div_2states, "Data/Processed/MARSS_Covariates/Reduced/PredCum_Div_2statesReduced.csv", row.names = T)
# write.csv(PredChng_Div_2states, "Data/Processed/MARSS_Covariates/Reduced/PredChng_Div_2statesReduced.csv", row.names = T)

#check z-scoring
apply(Pred_Div_2states, 1, var)

