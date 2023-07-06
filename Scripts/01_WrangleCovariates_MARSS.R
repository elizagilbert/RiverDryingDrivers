#read Me ####
#The purpose of this script is to wrangle the covariates for each of the 
#different spatial MARSS hypothesis for both drying and diversion reaches

#library ####
library(MARSS)
library(tidyverse)
library(lubridate)
library(zoo)


#data ####
dat_DryR <- read.csv("Data/Processed/DryingSubreachData.csv", header = T)
dat_DivR <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T)

#3 states drying ####

#zscore for 3 states#
Pred_Dry_3states <-dat_DryR %>% 
  select(-c(contains("Cum"))) %>% 
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
  select(1,7,8,10,12,14,15) %>% 
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

write.csv(Pred_Dry_3states, "Data/Processed/MARSS_Covariates/Pred_Dry_3states.csv", row.names = T)
write.csv(PredCum_Dry_3states, "Data/Processed/MARSS_Covariates/PredCum_Dry_3states.csv", row.names = T)

#check z-scoring
apply(Pred_Dry_3states, 1, var)

#2 states drying ####

#zscore for 2 states#

#combine R1 and R2 and leave R3
#total precip; average temp; average discharge; diversion = diversion because only one diversion; total returns 
temp1 <- dat_DryR %>% 
  select(-c(contains("Cum"))) %>% 
  select(Date, Precip_mm:Reach) %>%
  filter(Reach == "R3")

temp2 <- dat_DryR %>% 
  select(-c(contains("Cum"))) %>% 
  select(Date, Precip_mm:Reach) %>% 
  filter(Reach == "R1" | Reach == "R2") %>% 
  group_by(Date) %>% 
  mutate(Precip_mm_R1_2 = sum(Precip_mm, na.rm = T),
         Temp_C_R1_2 = mean(Temp_C, na.rm = T),
         Discharge_cfs_R1_2 = mean(Discharge_cfs, na.rm = T),
         Diversion_cfs_R1_2 = Diversion_cfs,
         Returns_cfs_R1_2 = sum(Returns_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R1_2")) %>% 
  select(Date, Reach, Precip_mm_R1_2:Returns_cfs_R1_2) %>% 
  rename(Precip_mm = Precip_mm_R1_2, Temp_C = Temp_C_R1_2,
         Discharge_cfs = Discharge_cfs_R1_2, Diversion_cfs = Diversion_cfs_R1_2,
         Returns_cfs = Returns_cfs_R1_2) %>% 
  rbind(temp1)

Pred_Dry_2states <- temp2 %>% 
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

tempcum1 <- dat_DryR %>% 
  select(1,7,8,10,12,14,15) %>% 
  filter(Reach == "R3") %>% 
  rename(PrecipCum_mm_R1_2 = PrecipCum_mm, TempCum_C_R1_2 = TempCum_C,
         DischargeCum_cfs_R1_2 = DischargeCum_cfs, DiversionCum_cfs_R1_2 = DiversionCum_cfs,
         ReturnsCum_cfs_R1_2 = ReturnsCum_cfs)

tempcum2 <- dat_DryR %>% 
  select(1,7,8,10,12,14,15) %>%  
  filter(Reach == "R1" | Reach == "R2") %>% 
  group_by(Date) %>% 
  mutate(PrecipCum_mm_R1_2 = sum(PrecipCum_mm, na.rm = T),
         TempCum_C_R1_2 = mean(TempCum_C, na.rm = T),
         DischargeCum_cfs_R1_2 = mean(DischargeCum_cfs, na.rm = T),
         DiversionCum_cfs_R1_2 = DiversionCum_cfs,
         ReturnsCum_cfs_R1_2 = sum(ReturnsCum_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R1_2")) %>% 
  select(Date, PrecipCum_mm_R1_2:ReturnsCum_cfs_R1_2, Reach) %>% 
  rbind(tempcum1)

PredCum_Dry_2states <- tempcum2 %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = PrecipCum_mm_R1_2:ReturnsCum_cfs_R1_2, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

write.csv(Pred_Dry_2states, "Data/Processed/MARSS_Covariates/Pred_Dry_2states.csv", row.names = T)
write.csv(PredCum_Dry_2states, "Data/Processed/MARSS_Covariates/PredCum_Dry_2states.csv", row.names = T)

#check z-scoring
apply(PredCum_Dry_2states, 1, var)

#1 state drying ####

#zscore for 1 state#
#combine R1-R3
#total precip; average temp; average discharge; total diversion; total returns ; 

t1 <- dat_DryR %>% 
  select(-c(contains("Cum"))) %>% 
  select(Date, Precip_mm:Reach) %>% 
  group_by(Date) %>% 
  mutate(Diversion_cfs0 = case_when(Reach == "R2" ~ 0,
                                    TRUE ~ Diversion_cfs)) %>% 
  mutate(Precip_mm_R123 = sum(Precip_mm, na.rm = T),
         Temp_C_R123 = mean(Temp_C, na.rm = T),
         Discharge_cfs_R123 = mean(Discharge_cfs, na.rm = T),
         Diversion_cfs_R123 = sum(Diversion_cfs0, na.rm = T),
         Returns_cfs_R123 = sum(Returns_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Reach = str_replace(Reach, "R1", "R123")) %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  select(Date, Reach, Precip_mm_R123:Returns_cfs_R123) %>% 
  rename(Precip_mm = Precip_mm_R123, Temp_C = Temp_C_R123,
         Discharge_cfs = Discharge_cfs_R123, Diversion_cfs = Diversion_cfs_R123,
         Returns_cfs = Returns_cfs_R123)


Pred_Dry_1state <- t1 %>% 
  pivot_longer(cols = Precip_mm:Returns_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>%
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

tCum1 <- dat_DryR %>% select(1,7,8,10,12,14,15) %>% 
  group_by(Date) %>% 
  mutate(DiversionCum_cfs0 = case_when(Reach == "R2" ~ 0,
                                    TRUE ~ DiversionCum_cfs)) %>% 
  mutate(Precip_mm = sum(PrecipCum_mm, na.rm = T),
         Temp_C = mean(TempCum_C, na.rm = T),
         Discharge_cfs = mean(DischargeCum_cfs, na.rm = T),
         Diversion_cfs = sum(DiversionCum_cfs0, na.rm = T),
         Returns_cfs = sum(ReturnsCum_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Reach = str_replace(Reach, "R1", "R123")) %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  select(Date, Reach, Precip_mm:Returns_cfs) 


PredCum_Dry_1state <- tCum1 %>% 
  pivot_longer(cols = Precip_mm:Returns_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>%
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

write.csv(Pred_Dry_1state, "Data/Processed/MARSS_Covariates/Pred_Dry_1state.csv", row.names = T)
write.csv(PredCum_Dry_1state, "Data/Processed/MARSS_Covariates/PredCum_Dry_1state.csv", row.names = T)

#check z-scoring
apply(Pred_Dry_1state, 1, var)

#2 states diversion####

#zscore for 2 states#
Pred_Div_2states <- dat_DivR %>% 
  select(-c(contains("Cum"))) %>% 
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
  select(1,6,8,10,12,14,15) %>% 
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


write.csv(Pred_Div_2states, "Data/Processed/MARSS_Covariates/Pred_Div_2states.csv", row.names = T)
write.csv(PredCum_Div_2states, "Data/Processed/MARSS_Covariates/PredCum_Div_2states.csv", row.names = T)

#check z-scoring
apply(Pred_Div_2states, 1, var)

#1 state diversion####
t2 <- dat_DivR %>% 
  select(-c(contains("Cum"))) %>% 
  select(Date, Precip_mm:Reach) %>% 
  group_by(Date) %>% 
  mutate(Precip_mm_R1_2 = sum(Precip_mm, na.rm = T),
         Temp_C_R1_2 = mean(Temp_C, na.rm = T),
         Discharge_cfs_R1_2 = mean(Discharge_cfs, na.rm = T),
         Diversion_cfs_R1_2 = sum(Diversion_cfs, na.rm = T),
         Returns_cfs_R1_2 = sum(Returns_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R1_2")) %>% 
  select(Date, Reach, Precip_mm_R1_2:Returns_cfs_R1_2) %>% 
  rename(Precip_mm = Precip_mm_R1_2, Temp_C = Temp_C_R1_2,
         Discharge_cfs = Discharge_cfs_R1_2, Diversion_cfs = Diversion_cfs_R1_2,
         Returns_cfs = Returns_cfs_R1_2)


Pred_Div_1state <- t2 %>% 
  pivot_longer(cols = Precip_mm:Returns_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

tCum2 <- dat_DivR %>% select(1,6,8,10,12,14,15) %>% 
  group_by(Date) %>% 
  mutate(Precip_mm = sum(PrecipCum_mm, na.rm = T),
         Temp_C = mean(TempCum_C, na.rm = T),
         Discharge_cfs = mean(DischargeCum_cfs, na.rm = T),
         Diversion_cfs = sum(DiversionCum_cfs, na.rm = T),
         Returns_cfs = sum(ReturnsCum_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R1_2")) %>% 
  select(Date, Reach, Precip_mm:Returns_cfs) 

PredCum_Div_1state <- tCum2 %>% 
  pivot_longer(cols = Precip_mm:Returns_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>%
  arrange(Date, Predictor, Reach) %>%
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

write.csv(Pred_Div_1state, "Data/Processed/MARSS_Covariates/Pred_Div_1state.csv", row.names = T)
write.csv(PredCum_Div_1state, "Data/Processed/MARSS_Covariates/PredCum_Div_1state.csv", row.names = T)

apply(Pred_Div_1state, 1, var)
