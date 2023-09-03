#read Me ####
#The purpose of this script is to wrangle the covariates for each of the 
#smaller scale spatial structure hypotheses for the drying reaches using Mile Days
#4 hypothesis = 1) 3 states, 2a) 2 states top and middle, 2b) 2 states middle and btm, 3) 1 state

#library ####
library(tidyverse)
library(lubridate)
library(zoo)

#data ####
#data already reduced to irrigation months 4-10
datR1 <- read.csv("Data/Processed/SmallScale/SmScl_R1MD_RespCov.csv", header = T)
datR2 <- read.csv("Data/Processed/SmallScale/SmScl_R2MD_RespCov.csv", header = T)
datR3 <- read.csv("Data/Processed/SmallScale/SmScl_R3MD_RespCov.csv", header = T)

#R1 1 state ####
tCum1 <- datR1 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>%   
  group_by(Date) %>% 
  mutate(DiversionCum_cfs0 = case_when(Reach == "R1" ~ 0,
                                       Reach == "R2" ~ 0,
                                       TRUE ~ DiversionCum_cfs),
         PrecipCum_mm0 = case_when(Reach == "R1" ~ 0,
                                       Reach == "R2" ~ 0,
                                       TRUE ~ PrecipCum_mm),
         TempCum_C0 = case_when(Reach == "R1" ~ 0,
                                   Reach == "R2" ~ 0,
                                   TRUE ~ TempCum_C),
         DiversionCum_cfs0 = case_when(Reach == "R1" ~ 0,
                                   Reach == "R2" ~ 0,
                                   TRUE ~ DiversionCum_cfs)) %>% 
  mutate(Precip_mm = sum(PrecipCum_mm, na.rm = T),
         Temp_C = mean(TempCum_C0, na.rm = T),
         Diversion_cfs = sum(DiversionCum_cfs0, na.rm = T),
         Returns_cfs = sum(ReturnsCum_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>%
  mutate(Discharge_cfs = DischargeCum_cfs) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R123")) %>% 
  select(Date, Reach, Precip_mm:Discharge_cfs) #okay I renamed under mutate


PredCum_Dry_R1_1state <- tCum1 %>% 
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

write.csv(PredCum_Dry_R1_1state, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R1_1state.csv", row.names = T)

#R1 2a states ####
#combine R1 and R2 and leave R3
tempcum1 <- datR1 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>% 
  filter(Reach == "R3") %>% 
  rename(PrecipCum_mm_R1_2 = PrecipCum_mm, TempCum_C_R1_2 = TempCum_C,
         DischargeCum_cfs_R1_2 = DischargeCum_cfs, DiversionCum_cfs_R1_2 = DiversionCum_cfs,
         ReturnsCum_cfs_R1_2 = ReturnsCum_cfs)

tempcum2 <- datR1 %>% 
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

PredCum_Dry_R1_2astates <- tempcum2 %>% 
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

write.csv(PredCum_Dry_R1_2astates, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R1_2a_states.csv", row.names = T)

#R1 2b states ####
#combine R2 and R3 and leave R1
tempcum12 <- datR1 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>% 
  filter(Reach == "R1") %>% 
  rename(PrecipCum_mm_R2_3 = PrecipCum_mm, TempCum_C_R2_3 = TempCum_C,
         DischargeCum_cfs_R2_3 = DischargeCum_cfs, DiversionCum_cfs_R2_3 = DiversionCum_cfs,
         ReturnsCum_cfs_R2_3 = ReturnsCum_cfs)

tempcum22 <- datR1 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>%   
  filter(Reach == "R2" | Reach == "R3") %>% 
  group_by(Date) %>% 
  mutate(PrecipCum_mm_R2_3 = sum(PrecipCum_mm, na.rm = T),
         TempCum_C_R2_3 = mean(TempCum_C, na.rm = T),
         DiversionCum_cfs_R2_3 = DiversionCum_cfs,
         ReturnsCum_cfs_R2_3 = sum(ReturnsCum_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(DischargeCum_cfs_R2_3 = DischargeCum_cfs) %>% 
  mutate(Reach = str_replace(Reach, "R2", "R1_3")) %>% 
  select(Date, PrecipCum_mm_R2_3:DischargeCum_cfs_R2_3, Reach) %>% 
  rbind(tempcum12)

PredCum_Dry_R1_2bstates <- tempcum22 %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = PrecipCum_mm_R2_3:DischargeCum_cfs_R2_3, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

write.csv(PredCum_Dry_R1_2bstates, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R1_2b_states.csv", row.names = T)

#R1 3 states ####
Pred_MD_R1_3states <-datR1 %>% 
  select(Date, PrecipCum_mm:Reach) %>% 
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

write.csv(Pred_MD_R1_3states, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R1_3states.csv", row.names = T)


#R2 1 state ####
tCumR2 <- datR2 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>%   
  group_by(Date) %>% 
  mutate(DiversionCum_cfs0 = case_when(Reach == "R1" ~ 0,
                                       Reach == "R2" ~ 0,
                                       TRUE ~ DiversionCum_cfs),
         PrecipCum_mm0 = case_when(Reach == "R1" ~ 0,
                                   Reach == "R2" ~ 0,
                                   TRUE ~ PrecipCum_mm),
         TempCum_C0 = case_when(Reach == "R1" ~ 0,
                                Reach == "R2" ~ 0,
                                TRUE ~ TempCum_C),
         DiversionCum_cfs0 = case_when(Reach == "R1" ~ 0,
                                       Reach == "R2" ~ 0,
                                       TRUE ~ DiversionCum_cfs)) %>% 
  mutate(Precip_mm = sum(PrecipCum_mm, na.rm = T),
         Temp_C = mean(TempCum_C0, na.rm = T),
         Diversion_cfs = sum(DiversionCum_cfs0, na.rm = T),
         Returns_cfs = sum(ReturnsCum_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>%
  mutate(Discharge_cfs = DischargeCum_cfs) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R123")) %>% 
  select(Date, Reach, Precip_mm:Discharge_cfs) #okay I renamed under mutate


PredCum_Dry_R2_1state <- tCumR2 %>% 
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

write.csv(PredCum_Dry_R2_1state, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R2_1state.csv", row.names = T)

#R2 2a states ####
#combine R1 and R2 and leave R3
tempcumR2_1 <- datR2 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>% 
  filter(Reach == "R3") %>% 
  rename(PrecipCum_mm_R1_2 = PrecipCum_mm, TempCum_C_R1_2 = TempCum_C,
         DischargeCum_cfs_R1_2 = DischargeCum_cfs, DiversionCum_cfs_R1_2 = DiversionCum_cfs,
         ReturnsCum_cfs_R1_2 = ReturnsCum_cfs)

tempcumR2_2 <- datR2 %>% 
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
  rbind(tempcumR2_1)

PredCum_Dry_R2_2astates <- tempcumR2_2 %>% 
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

write.csv(PredCum_Dry_R2_2astates, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R2_2a_states.csv", row.names = T)

#R2 2b states ####
#combine R2 and R3 and leave R1
tempcumR2_12 <- datR2 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>% 
  filter(Reach == "R1") %>% 
  rename(PrecipCum_mm_R2_3 = PrecipCum_mm, TempCum_C_R2_3 = TempCum_C,
         DischargeCum_cfs_R2_3 = DischargeCum_cfs, DiversionCum_cfs_R2_3 = DiversionCum_cfs,
         ReturnsCum_cfs_R2_3 = ReturnsCum_cfs)

tempcumR2_22 <- datR2 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>%   
  filter(Reach == "R2" | Reach == "R3") %>% 
  group_by(Date) %>% 
  mutate(PrecipCum_mm_R2_3 = sum(PrecipCum_mm, na.rm = T),
         TempCum_C_R2_3 = mean(TempCum_C, na.rm = T),
         DiversionCum_cfs_R2_3 = DiversionCum_cfs,
         ReturnsCum_cfs_R2_3 = sum(ReturnsCum_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(DischargeCum_cfs_R2_3 = DischargeCum_cfs) %>% 
  mutate(Reach = str_replace(Reach, "R2", "R1_3")) %>% 
  select(Date, PrecipCum_mm_R2_3:DischargeCum_cfs_R2_3, Reach) %>% 
  rbind(tempcumR2_12)

PredCum_Dry_R2_2bstates <- tempcumR2_22 %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = PrecipCum_mm_R2_3:DischargeCum_cfs_R2_3, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

write.csv(PredCum_Dry_R2_2bstates, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R2_2b_states.csv", row.names = T)

#R2 3 states ####
Pred_MD_R2_3states <-datR2 %>% 
  select(Date, PrecipCum_mm:Reach) %>% 
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

write.csv(Pred_MD_R2_3states, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R2_3states.csv", row.names = T)


#R3 1 state ####
tCumR3 <- datR3 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>%   
  group_by(Date) %>% 
  mutate(DiversionCum_cfs0 = case_when(Reach == "R1" ~ 0,
                                       Reach == "R2" ~ 0,
                                       TRUE ~ DiversionCum_cfs),
         PrecipCum_mm0 = case_when(Reach == "R1" ~ 0,
                                   Reach == "R2" ~ 0,
                                   TRUE ~ PrecipCum_mm),
         TempCum_C0 = case_when(Reach == "R1" ~ 0,
                                Reach == "R2" ~ 0,
                                TRUE ~ TempCum_C),
         DiversionCum_cfs0 = case_when(Reach == "R1" ~ 0,
                                       Reach == "R2" ~ 0,
                                       TRUE ~ DiversionCum_cfs)) %>% 
  mutate(Precip_mm = sum(PrecipCum_mm, na.rm = T),
         Temp_C = mean(TempCum_C0, na.rm = T),
         Diversion_cfs = sum(DiversionCum_cfs0, na.rm = T),
         Returns_cfs = sum(ReturnsCum_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>%
  mutate(Discharge_cfs = DischargeCum_cfs) %>% 
  mutate(Reach = str_replace(Reach, "R1", "R123")) %>% 
  select(Date, Reach, Precip_mm:Discharge_cfs) #okay I renamed under mutate


PredCum_Dry_R3_1state <- tCumR3 %>% 
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

write.csv(PredCum_Dry_R3_1state, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R3_1state.csv", row.names = T)

#R3 2a states ####
#combine R1 and R2 and leave R3
tempcumR3_1 <- datR3 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>% 
  filter(Reach == "R3") %>% 
  rename(PrecipCum_mm_R1_2 = PrecipCum_mm, TempCum_C_R1_2 = TempCum_C,
         DischargeCum_cfs_R1_2 = DischargeCum_cfs, DiversionCum_cfs_R1_2 = DiversionCum_cfs,
         ReturnsCum_cfs_R1_2 = ReturnsCum_cfs)

tempcumR3_2 <- datR3 %>% 
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
  rbind(tempcumR3_1)

PredCum_Dry_R3_2astates <- tempcumR3_2 %>% 
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

write.csv(PredCum_Dry_R3_2astates, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R3_2a_states.csv", row.names = T)

#R3 2b states ####
#combine R2 and R3 and leave R1
######had to replace Returns with 0 after zscoring
tempcumR3_12 <- datR3 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>% 
  filter(Reach == "R1") %>% 
  rename(PrecipCum_mm_R3_3 = PrecipCum_mm, TempCum_C_R3_3 = TempCum_C,
         DischargeCum_cfs_R3_3 = DischargeCum_cfs, DiversionCum_cfs_R3_3 = DiversionCum_cfs,
         ReturnsCum_cfs_R3_3 = ReturnsCum_cfs)

tempcumR3_22 <- datR3 %>% 
  select(contains(c("Date","Reach", "Cum"))) %>%   
  filter(Reach == "R2" | Reach == "R3") %>% 
  group_by(Date) %>% 
  mutate(PrecipCum_mm_R3_3 = sum(PrecipCum_mm, na.rm = T),
         TempCum_C_R3_3 = mean(TempCum_C, na.rm = T),
         DiversionCum_cfs_R3_3 = DiversionCum_cfs,
         ReturnsCum_cfs_R3_3 = sum(ReturnsCum_cfs, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  mutate(DischargeCum_cfs_R3_3 = DischargeCum_cfs) %>% 
  mutate(Reach = str_replace(Reach, "R2", "R1_3")) %>% 
  select(Date, PrecipCum_mm_R3_3:DischargeCum_cfs_R3_3, Reach) %>% 
  rbind(tempcumR3_12)

PredCum_Dry_R3_2bstates <- tempcumR3_22 %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = PrecipCum_mm_R3_3:DischargeCum_cfs_R3_3, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  mutate_at(vars(zValues), ~replace(., is.nan(.), 0)) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

write.csv(PredCum_Dry_R3_2bstates, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R3_2b_states.csv", row.names = T)

#R3 3 states ####
#####had to replace Returns with 0 after zscoring
Pred_MD_R3_3states <-datR3 %>% 
  select(Date, PrecipCum_mm:Reach) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = PrecipCum_mm:ReturnsCum_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>% 
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  mutate_at(vars(zValues), ~replace(., is.nan(.), 0)) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

write.csv(Pred_MD_R3_3states, "Data/Processed/MARSS_Covariates/SmallScale/Pred_MD_R3_3states.csv", row.names = T)

