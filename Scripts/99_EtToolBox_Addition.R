#read me ####
#The purpose of this script is the added evapotranspriation to the covariates

#libararies ####
library(tidyverse)
library(lubridate)
library(zoo)

#data ####
dat_DivR <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Year = year(Date)) %>% 
  filter(between(Year, 2011, 2018))%>% 
  filter(between(month(Date), 4, 10)) 

#ET data ####
dat_ET5 <- read.csv("Data/Raw/ET_data_R5.csv")%>% 
  select(Date, Tot_DCU_cfs) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Year = year(Date)) %>% 
  filter(between(Year, 2011, 2018)) %>% 
  complete(Date = seq.Date(as.Date("2011-01-01"), as.Date("2018-12-31"), by = "day")) %>% 
  filter(between(month(Date), 4, 10)) %>% 
  mutate(ET5 = Tot_DCU_cfs) %>% 
  select(Date, ET5)

dat_ET6 <- read.csv("Data/Raw/ET_data_R6.csv")%>% 
  select(Date, Tot_DCU_cfs) %>% 
  mutate(Date2 = as.Date(Date, format = "%Y-%m-%d")) %>% 
  complete(Date2 = seq.Date(as.Date("2011-01-01"), as.Date("2018-12-31"), by = "day"))%>% 
  mutate(Year = year(Date2),
         DOY = yday(Date2)) %>% 
  filter(between(Year, 2011, 2018)) %>% 
  filter(between(month(Date2), 4, 10)) %>% 
  group_by(DOY) %>% 
  mutate(MnDOYET = mean (Tot_DCU_cfs, na.rm = T)) %>% 
  mutate(ET6 = case_when(Tot_DCU_cfs >0 ~ Tot_DCU_cfs,
                        TRUE ~ MnDOYET)) %>% 
  ungroup() %>% 
  select(ET6)

dat_ET7 <- read.csv("Data/Raw/ET_data_R7.csv")%>% 
  select(Date, Tot_DCU_cfs) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Year = year(Date)) %>% 
  complete(Date = seq.Date(as.Date("2011-01-01"), as.Date("2018-12-31"), by = "day")) %>% 
  filter(between(Year, 2011, 2018))%>% 
  filter(between(month(Date), 4, 10)) %>% 
  mutate(Reach = "R2") %>% 
  rename(ET_cfs = Tot_DCU_cfs) %>% 
  select(Date, ET_cfs, Reach)

temp1 <- cbind(dat_ET5, dat_ET6) %>% 
  rowwise() %>% 
  mutate(ET_cfs = sum(ET5, ET6)) %>% 
  select(Date, ET_cfs) %>% 
  mutate(Reach = "R1")

temp2 <- rbind(dat_ET7, temp1)

dat_DivR_new <- dat_DivR %>% 
  left_join(temp2) %>% 
  select(Date, Reach, Precip_mm, Temp_C, Discharge_cfs, Diversion_cfs, Returns_cfs, ET_cfs) %>% 
  pivot_longer(cols = Precip_mm:ET_cfs, names_to = "Predictor", values_to = "Values") %>% 
  group_by(Predictor, Reach) %>% 
  mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
  ungroup() %>%
  arrange(Date, Predictor, Reach) %>% 
  mutate(NewRowName = paste0(Reach,"_",Predictor)) %>% 
  select(Date, zValues,NewRowName) %>% 
  pivot_wider(names_from = Date, values_from = zValues) %>% 
  column_to_rownames(var = "NewRowName")

write.csv(dat_DivR_new, "Data/Processed/DivR_ET.csv")
