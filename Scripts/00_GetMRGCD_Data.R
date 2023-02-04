#Read me ####
#The purpose of this script is to get the diversion and river return data from the 
#Middle Rio Grande Conservancy district webpage https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/gage_list.html

#Libraries ####
library(readr)
library(tidyverse)

#Data diversions ####
CHICN_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_CHICN.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "CHICN")%>% 
  rename(Discharge_cfs = 7) %>%
  group_by(Date) %>% 
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>% 
  distinct(Date, DivName, MnDischarge_cfs)%>% 
  ungroup()

PERCN_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_PERCN.txt", skip = 1, col_names = T)%>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "PERCN")%>% 
  rename(Discharge_cfs = 7) %>%
  group_by(Date) %>% 
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>% 
  distinct(Date, DivName, MnDischarge_cfs)%>% 
  ungroup()

CACCN_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_CACCN.txt", skip = 1, col_names = T)%>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "CACCN")%>% 
  rename(Discharge_cfs = 7) %>%
  group_by(Date) %>% 
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>% 
  distinct(Date, DivName, MnDischarge_cfs)%>% 
  ungroup()

CHACN_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_CHACN.txt", skip = 1, col_names = T)%>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "CHACN")%>% 
  rename(Discharge_cfs = 7) %>%
  group_by(Date) %>% 
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>% 
  distinct(Date, DivName, MnDischarge_cfs)%>% 
  ungroup()

BELCN_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_BELCN.txt", skip = 1, col_names = T)%>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "BELCN")%>% 
  rename(Discharge_cfs = 7) %>%
  group_by(Date) %>% 
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>% 
  distinct(Date, DivName, MnDischarge_cfs)%>% 
  ungroup()
  
SNA02_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_SNA02.txt", skip = 1, col_names = T)
write.csv(SNA02_dat, "Data/Raw/MRGCDGageSNA02.csv", row.names = F)

SNA02_dat2 <- SNA02_dat %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(Date <= as.Date("2021-12-31")) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "SNA02") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()
  
Diversion_dat <- rbind(BELCN_dat, CACCN_dat, CHACN_dat, CHICN_dat, PERCN_dat, SNA02_dat2) %>% 
  mutate(MnDischarge_cfs = case_when(MnDischarge_cfs <0 ~ 0, 
                                     TRUE ~ MnDischarge_cfs))
Diversion_dat$MnDischarge_cfs[is.nan(Diversion_dat$MnDischarge_cfs)]<-NA
write.csv(Diversion_dat, "Data/Processed/MRGCD_diversion.csv", row.names = F)

#Data returns ####
ALJWW_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_ALJWW.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(Date <= as.Date("2021-12-31")) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "ALJWW") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

Two40WW_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_240WW.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(Date <= as.Date("2021-12-31")) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "240WW") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

BELDR_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_BELDR.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "BELDR") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

FCRPS_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_FCRPS.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(Date <= as.Date("2021-12-31")) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "FCRPS") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

FD3WW_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_FD3WW.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "FD3WW") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

LCZWW_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_LCZWW.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "LCZWW") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

LP1DR_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_LP1DR.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "LP1DR") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

LP2DR_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_LP2DR.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "LP2DR") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

LSJDR_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_LSJDR.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "LSJDR") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

NBYPS_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_NBYPS.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "NBYPS") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

NCPPS_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_NCPPS.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "NCPPS") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

PERWW_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_PERWW.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "PERWW") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

SABDR_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_SABDR.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "SABDR") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

SBYPS_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_SBYPS.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "SBYPS") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

SFRDR_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_SFRDR.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "SFRDR") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

STYWW_dat <- read_table("https://www.usbr.gov/uc/albuq/water/ETtoolboxV2/data/mrgcd_STYWW.txt", skip = 1, col_names = T) %>% 
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))%>%
  filter(between(Date, as.Date("2002-01-01"), as.Date("2021-12-31"))) %>%
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(DivName = "STYWW") %>% 
  rename(Discharge_cfs = 7) %>%
  mutate(Discharge_cfs = as.numeric(Discharge_cfs)) %>% 
  group_by(Date) %>%
  mutate(MnDischarge_cfs = mean(Discharge_cfs, na.rm = TRUE)) %>%
  distinct(Date, DivName, MnDischarge_cfs)%>%
  ungroup()

Return_dat <- rbind(ALJWW_dat, Two40WW_dat, BELDR_dat, FCRPS_dat, FD3WW_dat, LCZWW_dat, LP1DR_dat, LP2DR_dat,
                    LSJDR_dat, NBYPS_dat, NCPPS_dat, PERWW_dat, SABDR_dat, SBYPS_dat, SFRDR_dat,
                    STYWW_dat) %>% 
  mutate(MnDischarge_cfs = case_when(MnDischarge_cfs <0 ~ 0, 
                                     TRUE ~ MnDischarge_cfs))
Return_dat$MnDischarge_cfs[is.nan(Return_dat$MnDischarge_cfs)]<-NA
write.csv(Return_dat, "Data/Processed/MRGCD_returns.csv", row.names = F)


