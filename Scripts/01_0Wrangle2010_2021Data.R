#Read Me ####
#purpose of this script is to combine the 2002-2021 data and
#break the 2019+ interval data to the tenth of 

#Libraries ####
library(tidyverse)
library(lubridate)
library(readxl)

#Sequencing 2019-2021 Don't rerun if not necessary ####
#Data 2019 
dat2019_Isleta <- read_xlsx("Data/Raw/RiverEyes2019SummaryDataAndColorCharts.xlsx",
                            sheet = "Running Data Isleta", range = cell_limits(c(1,1), c(NA,13))) %>% 
  rename(DownStreamSeg = 5, UpstreamSeg = 6) %>% 
  select(Date, Reach, DownStreamSeg, UpstreamSeg, Dry)

dat2019_SanAcacia <- read_excel("Data/Raw/RiverEyes2019SummaryDataAndColorCharts.xlsx",
                                sheet = "Running Data San Acacia", range = cell_limits(c(1,1), c(NA,13))) %>% 
  rename(DownStreamSeg = 5, UpstreamSeg = 6) %>% 
  select(Date, Reach, DownStreamSeg, UpstreamSeg, Dry)

dat2019 <- rbind(dat2019_Isleta, dat2019_SanAcacia) %>% 
  filter(Dry == "YES") %>% 
  mutate(Dry = case_when(Dry == "YES" ~ 0,
                         TRUE ~ 0)) %>% 
  mutate(UpstreamSeg = round(UpstreamSeg,1), DownStreamSeg = round(DownStreamSeg, 1))

dat2019_tenths <- dat2019 %>% 
  mutate(seq = map2(DownStreamSeg, UpstreamSeg, ~seq(.x, .y, by = 0.1))) %>% 
  unnest_wider(seq)

dat2019_tenths_longer <- dat2019_tenths %>% 
  select(!Dry) %>% 
  pivot_longer(cols = 4:178, names_to = "RM", values_to = "RMTenthDry") %>% 
  drop_na(RMTenthDry) %>% 
  select(!RM) %>% 
  mutate(Dry = 0)

#write.csv(dat2019_tenths_longer, "Data/Processed/2019_tenths.csv", row.names = F)

#Data 2020 
filter2 <- c("date", "text", "numeric", "numeric", "numeric", "numeric", 
            "numeric","text", "date", "text", "numeric", "text")

dat2020_IsletaPer <- read_xlsx("Data/Raw/RiverEyes2020SummaryData.xlsx",
                               sheet = "Running Data Isleta Peralta", 
                               range = cell_limits(c(1,1), c(NA,12)), col_types = filter2) %>% 
  rename(DownStreamSeg = 4, UpstreamSeg = 5) %>% 
  select(Date, DownStreamSeg, UpstreamSeg, Dry) %>% 
  drop_na()

dat2020_IsletaAby <- read_xlsx("Data/Raw/RiverEyes2020SummaryData.xlsx",
                               sheet = "Running Data Isleta Abeytas", 
                               range = cell_limits(c(1,1), c(NA,12)), col_types = filter2) %>% 
  rename(DownStreamSeg = 4, UpstreamSeg = 5) %>% 
  select(Date, DownStreamSeg, UpstreamSeg, Dry)
dat2020_SanASeg1a <- read_xlsx("Data/Raw/RiverEyes2020SummaryData.xlsx",
                               sheet = "Running Data San Acacia Seg 1a", 
                               range = cell_limits(c(1,1), c(NA,12)), col_types = filter2) %>% 
  rename(DownStreamSeg = 4, UpstreamSeg = 5) %>% 
  select(Date, DownStreamSeg, UpstreamSeg, Dry) %>% 
  drop_na()
dat2020_SanASeg1b <- read_xlsx("Data/Raw/RiverEyes2020SummaryData.xlsx",
                               sheet = "Running Data San Acacia Seg 1b", 
                               range = cell_limits(c(1,1), c(NA,12)), col_types = filter2) %>% 
  rename(DownStreamSeg = 4, UpstreamSeg = 5) %>% 
  select(Date, DownStreamSeg, UpstreamSeg, Dry)
dat2020_SanASeg2 <- read_xlsx("Data/Raw/RiverEyes2020SummaryData.xlsx",
                              sheet = "Running Data San Acacia Seg 2", 
                              range = cell_limits(c(1,1), c(NA,12)), col_types = filter2) %>% 
  rename(DownStreamSeg = 4, UpstreamSeg = 5) %>% 
  select(Date, DownStreamSeg, UpstreamSeg, Dry)
dat2020_SanASeg3 <- read_xlsx("Data/Raw/RiverEyes2020SummaryData.xlsx",
                              sheet = "Running Data San Acacia Seg3", 
                              range = cell_limits(c(1,1), c(NA,12)), col_types = filter2) %>% 
  rename(DownStreamSeg = 4, UpstreamSeg = 5) %>% 
  select(Date, DownStreamSeg, UpstreamSeg, Dry)

dat2020 <- rbind(dat2020_IsletaPer, dat2020_IsletaAby, dat2020_SanASeg1a, 
                 dat2020_SanASeg1b, dat2020_SanASeg2, dat2020_SanASeg3) %>% 
  filter(DownStreamSeg != 0.00) %>% 
  mutate(Dry = case_when(Dry == "YES" ~ 0,
                         TRUE ~ 0)) %>% 
  mutate(UpstreamSeg = round(UpstreamSeg,1), DownStreamSeg = round(DownStreamSeg, 1))

 #don't run if you don't have to
dat2020_tenths <- dat2020 %>% 
  mutate(seq = map2(DownStreamSeg, UpstreamSeg, ~seq(.x, .y, by = 0.1))) %>% 
  unnest_wider(seq)

dat2020_tenths_longer <- dat2020_tenths %>% 
  select(!Dry) %>% 
  pivot_longer(cols = 4:359, names_to = "RM", values_to = "RMTenthDry") %>% 
  drop_na(RMTenthDry) %>% 
  select(!RM) %>% 
  mutate(Dry = 0)

#write.csv(dat2020_tenths_longer, "Data/Processed/2020_tenths.csv", row.names = F)

#Data 2021 
dat2021_IsletaPer <- read_xlsx("Data/Raw/2021RunningData.xlsx",
                               sheet = "Running Data Isleta Peralta", 
                               range = cell_limits(c(1,1), c(NA,5))) 
dat2021_Abeytas <- read_xlsx("Data/Raw/2021RunningData.xlsx",
                             sheet = "Running Data Isleta Abeytas", 
                             range = cell_limits(c(1,1), c(NA,5)))  
dat2021_SanA <- read_xlsx("Data/Raw/2021RunningData.xlsx",
                          sheet = "Running Data San Acacia", 
                          range = cell_limits(c(1,1), c(NA,5)))  
dat2021 <- rbind(dat2021_IsletaPer, dat2021_Abeytas, dat2021_SanA) %>% 
  rename(DownStreamSeg = 4, UpstreamSeg = 5) %>% 
  select(Date, DownStreamSeg, UpstreamSeg) %>% 
  filter(DownStreamSeg != 0.00) %>% 
  mutate(Dry = 0, UpstreamSeg = round(UpstreamSeg, 1), DownStreamSeg = round(DownStreamSeg, 1))

dat2021_tenths <- dat2021 %>% 
  mutate(seq = map2(DownStreamSeg, UpstreamSeg, ~seq(.x, .y, by = 0.1))) %>% 
  unnest_wider(seq)

dat2021_tenths_longer <- dat2021_tenths %>% 
  select(!Dry) %>% 
  pivot_longer(cols = 4:323, names_to = "RM", values_to = "RMTenthDry") %>% 
  drop_na(RMTenthDry) %>% 
  select(!RM) %>% 
  mutate(Dry = 0)

#write.csv(dat2021_tenths_longer, "Data/Processed/2021_tenths.csv", row.names = F)


#Adding 2010 to 2019 -2021 ####
dat2010_2018 <- read.csv("Data/Raw/RiverEyesTenthMile2002_2018.csv") %>% 
  select(Year, X) %>% 
  rename(Date = 1, RMTenthDry = 2) %>% 
  mutate(Dry = 0, Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  filter(Date >= as.Date("2010-01-01")) %>% 
  distinct(Date, RMTenthDry, .keep_all = T)
dat2019 <- read.csv("Data/Processed/2019_tenths.csv") %>% 
  select(Date, RMTenthDry, Dry) %>% 
  mutate(RMTenthDry = round(RMTenthDry, 1), Date = as.Date(Date)) %>% 
  distinct(Date, RMTenthDry, .keep_all = T)
dat2020 <- read.csv("Data/Processed/2020_tenths.csv") %>% 
  select(Date, RMTenthDry, Dry) %>% 
  mutate(RMTenthDry = round(RMTenthDry, 1), Date = as.Date(Date)) %>% 
  distinct(Date, RMTenthDry, .keep_all = T)
dat2021 <- read.csv("Data/Processed/2021_tenths.csv")%>% 
  select(Date, RMTenthDry, Dry) %>% 
  mutate(RMTenthDry = round(RMTenthDry, 1), Date = as.Date(Date)) %>% 
  distinct(Date, RMTenthDry, .keep_all = T)

#bind 2010_2021
dat2010_2021 <- as.data.frame(rbind(dat2019, dat2020, dat2021, dat2010_2018))


  #get all dates and tenths mile to join to data
DatesAll <- as.data.frame(seq(as.Date("2010/1/1"), as.Date("2021/12/31"), "day")) %>% 
  rename(Date = 1)
RM_tenths <- read.csv("Data/Raw/RM_seq_tenths.csv")

FillDatesRM <- crossing(DatesAll, RM_tenths) 

  #join to get wet days
dat2_0 <- FillDatesRM %>% 
  left_join(dat2010_2021, by = c("Date", "RMTenthDry")) %>% 
  mutate(Dry2 = as.factor(case_when(Dry == 0 ~ 0,
                          TRUE ~ 1))) %>% 
  select(Date, RMTenthDry, Dry2) %>% 
  rename(Date = 1, DryRM = 3)

#there are 4,383 days between 2010-01-01 and 2012-12-31 including the first day
#there are 1,161 tenths of a mile between rm 170 and 54
#this equals 5,088,663 and is the same number of records in dat2_0

write.csv(dat2_0, "Data/Processed/2010_2021_WetDryTenths.csv")





