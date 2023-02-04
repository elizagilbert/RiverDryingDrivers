#Read Me####
#The purpose of this script is to get the USGS gage data

#Libraries ####
library(dataRetrieval)
library(tidyverse)
library(lubridate)

#make dataframe to use for NWIS function 
site_name <- c("BosqueFarms",	"StateHwy346","Bernardo", "RioPuerco",	"San Acacia",
          "Escondida", 	"SanAntonio", "LfccSanMarcial",	"SanMarcial")
site_num <- c("08331160",	"08331510",	"08332010",	"08353000",	"08354900",	"08355050",	"08355490",	
              '08358300',	"08358400")
parameter <- c(rep("00060", 9))
start_date <- c("2007-10-01","2006-10-09",'2002-01-01',"2002-01-01","2002-01-01",'2005-10-01',
                '2005-10-01','2002-01-01','2002-01-01')
end_date <- c(rep("2021-12-31", 9))

usgs_dat <- cbind(site_name, site_num, parameter, start_date, end_date)
usgs_dat_df <- as.data.frame(cbind(site_name, site_num, parameter, start_date, end_date)) %>% 
  rename(site_no = site_num)
#Gage Data
RGBosque_gage <- readNWISdata(sites = usgs_dat[10],
                         parameterCd = "00060",
                         startDate = usgs_dat[28],
                         endDate = usgs_dat[37])

RGHwy346 <- readNWISdata(sites = usgs_dat[11],
                            parameterCd = "00060",
                            startDate = usgs_dat[29],
                            endDate = usgs_dat[38])

RGBernardo <- readNWISdata(sites = usgs_dat[12],
                            parameterCd = "00060",
                            startDate = usgs_dat[30],
                            endDate = usgs_dat[39])

RioPuercoBernardo <- readNWISdata(sites = usgs_dat[13],
                           parameterCd = "00060",
                           startDate = usgs_dat[31],
                           endDate = usgs_dat[40])

RGSanAcacia <- readNWISdata(sites = usgs_dat[14],
                                  parameterCd = "00060",
                                  startDate = usgs_dat[32],
                                  endDate = usgs_dat[41])

RGEscondida <- readNWISdata(sites = usgs_dat[15],
                            parameterCd = "00060",
                            startDate = usgs_dat[33],
                            endDate = usgs_dat[42])

RGSanAntonio <- readNWISdata(sites = usgs_dat[16],
                            parameterCd = "00060",
                            startDate = usgs_dat[34],
                            endDate = usgs_dat[43])

LowFlowConveySanMarcial <- readNWISdata(sites = usgs_dat[17],
                             parameterCd = "00060",
                             startDate = usgs_dat[35],
                             endDate = usgs_dat[44])

RGSanMarcial <- readNWISdata(sites = usgs_dat[18],
                             parameterCd = "00060",
                             startDate = usgs_dat[36],
                             endDate = usgs_dat[45])

#bind data
discharge_dat <- as.data.frame(rbind(LowFlowConveySanMarcial, RGBernardo, RGBosque_gage, RGEscondida,
                      RGHwy346, RGSanAcacia, RGSanAntonio, RGSanMarcial, RioPuercoBernardo)) %>% 
  left_join(usgs_dat_df, by = "site_no") %>% 
  rename(Discharge_cfs = 4) %>% 
  select(site_no, dateTime, Discharge_cfs, site_name) %>% 
  group_by(site_name) %>% 
  complete(dateTime = seq.Date(as.Date(min(dateTime)), as.Date(max(dateTime)), by = "day"))

#fill NAs
discharge_dat %>% 
  group_by(site_name, year(dateTime)) %>% 
  summarise_all(~sum(is.na(.)))
  

#write.csv(discharge_dat, "Data/Processed/USGS_discharge.csv", row.names = F)
