library(tidyverse)

dat <- read.csv("Data/Processed/2002_2021_WetDryTenths.csv")

temp <- dat %>% 
  mutate(RoundRM = floor(RMTenthDry), Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(Date >= "2010-01-01") %>% 
  distinct(Date, RoundRM, DryRM) %>% 
  group_by(RoundRM) %>% 
  mutate(SumDry = sum(DryRM)) %>% 
  group_by(RoundRM) %>% 
  mutate(PerDry = 1-(SumDry/length(SumDry))) %>% 
  distinct(RoundRM, PerDry)

write.csv(temp, "Data/Processed/SummaryForGIS.csv", row.names = F)


