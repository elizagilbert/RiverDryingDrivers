#Read Me ####
#The purpose of this script is to plot River Eyes data

#Libraries ####
library(tidyverse)
library(forcats)
library(lubridate)
library(ggforce)
library(ggplot2)

#data ####
dat <- read.csv("Data/Processed/2002_2021_WetDryTenths.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  select(!X)

temp <- dat %>% 
  filter(between(RMTenthDry, 105, 110), 
         year(Date) == 2021, month(Date)>=6) 
  

dat2 <- dat %>% filter(Date >= "2010-01-01")


#Cumulative river mile dry (i.e., duration) data ####
cum_dry <- dat %>% 
  mutate(Year = year(Date),
         DryRM2 = case_when(DryRM == 1 ~ 0,
                            DryRM == 0 ~ 1)) %>% 
  group_by(RMTenthDry) %>% 
  mutate(cum_sum = cumsum(DryRM2)) 

midpt_cumdry <- max(cum_dry$cum_sum)/2

Ann_cum_dry <- dat %>% 
  mutate(Year = year(Date),
         DryRM2 = case_when(DryRM == 1 ~ 0,
                            DryRM == 0 ~ 1)) %>% 
  group_by(Year, RMTenthDry) %>% 
  mutate(cum_sum = cumsum(DryRM2)) 

midpt_Ann_cumdry <- max(Ann_cum_dry$cum_sum)/2

#Cumulative dry heatmap ####
cum_plt <- ggplot(cum_dry, aes(x = Date, y = RMTenthDry, fill = cum_sum))+
  geom_tile()+
  xlab("") + ylab("River mile")+
  scale_y_continuous(breaks = seq(50, 170, by = 20))+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_fill_gradient2(name = "Cumulative\ndry days", low = "#99CCFF",
                       mid = "yellow", high = "red", midpoint = midpt_cumdry)+
  theme_classic()

ann_cum_plt <- ggplot(Ann_cum_dry, aes(x = Date, y = RMTenthDry, fill = cum_sum))+
  geom_tile()+
  xlab("") + ylab("River mile")+
  scale_y_continuous(breaks = seq(50, 170, by = 20))+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_fill_gradient2(name = "Annual\nCumulative\ndry days", low = "#99CCCC",
                       mid = "#FFCC66", high = "#000000", midpoint = midpt_Ann_cumdry)+
  theme_classic()

jpeg("Figures/CumulativeDying.jpeg", units="in", width=8, height=10, res=300)
gridExtra::grid.arrange(cum_plt, ann_cum_plt)
dev.off()

#Cumulative dry histogram ####
ggplot(cum_dry, aes(x=cum_sum))+
  geom_histogram()

#Daily extent dry data ####
DailyDry <- dat %>% 
  mutate(RM = floor(RMTenthDry)) %>% 
  filter(DryRM == 0) %>% 
  group_by(Date) %>% 
  summarise(NumberDry = sum(DryRM == 0)/10) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(NumberDry = replace_na(NumberDry, 0), Year = year(Date)) %>% 
  relocate(Year, .before = Date) %>% 
  group_by(Year) %>% 
  mutate(AnnMean = mean(NumberDry), AnnVar = var(NumberDry)) %>% 
  pivot_longer(NumberDry:AnnVar, names_to = "TypeStat", values_to = "Statistic" ) %>% 
  filter(Date >= "2010-01-01")

#Daily dry figures ####

##### Time series with mean variance statistics 
#jpeg("Figures/DailyDistanceDry.jpeg", units="in", width=8, height=5, res=300)
# ggplot(DailyDry, aes(x = Date, y = Statistic, color = TypeStat))+
#   geom_line(size = .5)+
#   scale_color_manual(name = "", values = c("green", "blue", "red"), 
#                      labels = c("annual mean", "annual variance", "daily distance dry"))+
#   ylab("Number of dry river miles")+
#   xlab("")+
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
#   theme_classic()+
#   theme(legend.position="top")
#dev.off()
# 
# jpeg("Figures/DailyDistanceDryV2.jpeg", units="in", width=8, height=5, res=300)
# DailyDry %>% filter(TypeStat == "NumberDry") %>% 
#   ggplot(aes(x = Date, y = Statistic))+
#   geom_line(size = .5)+
#   ylab("Number of river miles")+
#   xlab("")+
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
#   theme_classic()+
#   theme(legend.position="top")
# dev.off()
# 
jpeg("Figures/DailyDistanceDryHisto.jpeg", units="in", width=8, height=5, res=300)
DailyDry %>% filter(TypeStat == "NumberDry") %>%
  ggplot(aes(x = log10(Statistic+0.1)))+
  geom_histogram(binwidth = .1)+
  xlab("log10(total number of dry river miles + 0.1)")+
  theme_classic()
dev.off()

temp <- DailyDry %>% filter(TypeStat == "NumberDry") %>% 
  mutate(Log10Dry = log10(Statistic + 0.1))
#Daily to tenth spatial visualize drying ####
jpeg("Figures/TimeRMdrying2010.jpeg", units="in", width=8, height=10, res=300)
ggplot(dat2, aes(x = Date, y = RMTenthDry, fill = as.factor(DryRM))) +
  geom_tile()+
  scale_fill_manual(name = "", values = c("#000000", "gray94"), labels = c("dry", "wet"))+
  xlab("") + ylab("River mile")+
  scale_y_continuous(breaks = seq(50, 170, by = 20))+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
  facet_zoom(x = Date >= as.Date("2012-05-01") & Date < as.Date("2012-11-01"),
             zoom.size = 1)+
  theme_classic()
dev.off()

#Change in daily dry ####
DailyDry2 <- dat %>% 
  mutate(RM = floor(RMTenthDry)) %>% 
  filter(DryRM == 0) %>% 
  group_by(Date) %>% 
  summarise(NumberDry = sum(DryRM == 0)/10) %>% 
  complete(Date = seq.Date(as.Date("2002-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(NumberDry = replace_na(NumberDry, 0), Year = year(Date),
         DiffDry = NumberDry - lag(NumberDry, default = NumberDry[1])) %>% 
  relocate(Year, .before = Date)

#Change daily dry plot
jpeg("Figures/ChangeDailyDistanceDry.jpeg", units="in", width=8, height=10, res=300)
ggplot(DailyDry2, aes(x = Date, y = DiffDry))+
  geom_line(size = 1) +
  xlab("") + ylab("Daily difference (# dry river miles)")+
  scale_y_continuous(breaks = seq(-70, 70, by = 10), limits = c(-70, 70))+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  facet_zoom(x = Date >= as.Date("2012-01-01") & Date <= as.Date("2012-12-31"),
             zoom.size = 1)+
  theme_classic()
dev.off()
