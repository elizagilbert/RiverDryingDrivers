#Read me ####

#The purpose of this script is to understand the groundwater
#levels in the study area.  The Isleta piezometer is the most downstream
#well location in the study area, but is still pretty high

#Libraries ####
library(tidyverse)
library(lubridate)
library(rstatix)

#Data ####
dat <- read.csv("Data/Raw/PiezometerIsleta_345650106415901.csv") %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

#Wrangle ####
temp <- dat %>% 
  # filter(between(month(Date), 4, 10)) %>% 
  mutate(Dif = Max - Min) %>% 
  rowid_to_column()

temp %>% 
  summarise(mean = mean(Dif, na.rm = T)) 
 
  
ggplot(temp, aes(x = Date, y = Dif))+
  geom_line()+
  scale_y_reverse()

ggplot(temp, aes(x = Date, y = Mean))+
  geom_line()+
  scale_y_reverse()

ggplot(temp, aes(x = rowid, y = Mean))+
  geom_point()+
  scale_y_reverse()+
  stat_smooth(method = "lm", se = F)+
  annotate("text",x=15,y=2010,label=(paste0("slope==",coef(lm(temp$Mean~temp$rowid))[2])),parse=TRUE)

ggplot(df,aes(x,y))+geom_point()+stat_smooth(method="lm",se=F)+
  annotate("text",x=-2,y=1,label=(paste0("slope==",coef(lm(df$y~df$x))[2])),parse=TRUE)

lm(temp$Mean ~ temp$rowid)
