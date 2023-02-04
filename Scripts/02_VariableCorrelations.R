#Read me###
#The purpose of this script is to look at the multicollinearlity and
#correlations between the river drying data and covariates

#library ####
library(tidyverse)
library(Hmisc)
library(PerformanceAnalytics)
library(gridExtra)

#data####
dat <- read.csv("Data/Processed/Reach1_ResponseAndCovariates.csv")
datReach2 <- read.csv("Data/Processed/Reach2_ResponseAndCovariates.csv")

#correlation Reach 1 ####
temp <- dat %>% mutate(RiverReach1_Date = as.Date(RiverReach1_Date, format = "%Y-%m-%d")) %>% 
  filter(RiverReach1_Date >= "2010-01-01") %>% 
  select(Precip_LosLunas_mm, TempMax_LosLunas_C,
         RiverDischarge_BosqueFarms_cfs,
         Diversion_Isleta_Totalcfs, Returns_Totalcfs) %>% 
  rename_with(~gsub('[[:digit:]]+_', "", .))
  
Corr_all <- cor(temp, use = "complete.obs")
Corr_pvalue <- rcorr(as.matrix(temp))
Corr_pvalue$r
Corr_pvalue$P

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

Corr_Df <- as.data.frame(flattenCorrMatrix(Corr_pvalue$r, Corr_pvalue$P)) %>% 
  filter(cor > abs(0.7))

#number of NAs Reach 1####
dat_Reach1 <- dat %>%
  mutate(RiverReach1_Date = as.Date(RiverReach1_Date, format = "%Y-%m-%d")) %>%
  filter(RiverReach1_Date >= "2010-01-01")

sapply(dat_Reach1, function(x) sum(is.na(x)))
longestNAstrech <- function(x) {
  with(rle(is.na(x)), max(lengths[values]))  
}

longestNAstrech(dat_Reach1$Precip_LosLunas_mm)
longestNAstrech(dat_Reach1$TempMax_LosLunas_C)

#correlation Reach 2 ####
temp2 <- datReach2 %>% mutate(RiverReach2_Date = as.Date(RiverReach2_Date, format = "%Y-%m-%d")) %>% 
  filter(RiverReach2_Date >= "2010-01-01") %>% 
  select(ExtentDry, DiffExtentDry, SumDaysDry, UpperLocationDry,
         Precip_Bernardo_mm, TempMax_Bernardo_C,
         Discharge_cfs,
         Diversion_Isleta_Totalcfs, Returns_Totalcfs) %>% 
  rename_with(~gsub('[[:digit:]]+_', "", .))

Corr2_all <- cor(temp2)
Corr2_pvalue <- rcorr(as.matrix(temp))
Corr2_pvalue$r
Corr2_pvalue$P

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

Corr_Df2 <- as.data.frame(flattenCorrMatrix(Corr2_pvalue$r, Corr2_pvalue$P)) %>% 
  filter(cor > abs(0.7))

#number of NAs Reach 2####
dat_Reach2 <- datReach2 %>%
  mutate(RiverReach2_Date = as.Date(RiverReach2_Date, format = "%Y-%m-%d")) %>%
  filter(RiverReach2_Date >= "2010-01-01")

sapply(dat_Reach2, function(x) sum(is.na(x)))
longestNAstrech <- function(x) {
  with(rle(is.na(x)), max(lengths[values]))  
}

longestNAstrech(dat_Reach2$Precip_Bernardo_mm)
longestNAstrech(dat_Reach2$TempMax_Bernardo_C)
sum(is.na(dat_Reach2$UpperLocationDry))

# Other stuff ####
#looking at 80 (Bosque) and 300 (San Acacia)
# jpeg("Figures/DailyDistanceDry_CorrMatrix_Bosque.jpeg", units="in", width=8, height=5, res=300)
# Bosque_chrt <- temp %>% 
#   select("NumberDry", "BosqueFarms") %>% 
#   filter(BosqueFarms <= 80) %>% 
#   chart.Correlation(histogram = T, pch = 19)
# dev.off()
# 
# jpeg("Figures/DailyDistanceDry_CorrMatrix_SanAcacia.jpeg", units="in", width=8, height=5, res=300)
# SanAca_chrt <- temp %>% 
#   rename(SanAcacia = 5) %>% 
#   select(NumberDry, SanAcacia) %>% 
#   filter(SanAcacia <= 300) %>% 
#   chart.Correlation(histogram = T, pch = 19)
# dev.off()

#write model - Alex said I should use correlation instead of VIF
# 
# mod1 <- lm(scale(ExtentDry) ~ scale(RiverDischarge_BosqueFarms_cfs) + scale(Diversion_Isleta_Totalcfs) + 
#              scale(Returns_Totalcfs), data = dat_Reach1)
# mod2 <- lm(scale(ExtentDry) ~ scale(SumDaysDry) + scale(UpperLocationDry), data = dat_Reach1)
# mod3 <- lm(scale(SumDaysDry) ~ scale(ExtentDry) + scale(UpperLocationDry), data = dat_Reach1)
# mod4 <- lm(scale(UpperLocationDry) ~ scale(ExtentDry) + scale(SumDaysDry), data = dat_Reach1)
# mod5 <- lm(scale(ExtentDry) ~ scale(RiverDischarge_BosqueFarms_cfs) + scale(Diversion_Isleta_Totalcfs) +
#              scale(Returns_Totalcfs) + scale(SumDaysDry) + + scale(UpperLocationDry), data = dat_Reach1)
# mod6 <- lm(scale(SumDaysDry) ~ scale(RiverDischarge_BosqueFarms_cfs) + scale(Diversion_Isleta_Totalcfs) +
#              scale(Returns_Totalcfs) + scale(ExtentDry) + + scale(UpperLocationDry), data = dat_Reach1)
# mod7 <- lm(scale(UpperLocationDry) ~ scale(RiverDischarge_BosqueFarms_cfs) + scale(Diversion_Isleta_Totalcfs) +
#              scale(Returns_Totalcfs) + scale(SumDaysDry) + + scale(ExtentDry), data = dat_Reach1)
# mod8 <- lm(scale(SumDaysDry) ~ scale(DiffExtentDry) + scale(UpperLocationDry), data = dat_Reach1)
# 
# #variable inflation factors are good at ~ 1 
# t1 <- rownames_to_column(as.data.frame(car::vif(mod1))) #good
# t2 <- rownames_to_column(as.data.frame(car::vif(mod2))) #good
# t3 <- rownames_to_column(as.data.frame(car::vif(mod3))) #not good
# t4 <- rownames_to_column(as.data.frame(car::vif(mod4))) #good
# t5 <- rownames_to_column(as.data.frame(car::vif(mod5))) #good
# t6 <- rownames_to_column(as.data.frame(car::vif(mod6))) #not good on response
# t7 <- rownames_to_column(as.data.frame(car::vif(mod7))) #good
# t8 <- rownames_to_column(as.data.frame(car::vif(mod8))) #good


