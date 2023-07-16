#read Me ####
#The purpose of this script is to assess the spatial structure of Rio Grande drying
#at two reach levels (drying and diversion)

#library ####
library(MARSS)
library(tidyverse)
library(lubridate)
library(zoo)
library(beepr)

#read predictor data ####
dat_DryR <- read.csv("Data/Processed/DryingSubreachData.csv", header = T)
dat_DivR <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T)

#read covariates ####
#already zscored
cov_file_list <- list.files("Data/Processed/MARSS_Covariates/", full.names = T, pattern = "*.csv")
all_cov_data <- lapply(cov_file_list, function(file){
  df <- read.csv(file)
})
names(all_cov_data) <- sub('\\.csv', '', basename(cov_file_list))
all_cov_data <- lapply(all_cov_data, function(x) column_to_rownames(x, var = "X"))
all_cov_matrix <- lapply(all_cov_data, function(x) as.matrix(x))

#response transform and zscore ####

#BECAUSE THESE ARE COUNTS DO I NEED TO NATURAL LOG AND DEAL WITH NEGATIVE IN CHANGE?
#Can add absolute value of the most negative number to all change extent so
#the most negative number become 0 and shifts everything else up

#DO I NEED TO REPLACE ZEROS WITH NA AS DONE IN MARSS TUTORIAL

#zscore response variables#
 #drying reaches
Extent_DryR <- as.matrix(dat_DryR %>% 
  select(Date, Extent, Reach) %>% 
  group_by(Reach) %>% 
  mutate(zExtent = (Extent - mean(Extent))/ sd(Extent)) %>% 
  select(!Extent) %>% 
  pivot_wider(names_from = Date, values_from = zExtent) %>% 
  column_to_rownames(var = "Reach"))

ExtentChng_DryR <- as.matrix(dat_DryR %>% 
  select(Date, ExtentChng, Reach) %>% 
  group_by(Reach) %>% 
  mutate(zExtentChng = (ExtentChng - mean(ExtentChng))/ sd(ExtentChng)) %>% 
  select(!ExtentChng) %>% 
  pivot_wider(names_from = Date, values_from = zExtentChng) %>% 
  column_to_rownames(var = "Reach"))

MileDays_DryR <- as.matrix(dat_DryR %>% 
                             select(Date, MileDays, Reach) %>% 
                             group_by(Reach) %>% 
                             mutate(zMileDays = (MileDays - mean(MileDays))/ sd(MileDays)) %>% 
                             select(!MileDays) %>% 
                             pivot_wider(names_from = Date, values_from = zMileDays) %>% 
                             column_to_rownames(var = "Reach"))
  #diversion reaches
Extent_DivR <- as.matrix(dat_DivR %>% 
                           select(Date, Extent, Reach) %>% 
                           group_by(Reach) %>% 
                           mutate(zExtent = (Extent - mean(Extent))/ sd(Extent)) %>% 
                           select(!Extent) %>% 
                           pivot_wider(names_from = Date, values_from = zExtent) %>% 
                           column_to_rownames(var = "Reach"))

ExtentChng_DivR <- as.matrix(dat_DivR %>% 
                               select(Date, ExtentChng, Reach) %>% 
                               group_by(Reach) %>% 
                               mutate(zExtentChng = (ExtentChng - mean(ExtentChng))/ sd(ExtentChng)) %>% 
                               select(!ExtentChng) %>% 
                               pivot_wider(names_from = Date, values_from = zExtentChng) %>% 
                               column_to_rownames(var = "Reach"))

MileDays_DivR <- as.matrix(dat_DivR %>% 
                             select(Date, MileDays, Reach) %>% 
                             group_by(Reach) %>% 
                             mutate(zMileDays = (MileDays - mean(MileDays))/ sd(MileDays)) %>% 
                             select(!MileDays) %>% 
                             pivot_wider(names_from = Date, values_from = zMileDays) %>% 
                             column_to_rownames(var = "Reach"))

#check z-scoring
apply(all_cov_matrix$Pred_Div_1state, 1, var)

#creating a time series of first predictive variable
Extent_DryR_ts <- ts(Extent_DryR, frequency = 365)
plot(Extent_DryR_ts[1,])


#C matrices ####

#3 states
C_3states <- matrix(0,3,15)  
diag3 <- function(x) {
  C_3dis <- matrix(list(0),3,3); diag(C_3dis) <- "dis"
  C_3div <- matrix(list(0),3,3); diag(C_3div) <- "div"
  C_3precip <- matrix(list(0),3,3); diag(C_3precip) <- "precip"
  C_3ret <- matrix(list(0),3,3); diag(C_3ret) <- "ret"
  C_3temp <- matrix(list(0),3,3); diag(C_3temp) <- "temp"
  C_3states <- cbind(C_3dis, C_3div,C_3precip, C_3ret, C_3temp)
  C_3states
}
C_3states <- diag3(C_3states)
C_3states

#2 states
C_2states <- matrix(0,2,10)  
diag2 <- function(x) {
  C_2dis <- matrix(list(0),2,2); diag(C_2dis) <- "dis"
  C_2div <- matrix(list(0),2,2); diag(C_2div) <- "div"
  C_2precip <- matrix(list(0),2,2); diag(C_2precip) <- "precip"
  C_2ret <- matrix(list(0),2,2); diag(C_2ret) <- "ret"
  C_2temp <- matrix(list(0),2,2); diag(C_2temp) <- "temp"
  C_2states <- cbind(C_2dis, C_2div,C_2precip, C_2ret, C_2temp)
  C_2states
}
C_2states <- diag2(C_2states)
C_2states

#1 state
C_1state <- matrix(0,1,5)  
diag2 <- function(x) {
  C_1dis <- matrix(list(0),1,1); diag(C_1dis) <- "dis"
  C_1div <- matrix(list(0),1,1); diag(C_1div) <- "div"
  C_1precip <- matrix(list(0),1,1); diag(C_1precip) <- "precip"
  C_1ret <- matrix(list(0),1,1); diag(C_1ret) <- "ret"
  C_1temp <- matrix(list(0),1,1); diag(C_1temp) <- "temp"
  C_1state <- cbind(C_1dis, C_1div,C_1precip, C_1ret, C_1temp)
  C_1state
}
C_1state <- diag2(C_1state)
C_1state

#Z for 2 drying states####
Z_2states <- matrix(0,3,2); Z_2states[1,1] <- 1;Z_2states[2,1] <- 1; Z_2states[3,2] <- 1
Z_2states

#model lists ####
#3 states
  #3 dry
moddry_3states_qdiaeq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                        c=all_cov_matrix$Pred_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                        R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_3states_qdiauneq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and unequal",
                           c=all_cov_matrix$Pred_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                           R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_3states_qdiavarcov <- list(B = "identity", U = matrix(0,3,1), Q = "equalvarcov",
                           c=all_cov_matrix$Pred_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                           R = "diagonal and equal", x0 = "equal", tinitx = 0)
  #3 dry null
moddry_null_3states_qdiaeq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                              Z = "identity", A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_null_3states_qdiauneq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and unequal",
                                Z = "identity", A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_null_3states_qdiavarcov <- list(B = "identity", U = matrix(0,3,1), Q = "equalvarcov",
                                  Z = "identity", A = matrix(0,3,1), 
                                  R = "diagonal and equal", x0 = "equal", tinitx = 0)
  #3 dry cumulative
moddry_3statescum_qdiaeq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                              c=all_cov_matrix$PredCum_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_3statescum_qdiauneq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and unequal",
                                c=all_cov_matrix$PredCum_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_3statescum_qdiavarcov <- list(B = "identity", U = matrix(0,3,1), Q = "equalvarcov",
                                  c=all_cov_matrix$PredCum_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                                  R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #3 dry cumulative null
moddry_null_3statescum_qdiaeq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                                 Z = "identity", A = matrix(0,3,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_null_3statescum_qdiauneq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and unequal",
                                    Z = "identity", A = matrix(0,3,1), 
                                   R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_null_3statescum_qdiavarcov <- list(B = "identity", U = matrix(0,3,1), Q = "equalvarcov",
                                     Z = "identity", A = matrix(0,3,1), 
                                     R = "diagonal and equal", x0 = "equal", tinitx = 0)
#2 states
  #2 dry
moddry_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$Pred_Dry_2states, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_2states_qdiauneq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and unequal",
                                c=all_cov_matrix$Pred_Dry_2states, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_2states_qdiavarcov <- list(B = "identity", U = matrix(0,2,1), Q = "equalvarcov",
                                  c=all_cov_matrix$Pred_Dry_2states, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                                  R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 dry null
moddry_null_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              Z = Z_2states, A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_null_2states_qdiauneq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and unequal",
                                Z = Z_2states, A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_null_2states_qdiavarcov <- list(B = "identity", U = matrix(0,2,1), Q = "equalvarcov",
                                  Z = Z_2states, A = matrix(0,3,1), 
                                  R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 dry cum
moddry_2statescum_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$PredCum_Dry_2states, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_2statescum_qdiauneq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and unequal",
                                c=all_cov_matrix$PredCum_Dry_2states, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_2statescum_qdiavarcov <- list(B = "identity", U = matrix(0,2,1), Q = "equalvarcov",
                                  c=all_cov_matrix$PredCum_Dry_2states, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                                  R = "diagonal and equal", x0 = "equal", tinitx = 0)
  #2 dry cum null
moddry_null_2statescum_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                                 Z = Z_2states, A = matrix(0,3,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_null_2statescum_qdiauneq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and unequal",
                                   Z = Z_2states, A = matrix(0,3,1), 
                                   R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_null_2statescum_qdiavarcov <- list(B = "identity", U = matrix(0,2,1), Q = "equalvarcov",
                                     Z = Z_2states, A = matrix(0,3,1), 
                                     R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 diversion
moddiv_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$Pred_Div_2states, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddiv_2states_qdiauneq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and unequal",
                                c=all_cov_matrix$Pred_Div_2states, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddiv_2states_qdiavarcov <- list(B = "identity", U = matrix(0,2,1), Q = "equalvarcov",
                                  c=all_cov_matrix$Pred_Div_2states, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                                  R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 diversion null
moddiv_null_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              Z = "identity", A = matrix(0,2,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddiv_null_2states_qdiauneq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and unequal",
                                Z = "identity", A = matrix(0,2,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddiv_null_2states_qdiavarcov <- list(B = "identity", U = matrix(0,2,1), Q = "equalvarcov",
                                  Z = "identity", A = matrix(0,2,1), 
                                  R = "diagonal and equal", x0 = "equal", tinitx = 0)

   #2 diversion cum
moddiv_2statescum_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$PredCum_Div_2states, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddiv_2statescum_qdiauneq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and unequal",
                                c=all_cov_matrix$PredCum_Div_2states, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddiv_2statescum_qdiavarcov <- list(B = "identity", U = matrix(0,2,1), Q = "equalvarcov",
                                  c=all_cov_matrix$PredCum_Div_2states, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                                  R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 diversion cum null
moddiv_null_2statescum_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                                 Z = "identity", A = matrix(0,2,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddiv_null_2statescum_qdiauneq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and unequal",
                                   Z = "identity", A = matrix(0,2,1), 
                                   R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddiv_null_2statescum_qdiavarcov <- list(B = "identity", U = matrix(0,2,1), Q = "equalvarcov",
                                     Z = "identity", A = matrix(0,2,1), 
                                     R = "diagonal and equal", x0 = "equal", tinitx = 0)

#1 state
  #1 dry
moddry_1state_qdiaeq <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                    c=all_cov_matrix$Pred_Dry_1state, C=C_1state, Z = matrix(1,3,1), A = matrix(0,3,1), 
                    R = "diagonal and equal", x0 = "equal", tinitx = 0)
  #1 dry null
moddry_null_1state_qdiaeq <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                             Z = matrix(1,3,1), A = matrix(0,3,1), 
                             R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #1 dry cum
moddry_1statecum_qdiaeq <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                             c=all_cov_matrix$PredCum_Dry_1state, C=C_1state, Z = matrix(1,3,1), A = matrix(0,3,1), 
                             R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #1 dry cum null
moddry_null_1statecum_qdiaeq <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                                Z = matrix(1,3,1), A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #1 diversion
moddiv_1state_qdiaeq <- list(B = matrix(1), U = matrix(1), Q = "diagonal and equal",
                      c=all_cov_matrix$Pred_Div_1state, C=C_1state, Z = matrix(1,2,1), A = matrix(0,2,1), 
                      R = "diagonal and equal", x0 = "equal", tinitx = 0)
  
  #1 diversion null
moddiv_null_1state_qdiaeq <- list(B = matrix(1), U = matrix(1), Q = "diagonal and equal",
                             Z = matrix(1,2,1), A = matrix(0,2,1), 
                             R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #1 diversion cum
moddiv_1statecum_qdiaeq <- list(B = matrix(1), U = matrix(1), Q = "diagonal and equal",
                            c=all_cov_matrix$PredCum_Div_1state, C=C_1state, Z = matrix(1,2,1), A = matrix(0,2,1), 
                            R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #1 diversion cum null
moddiv_null_1statecum_qdiaeq <- list(B = matrix(1), U = matrix(1), Q = "diagonal and equal",
                                Z = matrix(1,2,1), A = matrix(0,2,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
#model fits Extent Dry####

#3 states Extent
Extent_3states_dry_qdiaeq <- MARSS(Extent_DryR, model = moddry_3states_qdiaeq)
Extent_3states_dry_diauneq <- MARSS(Extent_DryR, model = moddry_3states_qdiauneq)
Extent_3states_dry_diavarcov <- MARSS(Extent_DryR, model = moddry_3states_qdiavarcov)

Extent_null_3states_dry_qdiaeq <- MARSS(Extent_DryR, model = moddry_null_3states_qdiaeq)
Extent_null_3states_dry_diauneq <- MARSS(Extent_DryR, model = moddry_null_3states_qdiauneq)
Extent_null_3states_dry_diavarcov <- MARSS(Extent_DryR, model = moddry_null_3states_qdiavarcov)

#2 states Extent
Extent_2states_dry_qdiaeq <- MARSS(Extent_DryR, model = moddry_2states_qdiaeq)
Extent_2states_dry_diauneq <- MARSS(Extent_DryR, model = moddry_2states_qdiauneq)
Extent_2states_dry_diavarcov <- MARSS(Extent_DryR, model = moddry_2states_qdiavarcov)

Extent_null_2states_dry_qdiaeq <- MARSS(Extent_DryR, model = moddry_null_2states_qdiaeq)
Extent_null_2states_dry_diauneq <- MARSS(Extent_DryR, model = moddry_null_2states_qdiauneq)
Extent_null_2states_dry_diavarcov <- MARSS(Extent_DryR, model = moddry_null_2states_qdiavarcov)

Extent_2states_div_qdiaeq <- MARSS(Extent_DivR, model = moddiv_2states_qdiaeq)
Extent_2states_div_diauneq <- MARSS(Extent_DivR, model = moddiv_2states_qdiauneq)
Extent_2states_div_diavarcov <- MARSS(Extent_DivR, model = moddiv_2states_qdiavarcov)

Extent_null_2states_div_qdiaeq <- MARSS(Extent_DivR, model = moddiv_null_2states_qdiaeq)
Extent_null_2states_div_diauneq <- MARSS(Extent_DivR, model = moddiv_null_2states_qdiauneq)
Extent_null_2states_div_diavarcov <- MARSS(Extent_DivR, model = moddiv_null_2states_qdiavarcov)

#1 states Extent
Extent_1state_dry_diaeq <- MARSS(Extent_DryR, model = moddry_1state_qdiaeq)
Extent_null_1state_dry_diaeq <- MARSS(Extent_DryR, model = moddry_null_1state_qdiaeq)

Extent_1state_div_diaeq <- MARSS(Extent_DivR, model = moddiv_1state_qdiaeq)
Extent_null_1state_div_diaeq <- MARSS(Extent_DivR, model = moddiv_null_1state_qdiaeq)

#model fits Extent Change####

#3 state Extent change
ExtentChng_3states_dry_qdiaeq <- MARSS(ExtentChng_DryR, model = moddry_3states_qdiaeq)
ExtentChng_3states_dry_diauneq <- MARSS(ExtentChng_DryR, model = moddry_3states_qdiauneq)
ExtentChng_3states_dry_diavarcov <- MARSS(ExtentChng_DryR, model = moddry_3states_qdiavarcov)

#2 state Extent change
ExtentChng_2states_dry_qdiaeq <- MARSS(ExtentChng_DryR, model = moddry_2states_qdiaeq)
ExtentChng_2states_dry_diauneq <- MARSS(ExtentChng_DryR, model = moddry_2states_qdiauneq)
ExtentChng_2states_dry_diavarcov <- MARSS(ExtentChng_DryR, model = moddry_2states_qdiavarcov)

ExtentChng_2states_div_qdiaeq <- MARSS(ExtentChng_DivR, model = moddiv_2states_qdiaeq)
ExtentChng_2states_div_diauneq <- MARSS(ExtentChng_DivR, model = moddiv_2states_qdiauneq)
ExtentChng_2states_div_diavarcov <- MARSS(ExtentChng_DivR, model = moddiv_2states_qdiavarcov)

#1 state Extent change
ExtentChng_1state_dry_qdiaeq <- MARSS(ExtentChng_DryR, model = moddry_1state_qdiaeq)
ExtentChng_null_1state_dry_diaeq <- MARSS(ExtentChng_DryR, model = moddry_null_1state_qdiaeq)

ExtentChng_1state_div_qdiaeq <- MARSS(ExtentChng_DivR, model = moddiv_1state_qdiaeq)
ExtentChng_null_1state_div_diaeq <- MARSS(ExtentChng_DivR, model = moddiv_null_1state_qdiaeq)

#model fits Daily Dry ####

#3 state Daily days
#beep("ping")
MD_3states_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_3statescum_qdiaeq)
MD_3states_dry_diauneq <- MARSS(MileDays_DryR, model = moddry_3statescum_qdiauneq)
MD_3states_dry_diavarcov <- MARSS(MileDays_DryR, model = moddry_3statescum_qdiavarcov)

MD_null_3states_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_null_3statescum_qdiaeq)
MD_null_3states_dry_diauneq <- MARSS(MileDays_DryR, model = moddry_null_3statescum_qdiauneq)
MD_null_3states_dry_diavarcov <- MARSS(MileDays_DryR, model = moddry_null_3statescum_qdiavarcov)

#2 state Daily days
MD_2states_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_2statescum_qdiaeq)
MD_2states_dry_diauneq <- MARSS(MileDays_DryR, model = moddry_2statescum_qdiauneq)
MD_2states_dry_diavarcov <- MARSS(MileDays_DryR, model = moddry_2statescum_qdiavarcov)

MD_null_2states_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_null_2statescum_qdiaeq)
MD_null_2states_dry_diauneq <- MARSS(MileDays_DryR, model = moddry_null_2statescum_qdiauneq)
MD_null_2states_dry_diavarcov <- MARSS(MileDays_DryR, model = moddry_null_2statescum_qdiavarcov)

MD_2states_div_qdiaeq <- MARSS(MileDays_DivR, model = moddiv_2statescum_qdiaeq)
MD_2states_div_diauneq <- MARSS(MileDays_DivR, model = moddiv_2statescum_qdiauneq)
MD_2states_div_diavarcov <- MARSS(MileDays_DivR, model = moddiv_2statescum_qdiavarcov)

MD_null_2states_div_qdiaeq <- MARSS(MileDays_DivR, model = moddiv_null_2statescum_qdiaeq)
MD_null_2states_div_diauneq <- MARSS(MileDays_DivR, model = moddiv_null_2statescum_qdiauneq)
MD_null_2states_div_diavarcov <- MARSS(MileDays_DivR, model = moddiv_null_2statescum_qdiavarcov)

#1 state Daily days
MD_1state_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_1statecum_qdiaeq)
MD_null_1state_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_null_1statecum_qdiaeq)

MD_1state_div_qdiaeq <- MARSS(MileDays_DivR, model = moddiv_1statecum_qdiaeq)
MD_null_1state_div_qdiaeq <- MARSS(MileDays_DivR, model = moddiv_null_1statecum_qdiaeq)

#AIC ####
#Extent
Extent_AIC <- c(Extent_3states_dry_qdiaeq$AICc, Extent_3states_dry_diauneq$AICc, Extent_3states_dry_diavarcov$AICc,
                 Extent_2states_dry_qdiaeq$AICc, Extent_2states_dry_diauneq$AICc, Extent_2states_dry_diavarcov$AICc,
                 Extent_1state_dry_diaeq$AICc,
                 Extent_2states_div_qdiaeq$AICc, Extent_2states_div_diauneq$AICc, Extent_2states_div_diavarcov$AICc,
                 Extent_1state_div_diaeq$AICc)

ExtDelAIC <- Extent_AIC - min(Extent_AIC)
ExtRelLik <- exp(-0.5 * ExtDelAIC)
ExtAICWeight <- ExtRelLik/sum(ExtRelLik)
ExtAICTable <- data.frame(AICc = Extent_AIC, delAIC = ExtDelAIC, relLike = ExtRelLik,
                          weight = ExtAICWeight)
rownames(ExtAICTable) <- c("3dry_diaeq", "3dry_diuneq", "3dry_varcov",
                           "2dry_diaeq", "2dry_diuneq", "2dry_varcov",
                           "1dry",
                           "2div_diaeq", "2div_diuneq", "2div_varcov",
                           "1div")
ExtAICTable %>% mutate(across(where(is.numeric),round,0)) %>% arrange(delAIC)

#ExtentChng
ExtentChng_fits <- c(ExtentChng_3states_dry_qdiaeq$AICc, ExtentChng_3states_dry_diauneq$AICc, ExtentChng_3states_dry_diavarcov$AICc,
                 ExtentChng_2states_dry_qdiaeq$AICc, ExtentChng_2states_dry_diauneq$AICc, ExtentChng_2states_dry_diavarcov$AICc,
                 ExtentChng_1state_dry_diaeq$AICc,
                 ExtentChng_2states_div_qdiaeq$AICc, ExtentChng_2states_div_diauneq$AICc, ExtentChng_2states_div_diavarcov$AICc,
                 ExtentChng_1state_div_diaeq$AICc)

#DaysDry
MD_fits <- c(MD_2states_dry_qdiaeq$AICc, MD_2states_dry_diauneq$AICc, MD_2states_dry_diavarcov$AICc,
             MD_1state_dry_diaeq$AICc,
             MD_2states_div_qdiaeq$AICc, MD_2states_div_diauneq$AICc, MD_2states_div_diavarcov$AICc,
             MD_1state_div_diaeq$AICc)