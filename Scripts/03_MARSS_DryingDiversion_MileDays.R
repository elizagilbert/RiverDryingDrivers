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

#covariates ####
  #already zscored and reduced to irrigation season (months 4-10)
cov_file_list <- list.files("Data/Processed/MARSS_Covariates/Reduced", full.names = T, pattern = "*.csv")
all_cov_data <- lapply(cov_file_list, function(file){
  df <- read.csv(file)
})
names(all_cov_data) <- sub('\\.csv', '', basename(cov_file_list))
all_cov_data <- lapply(all_cov_data, function(x) column_to_rownames(x, var = "X"))
all_cov_matrix <- lapply(all_cov_data, function(x) as.matrix(x))

  #check z-scoring
apply(all_cov_matrix$Pred_Div_1state, 1, var)

#response Extent and MileDays ####

#BECAUSE THESE ARE COUNTS DO I HAVE TO NATURAL LOG AND DEAL WITH NEGATIVE IN EXTENT CHANGE?
#Can add absolute value of the most negative number to all change extent so the most negative number become 0 and shifts everything else up

#reduce to irrigation season, zscore, pivot, and transform to matrix by reach function 
predictor_func <- function(data, predictor){
  result <- 
    as.matrix(data %>% 
                mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
                filter(between(month(Date), 4,10)) %>% 
                select(Date, {{predictor}}, Reach) %>% 
                group_by(Reach) %>% 
                mutate(zExtent = ({{predictor}} - mean({{predictor}}, na.rm=TRUE)) / sd({{predictor}}, na.rm=TRUE)) %>% 
                select(!{{predictor}}) %>% 
                pivot_wider(names_from = Date, values_from = zExtent) %>% 
                column_to_rownames(var = "Reach"))
  
  return(result)
}

 #drying reaches
MileDays_DryR <- predictor_func(dat_DryR, MileDays)
  #diversion reaches
MileDays_DivR <- predictor_func(dat_DivR, MileDays)

#creating a time series of first predictive variable
MileDays_DivR <- ts(MileDays_DivR, frequency = 365)
plot(MileDays_DivR[1,])

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

#1 state
C_1state <- matrix(0,1,5)  
diag1 <- function(x) {
  C_1dis <- matrix(list(0),1,1); diag(C_1dis) <- "dis"
  C_1div <- matrix(list(0),1,1); diag(C_1div) <- "div"
  C_1precip <- matrix(list(0),1,1); diag(C_1precip) <- "precip"
  C_1ret <- matrix(list(0),1,1); diag(C_1ret) <- "ret"
  C_1temp <- matrix(list(0),1,1); diag(C_1temp) <- "temp"
  C_1state <- cbind(C_1dis, C_1div,C_1precip, C_1ret, C_1temp)
  C_1state
}
C_1state <- diag1(C_1state)

#Z for 2 drying states####
Z_2states <- matrix(0,3,2); Z_2states[1,1] <- 1;Z_2states[2,1] <- 1; Z_2states[3,2] <- 1

#model lists ####
#3 states

  #3 dry cumulative this is for daily dry variable
moddry_3statescum_qdiaeq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                              c=all_cov_matrix$PredCum_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_3statescum_qdiauneq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and unequal",
                                c=all_cov_matrix$PredCum_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_3statescum_qdiavarcov <- list(B = "identity", U = matrix(0,3,1), Q = "equalvarcov",
                                  c=all_cov_matrix$PredCum_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                                  R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #3 dry cumulative null this is for daily dry variable
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

  #2 dry cum this is for daily dry variable
moddry_2statescum_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$PredCum_Dry_2states, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_2statescum_qdiauneq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and unequal",
                                c=all_cov_matrix$PredCum_Dry_2states, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_2statescum_qdiavarcov <- list(B = "identity", U = matrix(0,2,1), Q = "equalvarcov",
                                  c=all_cov_matrix$PredCum_Dry_2states, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                                  R = "diagonal and equal", x0 = "equal", tinitx = 0)
  #2 dry cum null this is for daily dry variable
moddry_null_2statescum_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                                 Z = Z_2states, A = matrix(0,3,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_null_2statescum_qdiauneq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and unequal",
                                   Z = Z_2states, A = matrix(0,3,1), 
                                   R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_null_2statescum_qdiavarcov <- list(B = "identity", U = matrix(0,2,1), Q = "equalvarcov",
                                     Z = Z_2states, A = matrix(0,3,1), 
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
  #1 dry cum
moddry_1statecum_qdiaeq <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                             c=all_cov_matrix$PredCum_Dry_1state, C=C_1state, Z = matrix(1,3,1), A = matrix(0,3,1), 
                             R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #1 dry cum null
moddry_null_1statecum_qdiaeq <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                                Z = matrix(1,3,1), A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #1 diversion cum
moddiv_1statecum_qdiaeq <- list(B = matrix(1), U = matrix(1), Q = "diagonal and equal",
                            c=all_cov_matrix$PredCum_Div_1state, C=C_1state, Z = matrix(1,2,1), A = matrix(0,2,1), 
                            R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #1 diversion cum null
moddiv_null_1statecum_qdiaeq <- list(B = matrix(1), U = matrix(1), Q = "diagonal and equal",
                                Z = matrix(1,2,1), A = matrix(0,2,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)

#model fits Mile Days ####

#3 state Daily days
#beep("ping")
MD_3states_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_3statescum_qdiaeq)
MD_3states_dry_diauneq <- MARSS(MileDays_DryR, model = moddry_3statescum_qdiauneq)
MD_3states_dry_diavarcov <- MARSS(MileDays_DryR, model = moddry_3statescum_qdiavarcov)

MD_null_3states_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_null_3statescum_qdiaeq)
MD_null_3states_dry_diauneq <- MARSS(MileDays_DryR, model = moddry_null_3statescum_qdiauneq)
MD_null_3states_dry_diavarcov <- MARSS(MileDays_DryR, model = moddry_null_3statescum_qdiavarcov)
beep(3)

#2 state Daily days
MD_2states_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_2statescum_qdiaeq)
MD_2states_dry_diauneq <- MARSS(MileDays_DryR, model = moddry_2statescum_qdiauneq)
MD_2states_dry_diavarcov <- MARSS(MileDays_DryR, model = moddry_2statescum_qdiavarcov)

MD_null_2states_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_null_2statescum_qdiaeq)
MD_null_2states_dry_diauneq <- MARSS(MileDays_DryR, model = moddry_null_2statescum_qdiauneq)
MD_null_2states_dry_diavarcov <- MARSS(MileDays_DryR, model = moddry_null_2statescum_qdiavarcov)
beep(3)

MD_2states_div_qdiaeq <- MARSS(MileDays_DivR, model = moddiv_2statescum_qdiaeq)
MD_2states_div_diauneq <- MARSS(MileDays_DivR, model = moddiv_2statescum_qdiauneq)
MD_2states_div_diavarcov <- MARSS(MileDays_DivR, model = moddiv_2statescum_qdiavarcov)

MD_null_2states_div_qdiaeq <- MARSS(MileDays_DivR, model = moddiv_null_2statescum_qdiaeq)
MD_null_2states_div_diauneq <- MARSS(MileDays_DivR, model = moddiv_null_2statescum_qdiauneq)
MD_null_2states_div_diavarcov <- MARSS(MileDays_DivR, model = moddiv_null_2statescum_qdiavarcov)
beep(3)
#1 state Daily days
MD_1state_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_1statecum_qdiaeq)
MD_null_1state_dry_qdiaeq <- MARSS(MileDays_DryR, model = moddry_null_1statecum_qdiaeq)

MD_1state_div_qdiaeq <- MARSS(MileDays_DivR, model = moddiv_1statecum_qdiaeq)
MD_null_1state_div_qdiaeq <- MARSS(MileDays_DivR, model = moddiv_null_1statecum_qdiaeq)
beep(3)
#AIC ####
#Mile Days
MD_AIC <- c(MD_3states_dry_qdiaeq$AICc, MD_3states_dry_diauneq$AICc, MD_3states_dry_diavarcov$AICc,
                MD_null_3states_dry_qdiaeq$AICc, MD_null_3states_dry_diauneq$AICc, MD_null_3states_dry_diavarcov$AICc,
                MD_2states_dry_qdiaeq$AICc, MD_2states_dry_diauneq$AICc, MD_2states_dry_diavarcov$AICc,
                MD_null_2states_dry_qdiaeq$AICc, MD_null_2states_dry_diauneq$AICc, MD_null_2states_dry_diavarcov$AICc, 
                MD_1state_dry_diaeq$AICc,
                MD_null_1state_dry_diaeq$AICc,
                MD_2states_div_qdiaeq$AICc, MD_2states_div_diauneq$AICc, MD_2states_div_diavarcov$AICc,
                MD_null_2states_div_qdiaeq$AICc, MD_null_2states_div_diauneq$AICc, MD_null_2states_div_diavarcov$AICc,
                MD_1state_div_diaeq$AICc,
                MD_null_1state_div_diaeq$AICc)

ExtDelAIC <- MD_AIC - min(MD_AIC)
ExtRelLik <- exp(-0.5 * ExtDelAIC)
ExtAICWeight <- ExtRelLik/sum(ExtRelLik)
ExtAICTable <- data.frame(AICc = MD_AIC, delAIC = ExtDelAIC, relLike = ExtRelLik,
                          weight = ExtAICWeight)
rownames(ExtAICTable) <- c("3dry_diaeq", "3dry_diuneq", "3dry_varcov",
                           "3dry_null_diaeq", "3dry_null_diuneq", "3dry_null_varcov",
                           "2dry_diaeq", "2dry_diuneq", "2dry_varcov",
                           "2dry_null_diaeq", "2dry_null_diuneq", "2dry_null_varcov",
                           "1dry",
                           "1dry_null",
                           "2div_diaeq", "2div_diuneq", "2div_varcov",
                           "2div_null_diaeq", "2div_null_diuneq", "2div_null_varcov",
                           "1div",
                           "1div_null")
ExtAICTable %>% mutate(across(where(is.numeric),round,0)) %>% arrange(delAIC)

#save top model
# saveRDS(Extent_2states_div_diauneq, "ModelOutput/TopExtentMod_2Div_Uneq.rds")
# mod <- readRDS("ModelOutput/TopExtentMod_2Div_Uneq.rds")
# summary(Extent_2states_div_diauneq)

