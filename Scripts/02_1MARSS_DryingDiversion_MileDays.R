#read Me ####
#The purpose of this script is to assess the spatial structure of Rio Grande drying
#at two reach levels (drying and diversion)

#library ####
library(MARSS)
library(tidyverse)
library(lubridate)
library(zoo)
library(beepr)
library(gridExtra)

#read predictor data ####
dat_DryR <- read.csv("Data/Processed/DryingSubreachData.csv", header = T) 
dat_DivR <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T)

#response distribution plots #####
dat_DryR %>% 
  group_by(Reach) %>% 
  mutate(zMileDays = scale(MileDays)) %>% 
  ggplot(aes(zMileDays))+
  geom_histogram()+
  facet_wrap(vars(Reach), scales = "free_x")

dat_DivR %>% 
  group_by(Reach) %>% 
  mutate(zMileDays = scale(MileDays))%>% 
  ggplot(aes(zMileDays))+
  geom_histogram()+
  facet_wrap(vars(Reach))

#response MileDays ####

#reduce to irrigation season, zscore, pivot, and transform to matrix by reach function 
predictor_func <- function(data, predictor){
  result <- 
    as.matrix(data %>% 
                mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
                filter(between(month(Date), 4,10)) %>% 
                select(Date, {{predictor}}, Reach) %>% 
                group_by(Reach) %>% 
                mutate(zMileDays = ({{predictor}} - mean({{predictor}}, na.rm=TRUE)) / sd({{predictor}}, na.rm=TRUE)) %>% 
                select(!{{predictor}}) %>% 
                pivot_wider(names_from = Date, values_from = zMileDays) %>% 
                column_to_rownames(var = "Reach"))
  
  return(result)
}

#drying reaches
MileDays_DryR <- predictor_func(dat_DryR, MileDays)

#diversion reaches
MileDays_DivR <- predictor_func(dat_DivR, MileDays)


#creating a time series of first predictive variable
MileDays_DivR1 <- ts(MileDays_DivR, frequency = 365)
plot(MileDays_DivR1[1,])

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
apply(all_cov_matrix$PredCum_Div_1state, 1, var)


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

#3 states dry 
moddry_3statescum <- list(B = "diagonal and equal", U = matrix(0,3,1), Q = "diagonal and unequal",
                                 c=all_cov_matrix$PredCum_Dry_3statesReduced, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)

#2 states dry

moddry_2statescum <- list(B = "diagonal and equal", U = matrix(0,2,1), Q = "diagonal and unequal",
                                 c=all_cov_matrix$PredCum_Dry_2statesReduced, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)

#1 state dry
moddry_1statecum <- list(B = "diagonal and equal", U = matrix(0,1,1), Q = "diagonal and unequal",
                         c=all_cov_matrix$PredCum_Dry_1stateReduced, C=C_1state, Z = matrix(1,3,1), A = matrix(0,3,1), 
                         R = "diagonal and equal", x0 = "equal", tinitx = 0)

#2 states diversion 
moddiv_2statescum <- list(B = "diagonal and equal", U = matrix(0,2,1), Q = "diagonal and unequal",
                                 c=all_cov_matrix$PredCum_Div_2statesReduced, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)

#1 state diversion
moddiv_1statecum <- list(B = matrix(1), U = matrix(1), Q = "diagonal and equal",
                             c=all_cov_matrix$PredCum_Div_1state, C=C_1state, Z = matrix(1,2,1), A = matrix(0,2,1), 
                             R = "diagonal and equal", x0 = "equal", tinitx = 0)

#model fits ####

start.time <- Sys.time()
#3 states MileDays dry

MD_3states_dry <- MARSS(MileDays_DryR, model = moddry_3statescum, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
  MD_3states_dry_BFGS <- MARSS(y = MileDays_DryR, model = moddry_3statescum, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_3states_dry$par) 
  
#2 states MileDays dry

MD_2states_dry <- MARSS(MileDays_DryR, model = moddry_2statescum, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
  MD_2states_dry_BFGS <- MARSS(y = MileDays_DryR, model = moddry_2statescum, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_2states_dry$par) 

#2 states MileDays diversion
MD_2states_div <- MARSS(MileDays_DivR, model = moddiv_2statescum, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
  MD_2states_div_BFGS <- MARSS(y = MileDays_DivR, model = moddiv_2statescum, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_2states_div$par) 


#1 state MileDays dry
MD_1state_dry <- MARSS(MileDays_DryR, model = moddry_1statecum, 
                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                  conv.test.slope.tol = 0.09), fit = T) 
  MD_1state_dry_BFGS <- MARSS(y = MileDays_DryR, model = moddry_1statecum, control = list(maxit = 5000), 
                        method = "BFGS", inits = MD_1state_dry$par) 

#1 state MileDays dry
MD_1state_div <- MARSS(MileDays_DivR, model = moddiv_1statecum, 
                     control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                    conv.test.slope.tol = 0.09), fit = T) 
  MD_1state_div_BFGS <- MARSS(y = MileDays_DivR, model = moddiv_1statecum, control = list(maxit = 5000), 
                          method = "BFGS", inits = MD_1state_div$par) 
  
beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))

saveRDS(MD_3states_dry_BFGS, "ModelOutput/MileDays/MD_3states_dry_BFGS.rds") #
saveRDS(MD_2states_dry_BFGS, "ModelOutput/MileDays/MD_2states_dry_BFGS.rds") #
saveRDS(MD_2states_div_BFGS, "ModelOutput/MileDays/MD_2states_div_BFGS.rds") #
saveRDS(MD_1state_dry_BFGS, "ModelOutput/MileDays/MD_1state_dry_BFGS.rds") #
saveRDS(MD_1state_div_BFGS, "ModelOutput/MileDays/MD_1state_div_BFGS.rds") #

#read in model outputs ####
MD_3states_dry_BFGS <- readRDS("ModelOutput/MileDays/MD_3states_dry_BFGS.rds")
MD_2states_dry_BFGS <- readRDS("ModelOutput/MileDays/MD_2states_dry_BFGS.rds")
MD_2states_div_BFGS <- readRDS("ModelOutput/MileDays/MD_2states_div_BFGS.rds")
MD_1state_dry_BFGS <- readRDS("ModelOutput/MileDays/MD_1state_dry_BFGS.rds")
MD_1state_div_BFGS <- readRDS("ModelOutput/MileDays/MD_1state_div_BFGS.rds")

#residuals ####
#raw data better than log offset
autoplot.marssMLE(MD_3states_dry_BFGS) # good, small acf outliers in R2
autoplot.marssMLE(MD_2states_dry_BFGS) # no good, residuals not good, acf is bad
autoplot.marssMLE(MD_2states_div_BFGS) # good, small acf outliers in R1
autoplot.marssMLE(MD_1state_dry_BFGS)  # no good, residuals not good, acf is bad
autoplot.marssMLE(MD_1state_div_BFGS)  # no good, residuals not good, acf is bad

