#read Me ####
#The purpose of this script is to assess the smaller scale spatial structure of 
#Drying Reach 1 and 2

#library ####
library(MARSS)
library(tidyverse)
library(lubridate)
library(zoo)
library(beepr)

#read predictor data ####
datR1 <- read.csv("Data/Processed/SmallScale/SmScl_R1MD_RespCov.csv", header = T) 
datR2 <- read.csv("Data/Processed/SmallScale/SmScl_R2MD_RespCov.csv", header = T) 
datR3<- read.csv("Data/Processed/SmallScale/SmScl_R3MD_RespCov.csv", header = T) 

#response distribution plots #####
datR3 %>% 
  group_by(Reach) %>% 
  mutate(zMileDays = scale(MileDays)) %>% 
  ggplot(aes(zMileDays))+
  geom_histogram()+
  facet_wrap(vars(Reach), scales = "free_x")

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
MileDays_DryR1 <- predictor_func(datR1, MileDays)
MileDays_DryR2 <- predictor_func(datR2, MileDays)
MileDays_DryR3 <- predictor_func(datR3, MileDays)

#creating a time series of first predictive variable
MileDays_DryR2pl <- ts(MileDays_DryR2, frequency = 365)
plot(MileDays_DryR2pl[1,])

#covariates ####
#already zscored and reduced to irrigation season (months 4-10)
cov_file_list <- list.files("Data/Processed/MARSS_Covariates/SmallScale", full.names = T, pattern = "*.csv")
all_cov_data <- lapply(cov_file_list, function(file){
  df <- read.csv(file)
})
names(all_cov_data) <- sub('\\.csv', '', basename(cov_file_list))
all_cov_data <- lapply(all_cov_data, function(x) column_to_rownames(x, var = "X"))
all_cov_matrix <- lapply(all_cov_data, function(x) as.matrix(x))

#check z-scoring
apply(all_cov_matrix$Pred_MD_R3_1state, 1, var)

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
Z_2astates <- matrix(0,3,2); Z_2astates[1,1] <- 1;Z_2astates[2,1] <- 1; Z_2astates[3,2] <- 1
Z_2bstates <- matrix(0,3,2); Z_2bstates[1,1] <- 1;Z_2bstates[2,2] <- 1; Z_2bstates[3,2] <- 1

#model lists R1, R2, and R3 ####

#3 states dry 
mod_3statesR1<- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                                 c=all_cov_matrix$Pred_MD_R1_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)
mod_3statesR2<- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                     c=all_cov_matrix$Pred_MD_R2_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                     R = "diagonal and equal", x0 = "equal", tinitx = 0)
mod_3statesR3<- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                     c=all_cov_matrix$Pred_MD_R3_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                     R = "diagonal and equal", x0 = "equal", tinitx = 0)
#2 states dry

mod_2astatesR1 <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                                 c=all_cov_matrix$Pred_MD_R1_2a_states, C=C_2states, Z = Z_2astates, A = matrix(0,3,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)
mod_2astatesR2 <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                      c=all_cov_matrix$Pred_MD_R2_2a_states, C=C_2states, Z = Z_2astates, A = matrix(0,3,1), 
                      R = "diagonal and equal", x0 = "equal", tinitx = 0)
mod_2astatesR3 <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                       c=all_cov_matrix$Pred_MD_R3_2a_states, C=C_2states, Z = Z_2astates, A = matrix(0,3,1), 
                       R = "diagonal and equal", x0 = "equal", tinitx = 0)

#2 states diversion 
mod_2bstatesR1<- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                                 c=all_cov_matrix$Pred_MD_R1_2b_states, C=C_2states, Z = Z_2bstates, A = matrix(0,3,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)
mod_2bstatesR2<- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                      c=all_cov_matrix$Pred_MD_R2_2b_states, C=C_2states, Z = Z_2bstates, A = matrix(0,3,1), 
                      R = "diagonal and equal", x0 = "equal", tinitx = 0)
mod_2bstatesR3<- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                      c=all_cov_matrix$Pred_MD_R3_2b_states, C=C_2states, Z = Z_2bstates, A = matrix(0,3,1), 
                      R = "diagonal and equal", x0 = "equal", tinitx = 0)
#1 state
mod_1stateR1 <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                             c=all_cov_matrix$Pred_MD_R1_1state, C=C_1state, Z = matrix(1,3,1), A = matrix(0,3,1), 
                             R = "diagonal and equal", x0 = "equal", tinitx = 0)
mod_1stateR2 <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                     c=all_cov_matrix$Pred_MD_R2_1state, C=C_1state, Z = matrix(1,3,1), A = matrix(0,3,1), 
                     R = "diagonal and equal", x0 = "equal", tinitx = 0)
mod_1stateR3 <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                     c=all_cov_matrix$Pred_MD_R3_1state, C=C_1state, Z = matrix(1,3,1), A = matrix(0,3,1), 
                     R = "diagonal and equal", x0 = "equal", tinitx = 0)
#model fits R1 ####

start.time <- Sys.time()
#3 states MileDays dry
MD_3states_R1 <- MARSS(MileDays_DryR1, model = mod_3statesR1, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
  MD_3states_R1_BFGS <- MARSS(y = MileDays_DryR1, model = mod_3statesR1, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_3states_R1$par) 

#2a states MileDays dry

MD_2astates_R1 <- MARSS(MileDays_DryR1, model = mod_2astatesR1, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
  MD_2astates_R1_BFGS <- MARSS(y = MileDays_DryR1, model = mod_2astatesR1, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_2astates_R1$par) 
#2b states MileDays dry
MD_2bstates_R1 <- MARSS(MileDays_DryR1, model = mod_2bstatesR1, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
  MD_2bstates_R1_BFGS <- MARSS(y = MileDays_DryR1, model = mod_2bstatesR1, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_2bstates_R1$par) 

#1 state Mile Days
MD_1state_R1 <- MARSS(MileDays_DryR1, model = mod_1stateR1, 
                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                  conv.test.slope.tol = 0.09), fit = T) 
  MD_1state_R1_BFGS <- MARSS(y = MileDays_DryR1, model = mod_1stateR1, control = list(maxit = 5000), 
                        method = "BFGS", inits = MD_1state_R1$par) 

beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))

#save outputs R1 ####
saveRDS(MD_3states_R1_BFGS, "ModelOutput/SmallScale/MD_DryingReach1/3stateMod_MD_R1BFGS.rds")
saveRDS(MD_2astates_R1_BFGS, "ModelOutput/SmallScale/MD_DryingReach1/2astateMod_MD_R1BFGS.rds")
saveRDS(MD_2bstates_R1_BFGS, "ModelOutput/SmallScale/MD_DryingReach1/2bstateMod_MD_R1BFGS.rds") 
saveRDS(MD_1state_R1_BFGS, "ModelOutput/SmallScale/MD_DryingReach1/1stateMod_MD_R1BFGS.rds") 

#read outputs R1 ####
MD_3states_R1 <- readRDS("ModelOutput/SmallScale/MD_DryingReach1/3stateMod_MD_R1BFGS.rds")
MD_2astates_R1 <- readRDS("ModelOutput/SmallScale/MD_DryingReach1/2astateMod_MD_R1BFGS.rds")
MD_2bstates_R1 <- readRDS("ModelOutput/SmallScale/MD_DryingReach1/2bstateMod_MD_R1BFGS.rds")
MD_1state_R1 <- readRDS("ModelOutput/SmallScale/MD_DryingReach1/1stateMod_MD_R1BFGS.rds")


#residuals R1 ####
#raw data better than log offset
autoplot.marssMLE(MD_3states_R1)  # all good
autoplot.marssMLE(MD_2astates_R1) # not good q/q horrible acf
autoplot.marssMLE(MD_2bstates_R1) # not good q/q horrible acf
autoplot.marssMLE(MD_1state_R1)   # not good q/q horrible acf

#summary R1 ####
MARSSparamCIs(MD_3states_R1)

#model fits R2 ####

start.time <- Sys.time()
#3 states MileDays dry
MD_3states_R2 <- MARSS(MileDays_DryR2, model = mod_3statesR2, 
                       control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                      conv.test.slope.tol = 0.09), fit = T) 
MD_3states_R2_BFGS <- MARSS(y = MileDays_DryR2, model = mod_3statesR2, control = list(maxit = 5000), 
                            method = "BFGS", inits = MD_3states_R2$par) 

#2a states MileDays dry

MD_2astates_R2 <- MARSS(MileDays_DryR2, model = mod_2astatesR2, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
MD_2astates_R2_BFGS <- MARSS(y = MileDays_DryR2, model = mod_2astatesR2, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_2astates_R2$par) 
#2b states MileDays dry
MD_2bstates_R2 <- MARSS(MileDays_DryR2, model = mod_2bstatesR2, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
MD_2bstates_R2_BFGS <- MARSS(y = MileDays_DryR2, model = mod_2bstatesR2, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_2bstates_R2$par) 

#1 state Mile Days
MD_1state_R2 <- MARSS(MileDays_DryR2, model = mod_1stateR2, 
                      control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                     conv.test.slope.tol = 0.09), fit = T) 
MD_1state_R2_BFGS <- MARSS(y = MileDays_DryR2, model = mod_1stateR2, control = list(maxit = 5000), 
                           method = "BFGS", inits = MD_1state_R2$par) 

beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))

#save outputs R2 ####
saveRDS(MD_3states_R2_BFGS, "ModelOutput/SmallScale/MD_DryingReach2/3stateMod_MD_R2BFGS.rds")  
saveRDS(MD_2astates_R2_BFGS, "ModelOutput/SmallScale/MD_DryingReach2/2astateMod_MD_R2BFGS.rds")  
saveRDS(MD_2bstates_R2_BFGS, "ModelOutput/SmallScale/MD_DryingReach2/2bstateMod_MD_R2BFGS.rds")  
saveRDS(MD_1state_R2_BFGS, "ModelOutput/SmallScale/MD_DryingReach2/1stateMod_MD_R2BFGS.rds") 

#read outputs R2 ####
MD_3states_R2 <- readRDS("ModelOutput/SmallScale/MD_DryingReach2/3stateMod_MD_R2BFGS.rds")
MD_2astates_R2 <- readRDS("ModelOutput/SmallScale/MD_DryingReach2/2astateMod_MD_R2BFGS.rds")
MD_2bstates_R2 <- readRDS("ModelOutput/SmallScale/MD_DryingReach2/2bstateMod_MD_R2BFGS.rds")
MD_1state_R2 <- readRDS("ModelOutput/SmallScale/MD_DryingReach2/1stateMod_MD_R2BFGS.rds")


#residuals R2 ####
#raw data better than log offset
autoplot.marssMLE(MD_3states_R2)  # good
autoplot.marssMLE(MD_2astates_R2) # q/q and acf no good
autoplot.marssMLE(MD_2bstates_R2) # q/q and acf no good
autoplot.marssMLE(MD_1state_R2)   # q/q and acf no good

#summary R2 ####
MARSSparamCIs(MD_3states_R2)

#model fits R3 ####

start.time <- Sys.time()
#3 states MileDays dry
MD_3states_R3 <- MARSS(MileDays_DryR3, model = mod_3statesR3, 
                       control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                      conv.test.slope.tol = 0.09), fit = T) 
MD_3states_R3_BFGS <- MARSS(y = MileDays_DryR3, model = mod_3statesR3, control = list(maxit = 5000), 
                            method = "BFGS", inits = MD_3states_R3$par) 

#2a states MileDays dry

MD_2astates_R3 <- MARSS(MileDays_DryR3, model = mod_2astatesR3, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
MD_2astates_R3_BFGS <- MARSS(y = MileDays_DryR3, model = mod_2astatesR3, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_2astates_R3$par) 
#2b states MileDays dry
MD_2bstates_R3 <- MARSS(MileDays_DryR3, model = mod_2bstatesR3, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
MD_2bstates_R3_BFGS <- MARSS(y = MileDays_DryR3, model = mod_2bstatesR3, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_2bstates_R3$par) 

#1 state Mile Days
MD_1state_R3 <- MARSS(MileDays_DryR3, model = mod_1stateR3, 
                      control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                     conv.test.slope.tol = 0.09), fit = T) 
MD_1state_R3_BFGS <- MARSS(y = MileDays_DryR3, model = mod_1stateR3, control = list(maxit = 5000), 
                           method = "BFGS", inits = MD_1state_R3$par) 

beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))

#save outputs R3 ####
saveRDS(MD_3states_R3_BFGS, "ModelOutput/SmallScale/MD_DryingReach3/3stateMod_MD_R3BFGS.rds")  
saveRDS(MD_2astates_R3_BFGS, "ModelOutput/SmallScale/MD_DryingReach3/2astateMod_MD_R3BFGS.rds")  
saveRDS(MD_2bstates_R3_BFGS, "ModelOutput/SmallScale/MD_DryingReach3/2bstateMod_MD_R3BFGS.rds")  
saveRDS(MD_1state_R3_BFGS, "ModelOutput/SmallScale/MD_DryingReach3/1stateMod_MD_R3BFGS.rds") 

#read outputs R3 ####
MD_3states_R3 <- readRDS("ModelOutput/SmallScale/MD_DryingReach3/3stateMod_MD_R3BFGS.rds")
MD_2astates_R3 <- readRDS("ModelOutput/SmallScale/MD_DryingReach3/2astateMod_MD_R3BFGS.rds")
MD_2bstates_R3 <- readRDS("ModelOutput/SmallScale/MD_DryingReach3/2bstateMod_MD_R3BFGS.rds")
MD_1state_R3 <- readRDS("ModelOutput/SmallScale/MD_DryingReach3/1stateMod_MD_R3BFGS.rds")


#residuals R3 ####
#raw data better than log offset
autoplot.marssMLE(MD_3states_R3)  # good
autoplot.marssMLE(MD_2astates_R3) # q/q and acf no good
autoplot.marssMLE(MD_2bstates_R3) # q/q not horrible and acf no good
autoplot.marssMLE(MD_1state_R3)   # q/q and acf no good

#summary R3 ####
MARSSparamCIs(MD_3states_R3)
