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
#testing model output of log transformations with and without 0s as NAs
dat_DryR <- read.csv("Data/Processed/DryingSubreachData.csv", header = T) %>% 
  mutate(NAExtent = Extent) %>% 
  mutate_at(c('NAExtent'), ~na_if(., 0)) %>% 
  mutate(LogExtent = log(Extent+0.0001),
         LogNAExtent = log(NAExtent))
dat_DivR <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T) %>% 
  mutate(NAExtent = Extent) %>% 
  mutate_at(c('NAExtent'), ~na_if(., 0)) %>% 
  mutate(LogExtent = log(Extent+0.0001),
         LogNAExtent = log(NAExtent))

#Response distribution plots #####
dat_DryR %>% 
  select(Date, Extent, Reach) %>% 
  group_by(Reach) %>% 
  mutate(zLogExtent = (LogExtent - mean(LogExtent, na.rm = TRUE))/ sd(LogExtent, na.rm = TRUE)) %>% 
  mutate(zExtent = (Extent - mean(Extent, na.rm = TRUE))/ sd(Extent, na.rm = TRUE)) %>% 
  ggplot(aes(zExtent))+
  geom_histogram()+
  facet_wrap(vars(Reach), scales = "free_x")

dat_DivR %>% 
  select(Date, Extent, Reach) %>% 
  group_by(Reach) %>% 
  mutate(LogExtent = log(Extent)) %>% 
  mutate(zLogExtent = (LogExtent - mean(LogExtent, na.rm = TRUE))/ sd(LogExtent, na.rm = TRUE)) %>% 
  mutate(zExtent = (Extent - mean(Extent, na.rm = TRUE))/ sd(Extent, na.rm = TRUE)) %>% 
  ggplot(aes(zExtent))+
  geom_histogram()+
  facet_wrap(vars(Reach))

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

#response Extent ####

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
Extent_DryR1 <- predictor_func(dat_DryR, Extent)
Extent_DryR2 <- predictor_func(dat_DryR, NAExtent)
Extent_DryR3 <- predictor_func(dat_DryR, LogExtent)
Extent_DryR4 <- predictor_func(dat_DryR, LogNAExtent)

  #diversion reaches
Extent_DivR1 <- predictor_func(dat_DivR, Extent)
Extent_DivR2 <- predictor_func(dat_DivR, NAExtent)
Extent_DivR3 <- predictor_func(dat_DivR, LogExtent)
Extent_DivR4 <- predictor_func(dat_DivR, LogNAExtent)

#creating a time series of first predictive variable
Extent_DryR_ts <- ts(Extent_DryR4, frequency = 365)
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
  #3 dry
moddry_3states_qdiaeq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                        c=all_cov_matrix$Pred_Dry_3statesReduced, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                        R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #3 dry null
moddry_null_3states_qdiaeq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                              Z = "identity", A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

#2 states
  #2 dry
moddry_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$Pred_Dry_2statesReduced, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 dry null
moddry_null_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              Z = Z_2states, A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 diversion
moddiv_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$Pred_Div_2statesReduced, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 diversion null
moddiv_null_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              Z = "identity", A = matrix(0,2,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

#1 state
  #1 covariates
mod_1state_qdiaeq <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                    c=all_cov_matrix$Pred_Dry_1stateReduced, C=C_1state, Z = matrix(1,3,1), A = matrix(0,3,1), 
                    R = "diagonal and equal", x0 = "equal", tinitx = 0)
  #1 null
mod_null_1state_qdiaeq <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                             Z = matrix(1,3,1), A = matrix(0,3,1), 
                             R = "diagonal and equal", x0 = "equal", tinitx = 0)


#model fits ####
#replace R.. with R1 = raw NA; R2 = 0s to NA; R3 = log(raw+0.001); R4 0s to NA and log(raw)

#3 states Extent
 
start.time <- Sys.time()
Extent_3states_dry <- MARSS(y = Extent_DryR4, model = moddry_3states_qdiaeq, 
                                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                  conv.test.slope.tol = 0.09), fit = T) 
Extent_3states_dry_BFGS <- MARSS(y = Extent_DryR4, model = moddry_3states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = Extent_3states_dry$par)

Extent_null_3states_dry <- MARSS(Extent_DryR4, model = moddry_null_3states_qdiaeq, 
                                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                       conv.test.slope.tol = 0.09), fit = T) 
Extent_null_3states_dry_BFGS <- MARSS(y = Extent_DryR4, model = moddry_null_3states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = Extent_null_3states_dry$par)

#2 states Extent
Extent_2states_dry <- MARSS(Extent_DryR4, model = moddry_2states_qdiaeq, 
                                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                  conv.test.slope.tol = 0.09),fit = T) 
Extent_2states_dry_BFGS <- MARSS(y = Extent_DryR4, model = moddry_2states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = Extent_2states_dry$par)

Extent_null_2states_dry <- MARSS(Extent_DryR4, model = moddry_null_2states_qdiaeq, 
                                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                       conv.test.slope.tol = 0.09),fit = T) 
Extent_null_2states_dry_BFGS <- MARSS(y = Extent_DryR4, model = moddry_null_2states_qdiaeq, control = list(maxit = 5000), 
                                      method = "BFGS", inits = Extent_null_2states_dry$par)

Extent_2states_div <- MARSS(Extent_DivR4, model = moddiv_2states_qdiaeq, 
                                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                  conv.test.slope.tol = 0.09),fit = T) 
Extent_2states_div_BFGS <- MARSS(y = Extent_DivR4, model = moddiv_2states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = Extent_2states_div$par)

Extent_null_2states_div <- MARSS(Extent_DivR4, model = moddiv_null_2states_qdiaeq, 
                                 control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                conv.test.slope.tol = 0.09),fit = T)
Extent_null_2states_div_BFGS <- MARSS(y = Extent_DivR4, model = moddiv_null_2states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = Extent_null_2states_div$par)

#1 state Extent
Extent_1state <- MARSS(Extent_DryR4, model = mod_1state_qdiaeq, 
                                 control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                conv.test.slope.tol = 0.09),fit = T) 
Extent_1state_BFGS <- MARSS(y = Extent_DryR4, model = mod_1state_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = Extent_1state$par)

Extent_null_1state <- MARSS(Extent_DryR4, model = mod_null_1state_qdiaeq, 
                                      control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                     conv.test.slope.tol = 0.09),fit = T) 
Extent_null_1state_BFGS <- MARSS(y = Extent_DryR4, model = mod_null_1state_qdiaeq, control = list(maxit = 5000), 
                                method = "BFGS", inits = Extent_null_1state$par)

beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))

#AIC ####
Extent_AIC <- c(Extent_3states_dry_BFGS$AICc, 
                Extent_null_3states_dry_BFGS$AICc, 
                Extent_2states_dry_BFGS$AICc, 
                Extent_null_2states_dry_BFGS$AICc, 
                Extent_2states_div_BFGS$AICc, 
                Extent_null_2states_div_BFGS$AICc, 
                Extent_1state_BFGS$AICc,
                Extent_null_1state_BFGS$AICc)

ExtDelAIC <- Extent_AIC - min(Extent_AIC)
ExtRelLik <- exp(-0.5 * ExtDelAIC)
ExtAICWeight <- ExtRelLik/sum(ExtRelLik)
ExtAICTable <- data.frame(AICc = Extent_AIC, delAIC = ExtDelAIC, relLike = ExtRelLik,
                          weight = ExtAICWeight)
rownames(ExtAICTable) <- c("3dry_diaeq", 
                           "3dry_null_diaeq", 
                           "2dry_diaeq", 
                           "2dry_null_diaeq", 
                           "2div_diaeq", 
                           "2div_null_diaeq", 
                           "1",
                           "1_null")
ExtAICTable %>% mutate(across(where(is.numeric),round,0)) %>% arrange(delAIC)

#save and read top model ####
saveRDS(Extent_2states_div_BFGS, "ModelOutput/Top_ExtentMod_BFGS.rds") # ~ 4 min R1 raw
saveRDS(Extent_2states_div_BFGS, "ModelOutput/Top_ExtentNAMod_BFGS.rds") # ~ 11 min R2 NA
saveRDS(Extent_2states_div_BFGS, "ModelOutput/Top_ExtentLogMod_BFGS.rds") # ~ 6 min R3 log
saveRDS(Extent_2states_div_BFGS, "ModelOutput/Top_ExtentLogNAMod_BFGS.rds") # ~ 10 min R4 logNA
# 
mod1 <- readRDS("ModelOutput/Top_ExtentMod_BFGS.rds")
mod2 <- readRDS("ModelOutput/Top_ExtentNAMod_BFGS.rds")
mod3 <- readRDS("ModelOutput/Top_ExtentLogMod_BFGS.rds") 
mod4 <- readRDS("ModelOutput/Top_ExtentLogNAMod_BFGS.rds")

#Residuals ####
autoplot.marssMLE(mod4)

plot(mod) #this give a slightly different picture than autoplot
predict(mod)
plot(mod, plot.type="model.resids.ytT") #smoothations model residuals as opposed to innovation 

#model output  ####
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

MARSSparamCIs(mod1) #tidy.marssMLE does the same thing
MARSSparamCIs(mod2) #tidy.marssMLE does the same thing
MARSSparamCIs(mod3) #tidy.marssMLE does the same thing
MARSSparamCIs(mod4) #tidy.marssMLE does the same thing

fitted(mod) #data and state fitted values (predictions)
preds <- predict(mod,
                     interval = "confidence",
                     se.fit = TRUE) #another way to get estimate and CIs
#plotting
conf_marss1 <- fitted(mod1, type = "ytT", interval = "confidence")
pred_marss1 <- fitted(mod1, type = "ytT", interval = "prediction")
df <- cbind(conf_marss1, pred_marss1[, c(".lwr", ".upr")]) %>% 
  rename(Reach = 1)

ggplot(df, aes(x = t, y = .fitted)) +
  geom_ribbon(aes(ymin = .lwr, ymax = .upr), fill = "grey") +
  geom_ribbon(aes(ymin = .conf.low, ymax = .conf.up), fill = "blue", alpha = 0.25) +
  geom_line(linetype = 2) +
  facet_grid(vars(Reach))+
  ylab("Predicted Extent Dry") +
  xlab("") +
  ggtitle("Rio Grande")






