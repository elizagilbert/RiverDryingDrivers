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
#but can't log transform 0s so added min value to all values

dat_Dry0 <- read.csv("Data/Processed/DryingSubreachData.csv", header = T) %>% 
  select(Date, ExtentChng, Reach)
dat_Div0 <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T)%>% 
  select(Date, ExtentChng, Reach)

#wrangle predictor data ####
minExtentChng_dry <- abs(min(dat_Dry0$ExtentChng))
minExtentChng_div <- abs(min(dat_Div0$ExtentChng))

#absolute negative numbers, replace na raw extent, replace na absolute, log na absolute, scale all variables
dat_Dry <- dat_Dry0 %>% 
  mutate(NAExtentChng = ExtentChng) %>% 
  mutate_at(c("NAExtentChng"), ~na_if(.,0)) %>% 
  mutate(PosExtentChng = ExtentChng + minExtentChng_dry) %>% 
  filter(PosExtentChng != 0) %>%
  mutate(LogPosExtentChng = log(PosExtentChng)) %>% 
  mutate(NA_PosExtentChng = case_when(PosExtentChng == minExtentChng_dry ~ 0,
                              TRUE ~ PosExtentChng)) %>% 
  mutate_at(c("NA_PosExtentChng"), ~na_if(.,0)) %>% 
  mutate(LogNA_PosExtentChng = log(NA_PosExtentChng)) %>% 
  mutate_at(c("ExtentChng", "NAExtentChng", "PosExtentChng", "LogPosExtentChng", "NA_PosExtentChng", "LogNA_PosExtentChng"), scale)

dat_Div <- dat_Div0 %>% 
  mutate(NAExtentChng = ExtentChng) %>% 
  mutate_at(c("NAExtentChng"), ~na_if(.,0)) %>% 
  mutate(PosExtentChng = ExtentChng + minExtentChng_dry) %>% 
  filter(PosExtentChng != 0) %>%
  mutate(LogPosExtentChng = log(PosExtentChng)) %>% 
  mutate(NA_PosExtentChng = case_when(PosExtentChng == minExtentChng_dry ~ 0,
                                      TRUE ~ PosExtentChng)) %>% 
  mutate_at(c("NA_PosExtentChng"), ~na_if(.,0)) %>% 
  mutate(LogNA_PosExtentChng = log(NA_PosExtentChng)) %>% 
  mutate_at(c("ExtentChng", "NAExtentChng", "PosExtentChng", "LogPosExtentChng", "NA_PosExtentChng", "LogNA_PosExtentChng"), 
            scale)

#Response distribution plots #####
dat_Dry %>% 
  group_by(Reach) %>% 
  ggplot(aes(PosExtentChng))+
  geom_histogram()+
  facet_wrap(vars(Reach), scales = "free_x")

dat_Div %>% 
  group_by(Reach) %>% 
  ggplot(aes(LogNA_PosExtentChng))+
  geom_histogram()+
  facet_wrap(vars(Reach), scales = "free_x")

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

#response ExtentChng ####
predictor_func <- function(data, predictor){
  result <- 
    as.matrix(data %>% 
                mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
                filter(between(month(Date), 4,10)) %>% 
                select(Date, {{predictor}}, Reach) %>% 
                group_by(Reach) %>% 
                pivot_wider(names_from = Date, values_from = {{predictor}}) %>% 
                column_to_rownames(var = "Reach"))
  
  return(result)
}

 #drying reaches
ExtentChng_DryR1 <- predictor_func(dat_Dry, ExtentChng) #raw data
ExtentChng_DryR2 <- predictor_func(dat_Dry, NAExtentChng) #NA raw data
ExtentChng_DryR3 <- predictor_func(dat_Dry,PosExtentChng) #positive raw
ExtentChng_DryR4 <- predictor_func(dat_Dry,LogPosExtentChng) #min value added to all negative, removed 1 zero, and logged
ExtentChng_DryR5 <- predictor_func(dat_Dry,NA_PosExtentChng) #positive with na at raw 0s 
ExtentChng_DryR6 <- predictor_func(dat_Dry,LogNA_PosExtentChng) #positive with na at raw 0s and log

  #diversion reaches
ExtentChng_DivR1 <- predictor_func(dat_Div, ExtentChng) #raw data
ExtentChng_DivR2 <- predictor_func(dat_Div, NAExtentChng) #NA raw data
ExtentChng_DivR3 <- predictor_func(dat_Div,PosExtentChng) #positive raw
ExtentChng_DivR4 <- predictor_func(dat_Div,LogPosExtentChng) #min value added to all negative, removed 1 zero, and logged
ExtentChng_DivR5 <- predictor_func(dat_Div,NA_PosExtentChng) #positive with na at raw 0s 
ExtentChng_DivR6 <- predictor_func(dat_Div,LogNA_PosExtentChng) #positive with na at raw 0s and log

#creating a time series of first predictive variable
ExtentChng_DryR_ts <- ts(ExtentChng_DryR1, frequency = 365)
plot(ExtentChng_DryR_ts[1,])

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
                        c=all_cov_matrix$Pred_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                        R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #3 dry null
moddry_null_3states_qdiaeq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                              Z = "identity", A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

#2 states
  #2 dry
moddry_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$Pred_Dry_2states, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 dry null
moddry_null_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              Z = Z_2states, A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 diversion
moddiv_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$Pred_Div_2states, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 diversion null
moddiv_null_2states_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
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

  #1 diversion
moddiv_1state_qdiaeq <- list(B = matrix(1), U = matrix(1), Q = "diagonal and equal",
                      c=all_cov_matrix$Pred_Div_1state, C=C_1state, Z = matrix(1,2,1), A = matrix(0,2,1), 
                      R = "diagonal and equal", x0 = "equal", tinitx = 0)
  
  #1 diversion null
moddiv_null_1state_qdiaeq <- list(B = matrix(1), U = matrix(1), Q = "diagonal and equal",
                             Z = matrix(1,2,1), A = matrix(0,2,1), 
                             R = "diagonal and equal", x0 = "equal", tinitx = 0)


#model fits ####

#replace R.. with R1 = raw; R2 = NA raw; R3 = Pos raw; R4 = logPos; R5 = NA_Pos; R6 = logNA_Pos

#3 states ExtentChng
  #with NA ExtentChng kem does not converge
start.time <- Sys.time()
ExtentChng_3states_dry <- MARSS(y = ExtentChng_DryR6, model = moddry_3states_qdiaeq, 
                                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                  conv.test.slope.tol = 0.09), fit = T) 
ExtentChng_3states_dry_BFGS <- MARSS(y = ExtentChng_DryR6, model = moddry_3states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = ExtentChng_3states_dry$par)

ExtentChng_null_3states_dry <- MARSS(ExtentChng_DryR6, model = moddry_null_3states_qdiaeq, 
                                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                       conv.test.slope.tol = 0.09), fit = T) 
ExtentChng_null_3states_dry_BFGS <- MARSS(y = ExtentChng_DryR6, model = moddry_null_3states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = ExtentChng_null_3states_dry$par)

#2 states ExtentChng
ExtentChng_2states_dry <- MARSS(ExtentChng_DryR6, model = moddry_2states_qdiaeq, 
                                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                  conv.test.slope.tol = 0.09),fit = T) 
ExtentChng_2states_dry_BFGS <- MARSS(y = ExtentChng_DryR6, model = moddry_2states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = ExtentChng_2states_dry$par)

ExtentChng_null_2states_dry <- MARSS(ExtentChng_DryR6, model = moddry_null_2states_qdiaeq, 
                                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                       conv.test.slope.tol = 0.09),fit = T) 
ExtentChng_null_2states_dry_BFGS <- MARSS(y = ExtentChng_DryR6, model = moddry_null_2states_qdiaeq, control = list(maxit = 5000), 
                                      method = "BFGS", inits = ExtentChng_null_2states_dry$par)

ExtentChng_2states_div <- MARSS(ExtentChng_DivR6, model = moddiv_2states_qdiaeq, 
                                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                  conv.test.slope.tol = 0.09),fit = T) 
ExtentChng_2states_div_BFGS <- MARSS(y = ExtentChng_DivR6, model = moddiv_2states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = ExtentChng_2states_div$par)

ExtentChng_null_2states_div <- MARSS(ExtentChng_DivR6, model = moddiv_null_2states_qdiaeq, 
                                 control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                conv.test.slope.tol = 0.09),fit = T)
ExtentChng_null_2states_div_BFGS <- MARSS(y = ExtentChng_DivR6, model = moddiv_null_2states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = ExtentChng_null_2states_div$par)

#1 states ExtentChng
ExtentChng_1state_dry <- MARSS(ExtentChng_DryR6, model = moddry_1state_qdiaeq, 
                                 control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                conv.test.slope.tol = 0.09),fit = T) 
ExtentChng_1state_dry_BFGS <- MARSS(y = ExtentChng_DryR6, model = moddry_1state_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = ExtentChng_1state_dry$par)

ExtentChng_null_1state_dry <- MARSS(ExtentChng_DryR6, model = moddry_null_1state_qdiaeq, 
                                      control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                     conv.test.slope.tol = 0.09),fit = T) 
ExtentChng_null_1state_dry_BFGS <- MARSS(y = ExtentChng_DryR6, model = moddry_null_1state_qdiaeq, control = list(maxit = 5000), 
                                method = "BFGS", inits = ExtentChng_null_1state_dry$par)

ExtentChng_1state_div <- MARSS(ExtentChng_DivR6, model = moddiv_1state_qdiaeq, 
                                 control = list(maxit = 100, allow.degen = T, safe = T, 
                                                conv.test.slope.tol = 0.09),fit = T) # ExtentChng trace = 1 error
ExtentChng_1state_div_BFGS <- MARSS(y = ExtentChng_DivR6, model = moddiv_1state_qdiaeq, control = list(maxit = 5000), 
                                method = "BFGS", inits = ExtentChng_1state_div$par)

ExtentChng_null_1state_div <- MARSS(ExtentChng_DivR6, model = moddiv_null_1state_qdiaeq, 
                                 control = list(maxit = 100, allow.degen = T, safe = T, 
                                                conv.test.slope.tol = 0.09), fit = T) # ExtentChng trace = 1 error
ExtentChng_null_1state_div_BFGS <- MARSS(y = ExtentChng_DivR6, model = moddiv_null_1state_qdiaeq, control = list(maxit = 5000), 
                                method = "BFGS", inits = ExtentChng_null_1state_div$par)

beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))

###AIC ####
ExtentChng_AIC <- c(ExtentChng_3states_dry_BFGS$AICc, 
                ExtentChng_null_3states_dry_BFGS$AICc, 
                ExtentChng_2states_dry_BFGS$AICc, 
                ExtentChng_null_2states_dry_BFGS$AICc, 
                ExtentChng_1state_dry_BFGS$AICc,
                ExtentChng_null_1state_dry_BFGS$AICc,
                ExtentChng_2states_div_BFGS$AICc, 
                ExtentChng_null_2states_div_BFGS$AICc, 
                ExtentChng_1state_div_BFGS$AICc,
                ExtentChng_null_1state_div_BFGS$AICc)

ExtDelAIC <- ExtentChng_AIC - min(ExtentChng_AIC)
ExtRelLik <- exp(-0.5 * ExtDelAIC)
ExtAICWeight <- ExtRelLik/sum(ExtRelLik)
ExtAICTable <- data.frame(AICc = ExtentChng_AIC, delAIC = ExtDelAIC, relLike = ExtRelLik,
                          weight = ExtAICWeight)
rownames(ExtAICTable) <- c("3dry_diaeq", 
                           "3dry_null_diaeq", 
                           "2dry_diaeq", 
                           "2dry_null_diaeq", 
                           "1dry",
                           "1dry_null",
                           "2div_diaeq", 
                           "2div_null_diaeq", 
                           "1div",
                           "1div_null")
ExtAICTable %>% mutate(across(where(is.numeric),round,0)) %>% arrange(delAIC)

#save and read top model ####
#replace R.. with R1 = raw; R2 = NA raw; R3 = Pos raw; R4 = logPos; R5 = NA_Pos; R6 = logNA_Pos

saveRDS(ExtentChng_null_2states_div_BFGS, "ModelOutput/Top_ExtentChngMod_BFGS.rds") # ~ 10 min R1 raw
saveRDS(ExtentChng_null_2states_div_BFGS, "ModelOutput/Top_ExtentChngNAMod_BFGS.rds") # ~ 14 min R2 0s replaced with NA
saveRDS(ExtentChng_null_2states_div_BFGS, "ModelOutput/Top_ExtentChngPosMod_BFGS.rds") # ~ 9 min R3 Positive raw
saveRDS(ExtentChng_2states_div_BFGS, "ModelOutput/Top_ExtentChngLogPosMod_BFGS.rds") # ~ 10 min R4 Log Positive raw
saveRDS(ExtentChng_null_2states_div_BFGS, "ModelOutput/Top_ExtentChngNAPosMod_BFGS.rds") # ~ 14 min R4 NA Positive raw
saveRDS(ExtentChng_null_2states_div_BFGS, "ModelOutput/Top_ExtentChngLogNAPosMod_BFGS.rds") # ~ 14 min R4 LogNA Positive raw

mod1 <- readRDS("ModelOutput/Top_ExtentChngMod_BFGS.rds")
mod2 <- readRDS("ModelOutput/Top_ExtentChngNAMod_BFGS.rds")
mod3 <- readRDS("ModelOutput/Top_ExtentChngPosMod_BFGS.rds")
mod4 <- readRDS("ModelOutput/Top_ExtentChngLogPosMod_BFGS.rds")
mod5 <- readRDS("ModelOutput/Top_ExtentChngNAPosMod_BFGS.rds")
mod6 <- readRDS("ModelOutput/Top_ExtentChngLogNAPosMod_BFGS.rds")

#Residuals ####
autoplot.marssMLE(mod6)

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
  ylab("Predicted ExtentChng Dry") +
  xlab("") +
  ggtitle("Rio Grande")






