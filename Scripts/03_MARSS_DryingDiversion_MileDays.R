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

dat_DryR <- read.csv("Data/Processed/DryingSubreachData.csv", header = T) %>% 
  mutate(NAMileDays = MileDays) %>% 
  mutate_at(c('NAMileDays'), ~na_if(., 0)) %>% 
  mutate(LogMileDays = log(MileDays+0.0001),
         LogNAMileDays = log(NAMileDays))
dat_DivR <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T) %>% 
  mutate(NAMileDays = MileDays) %>% 
  mutate_at(c('NAMileDays'), ~na_if(., 0)) %>% 
  mutate(LogMileDays = log(MileDays+0.0001),
         LogNAMileDays = log(NAMileDays))

#response distribution plots #####
dat_DryR %>% 
  group_by(Reach) %>% 
  mutate(zLogMileDays = (LogMileDays - mean(LogMileDays, na.rm = TRUE))/ sd(LogMileDays, na.rm = TRUE)) %>% 
  mutate(zMileDays = (MileDays - mean(MileDays, na.rm = TRUE))/ sd(MileDays, na.rm = TRUE)) %>% 
  ggplot(aes(zLogMileDays))+
  geom_histogram()+
  facet_wrap(vars(Reach), scales = "free_x")

dat_DivR %>% 
  group_by(Reach) %>% 
  mutate(zLogMileDays = (LogMileDays - mean(LogMileDays, na.rm = TRUE))/ sd(LogMileDays, na.rm = TRUE)) %>% 
  mutate(zMileDays = (MileDays - mean(MileDays, na.rm = TRUE))/ sd(MileDays, na.rm = TRUE)) %>% 
  ggplot(aes(zLogMileDays))+
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
MileDays_DryR1 <- predictor_func(dat_DryR, MileDays)
MileDays_DryR2 <- predictor_func(dat_DryR, NAMileDays)
MileDays_DryR3 <- predictor_func(dat_DryR, LogMileDays)
MileDays_DryR4 <- predictor_func(dat_DryR, LogNAMileDays)

#diversion reaches
MileDays_DivR1 <- predictor_func(dat_DivR, MileDays)
MileDays_DivR2 <- predictor_func(dat_DivR, NAMileDays)
MileDays_DivR3 <- predictor_func(dat_DivR, LogMileDays)
MileDays_DivR4 <- predictor_func(dat_DivR, LogNAMileDays)

#creating a time series of first predictive variable
MileDays_DivR <- ts(MileDays_DivR1, frequency = 365)
plot(MileDays_DivR[1,])

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
moddry_3statescum_qdiaeq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                              c=all_cov_matrix$PredCum_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #3 drynull 
moddry_null_3statescum_qdiaeq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and equal",
                                 Z = "identity", A = matrix(0,3,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)

#2 states

  #2 dry cum 
moddry_2statescum_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$PredCum_Dry_2states, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 dry cum null 
moddry_null_2statescum_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                                 Z = Z_2states, A = matrix(0,3,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)

   #2 diversion cum
moddiv_2statescum_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$PredCum_Div_2states, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 diversion cum null
moddiv_null_2statescum_qdiaeq <- list(B = "identity", U = matrix(0,2,1), Q = "diagonal and equal",
                                 Z = "identity", A = matrix(0,2,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)

#1 state
  #1 dry cum
mod_1statecum_qdiaeq <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                             c=all_cov_matrix$PredCum_Dry_1state, C=C_1state, Z = matrix(1,3,1), A = matrix(0,3,1), 
                             R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #1 dry cum null
mod_null_1statecum_qdiaeq <- list(B = matrix(1), U = matrix(0,1,1), Q = "diagonal and equal",
                                Z = matrix(1,3,1), A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)

#model fits ####
#replace R.. with R1 = raw NA; R2 = 0s to NA; R3 = log(raw+0.001); R4 0s to NA and log(raw)

#3 state Daily days
start.time <- Sys.time()

MD_3states_dry <- MARSS(MileDays_DryR4, model = moddry_3statescum_qdiaeq, 
                               control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                              conv.test.slope.tol = 0.09), fit = T) # R2 R matrix has to be 0
  MD_3states_dry_BFGS <- MARSS(y = MileDays_DryR4, model = moddry_3statescum_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = MD_3states_dry$par) 

MD_null_3states_dry <- MARSS(MileDays_DryR4, model = moddry_null_3statescum_qdiaeq, 
                                    control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                   conv.test.slope.tol = 0.09), fit = T) # R2 R matrix has to be 0
  MD_null_3states_dry_BFGS <- MARSS(y = MileDays_DryR4, model = moddry_null_3statescum_qdiaeq, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_null_3states_dry$par) 
#2 state MileDays
  #dry
MD_2states_dry <- MARSS(MileDays_DryR4, model = moddry_2statescum_qdiaeq, 
                               control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                              conv.test.slope.tol = 0.09), fit = T) #R2 error R matrix 0 (over-determined), works diaeq
  MD_2states_dry_BFGS <- MARSS(y = MileDays_DryR4, model = moddry_2statescum_qdiaeq, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_2states_dry$par)

MD_null_2states_dry <- MARSS(MileDays_DryR4, model = moddry_null_2statescum_qdiaeq, 
                                    control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                   conv.test.slope.tol = 0.09), fit = T) #R2 error R matrix 0 (over-determined), works diaeq
  MD_null_2states_dry_BFGS <- MARSS(y = MileDays_DryR4, model = moddry_null_2statescum_qdiaeq, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_null_2states_dry$par)

  #div
MD_2states_div <- MARSS(MileDays_DivR4, model = moddiv_2statescum_qdiaeq, 
                               control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                              conv.test.slope.tol = 0.09), fit = T) # R2 R matrix has to be 0
  MD_2states_div_BFGS <- MARSS(y = MileDays_DivR4, model = moddiv_2statescum_qdiaeq, control = list(maxit = 5000), 
                                  method = "BFGS", inits = MD_2states_div$par) 

MD_null_2states_div <- MARSS(MileDays_DivR4, model = moddiv_null_2statescum_qdiaeq, 
                                    control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                   conv.test.slope.tol = 0.09), fit = T) # R2 R matrix has to be 0
  MD_null_2states_div_BFGS <- MARSS(y = MileDays_DivR4, model = moddiv_null_2statescum_qdiaeq, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_null_2states_div$par) 

#1 state Mile Days
MD_1state <- MARSS(MileDays_DryR4, model = mod_1statecum_qdiaeq, 
                          control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                         conv.test.slope.tol = 0.09), fit = T) #R2 error R matrix 0 (over-determined), works diaeq
  MD_1state_BFGS <- MARSS(y = MileDays_DryR4, model = mod_1statecum_qdiaeq, control = list(maxit = 5000), 
                                  method = "BFGS", inits = MD_1state$par)

MD_null_1state <- MARSS(MileDays_DryR4, model = mod_null_1statecum_qdiaeq, 
                               control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                              conv.test.slope.tol = 0.09), fit = T)
  MD_null_1state_BFGS <- MARSS(y = MileDays_DryR4, model = mod_null_1statecum_qdiaeq, control = list(maxit = 5000), 
                        method = "BFGS", inits = MD_null_1state$par)

beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))


#AIC ####
#Mile Days
MD_AIC <- c(MD_3states_dry_BFGS$AICc, 
                MD_null_3states_dry_BFGS$AICc, 
                MD_2states_dry_BFGS$AICc, 
                MD_null_2states_dry_BFGS$AICc, 
                MD_2states_div_BFGS$AICc, 
                MD_null_2states_div_BFGS$AICc, 
                MD_1state_BFGS$AICc,
                MD_null_1state_BFGS$AICc)

ExtDelAIC <- MD_AIC - min(MD_AIC)
ExtRelLik <- exp(-0.5 * ExtDelAIC)
ExtAICWeight <- ExtRelLik/sum(ExtRelLik)
ExtAICTable <- data.frame(AICc = MD_AIC, delAIC = ExtDelAIC, relLike = ExtRelLik,
                          weight = ExtAICWeight)
rownames(ExtAICTable) <- c("3dry_diaeq", 
                           "3dry_null_diaeq", 
                           "2dry_diaeq", 
                           "2dry_null_diaeq", 
                           "2div_diaeq", 
                           "2div_null_diaeq", 
                           "1div",
                           "1div_null")
ExtAICTable %>% mutate(across(where(is.numeric),round,0)) %>% arrange(delAIC)

#save top model and read ####
#replace R.. with R1 = raw NA; R2 = 0s to NA; R3 = log(raw+0.001); R4 0s to NA and log(raw)

saveRDS(MD_3states_dry_BFGS, "ModelOutput/Top_MDMod_BFGS.rds") # ~6 min R1 raw
saveRDS(MD_3states_dry_BFGS, "ModelOutput/Top_MDNAMod_BFGS.rds") # ~9 min R2 NAs raw had to vary R matrix some
saveRDS(MD_3states_dry_BFGS, "ModelOutput/Top_MDLogMod_BFGS.rds") # ~6 min R3 lograw
saveRDS(MD_3states_dry_BFGS, "ModelOutput/Top_MDLogNAMod_BFGS.rds") # ~10 min R4 logNAraw

mod1 <- readRDS("ModelOutput/Top_MDMod_BFGS.rds") #not horrible, not good neg residuals and thus Q-Q plot, first ACF bad and rest good (so maybe ok)
mod2 <- readRDS("ModelOutput/Top_MDNAMod_BFGS.rds") #bad acf
mod3 <- readRDS("ModelOutput/Top_MDLogMod_BFGS.rds") # not good neg and pos residuals and Q-Q, first ACF bad and rest good (so maybe ok)
mod4 <- readRDS("ModelOutput/Top_MDLogMod_BFGS.rds") # same as lograw (mod3)

#residuals ####
autoplot.marssMLE(mod1)

#model output####
summary(mod1)
predict(mod1)
fitted(mod3)
preds <- predict(mod1,
                 interval = "confidence",
                 se.fit = TRUE) #another way to get estimate and CIs

#plotting
conf_marss1 <- fitted(mod1, type = "ytT", interval = "confidence")
pred_marss1 <- fitted(mod3, type = "ytT", interval = "prediction")
df <- cbind(conf_marss1, pred_marss1[, c(".lwr", ".upr")]) %>% 
  rename(Reach = 1)

ggplot(df, aes(x = t, y = .fitted)) +
  geom_ribbon(aes(ymin = .lwr, ymax = .upr), fill = "grey") +
  geom_ribbon(aes(ymin = .conf.low, ymax = .conf.up), fill = "blue", alpha = 0.25) +
  geom_line(linetype = 2) +
  facet_grid(vars(Reach))+
  ylab("Predicted Mile Days") +
  xlab("") +
  ggtitle("Rio Grande")

