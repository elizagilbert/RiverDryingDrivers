#Read Me ####
#The purpose of this script is to run a MARSS model on Rio Grande Reach 1 data

#Library ####
library(MARSS)
library(tidyverse)
library(lubridate)


#Data ####
dat_R1 <- read.csv("Data/Processed/Reach1_ResponseAndCovariates.csv", header = T) %>% 
  mutate(RiverReach1_Date = as.Date(RiverReach1_Date, format = "%Y-%m-%d"))

  #transforming as done in MARSS package training 
  #lakeWAPlantonTrans has zeros replaced with NAs (missing) for plankton
  #logged (natural log) raw plankton counts 
  #z score plankton
  #plankton been standardized to mean of zero and variance of 1 (so logged and then z-scored). 
  #Temp, TP and pH also z-scored but not logged. 
  #Single missing temp value replaced with -1 and single missing TP replaced with -0.3

 #LOGGING NEGATIVE NUMBERS IN THE CHANGE EXTENT PRODUCES NANS AND THIS IS NOT GOOD!!!!!
 #So I'm adding the absolute value of the most negative number to all change extent so
  #the most negative number become 0 and shifts everything else up

minExtentDry <- abs(min(dat_R1$ChngExtentDry))

R1_Trans <- dat_R1 %>% 
  mutate(AbsChngExtentDry = ChngExtentDry + minExtentDry) %>% 
  mutate_at(c('AbsChngExtentDry', 'SumDaysDry', 'UpperLocationDry'), ~na_if(.,0)) %>%   #replace 0 with na in response variables
  mutate_at('AbsChngExtentDry', ~na_if(.,minExtentDry)) %>%    #replace normalized to 0 with na in response variables
  mutate_at(setNames(c('AbsChngExtentDry', 'SumDaysDry', 'UpperLocationDry'), 
                     paste0("log_", c('AbsChngExtentDry', 'SumDaysDry', 'UpperLocationDry'))), 
            log) %>% 
  mutate_at(setNames(c("Precip_LosLunas2_mm","TempMax_LosLunas2_C", "RiverDischarge_BosqueFarms_cfs",
                                "Diversion_Isleta_Totalcfs", "Returns_Totalcfs", 
                                "log_AbsChngExtentDry", "log_SumDaysDry", "log_UpperLocationDry"),
                              paste0("scaled_", c("Precip_LosLunas2_mm","TempMax_LosLunas2_C", "RiverDischarge_BosqueFarms_cfs",
                                                  "Diversion_Isleta_Totalcfs", "Returns_Totalcfs", 
                                                  "log_AbsChngExtentDry", "log_SumDaysDry", "log_UpperLocationDry"))),
            funs(c(scale(.))))


R1_Trans_red <- R1_Trans %>% 
  filter(year(RiverReach1_Date) <= 2021)

#Univariate Sum Days Dry ####
#creating a time series of first predictive variable
Scaled_SumDaysDry_ts <- ts(R1_Trans_red$scaled_log_SumDaysDry, frequency = 365)
plot(Scaled_SumDaysDry_ts)


#Models
q <- 0.1
r <- 0.1

 #flat level model
 #only error get is in the R (data error)  
mod.list.0 <- list(B = matrix(1), U = matrix(0), Q = matrix(0), 
                   Z = matrix(1), A = matrix(0), R = matrix("r"), 
                   x0 = matrix("mu"), tinitx = 0)

fit_flat_SDD <- MARSS(Scaled_SumDaysDry_ts, model = mod.list.0)

 #linear trend model 
 #by giving u the average of the data at U and keeping data error R
mod.list.1 <- list(B = matrix(1), U = matrix("u"), Q = matrix(0), 
                   Z = matrix(1), A = matrix(0), R = matrix("r"), 
                   x0 = matrix("mu"), tinitx = 0)
fit_linear_SDD <- MARSS(Scaled_SumDaysDry_ts, model = mod.list.1)

 #stochastic random walk model 
 #by removing the linear trend U and placing error in hidden
 #state Q, keeping error in data R

mod.list.2 <- list(B = matrix(1), U = matrix(0), Q = matrix("q"), 
                   Z = matrix(1), A = matrix(0), R = matrix("r"), 
                   x0 = matrix("mu"), tinitx = 0)
fit_stoch_SDD <- MARSS(Scaled_SumDaysDry_ts, model = mod.list.2)

 #stochastic with drift (allows random walk to drift up or down based on U)
 #by using Q in hidden, and U in trend, maintaining data error R
mod.list.3 <- list(B = matrix(1), U = matrix("u"), Q = matrix("q"), 
                   Z = matrix(1), A = matrix(0), R = matrix("r"), 
                   x0 = matrix("mu"), tinitx = 0)
fit_stoch_drift_SDD <- MARSS(Scaled_SumDaysDry_ts, model = mod.list.3)

#plotting model with best AIC
 #stochastic without any drift 
mod_output <- as.data.frame(t(fit_stoch_SDD$states)) %>% 
  rename(Pred_mod = 1)
pl_output <- cbind(R1_Trans_red$RiverReach1_Date, mod_output) %>% 
  rename(RiverReach1_Date = 1)

ggplot(data = R1_Trans_red, aes(x = RiverReach1_Date, y = scaled_log_SumDaysDry))+
  geom_line(size = 1)+
  geom_point(data = pl_output, aes(x = RiverReach1_Date, y = Pred_mod, 
                                  color = "red"), size = 1)+
  theme_classic()+
  ggtitle("Best model: stochastic without drift (red line) AIC = 585")+
  xlab("") + ylab("scaled sum of dry days")+
  theme(legend.position = "none")

#AIC table
Mod_AIC <- c(fit_flat_SDD$AIC, fit_linear_SDD$AIC, fit_stoch_SDD$AIC, fit_stoch_drift_SDD$AIC)
del_AIC <- Mod_AIC- min(Mod_AIC)  
relLik <- exp(-0.5 * del_AIC)
aicweight <- relLik/sum(relLik)

aic.table <- data.frame(AIC = Mod_AIC, delAIC = del_AIC, relLik = relLik,
                        weight = aicweight)
rownames(aic.table) <- c("flat level", "linear trend", "stochastic", "stochastic w/drift" )
round(aic.table, digits = 3)

#use innovative residuals 
par(mfrow = c(2, 2), mar = c(2, 2, 4, 2))
resids <- residuals(fit_flat_SDD)
acf(resids$.resids, main = "flat level v(t)", na.action = na.pass)
resids <- residuals(fit_linear_SDD)
acf(resids$.resids, main = "linear trend v(t)", na.action = na.pass)
resids <- residuals(fit_stoch_SDD)
acf(resids$.resids, main = "stoc level v(t)", na.action = na.pass)
resids <- residuals(fit_stoch_drift_SDD)
acf(resids$.resids, main = "stoc level drift v(t)", na.action = na.pass)

#use Smoothation residuals to detect outliers 
 #can use MARSS residuals below
par(mfrow = c(3, 2))
resids <- MARSSresiduals(fit_flat_SDD)
plot(resids$model.residuals[1, ],
     ylab = "model residual", xlab = "", main = "flat level")
abline(h = 0)
plot(resids$state.residuals[1, ],
     ylab = "state residual", xlab = "", main = "flat level")
abline(h = 0)

resids <- MARSSresiduals(fit_linear_SDD)
plot(resids$model.residuals[1, ], ylab = "model residual", xlab = "", main = "linear trend")
abline(h = 0)
plot(resids$state.residuals[1, ], ylab = "state residual", xlab = "", main = "linear trend", ylim = c(-1, 1))
abline(h = 0)

resids <- MARSSresiduals(fit_stoch_SDD)
plot(resids$model.residuals[1, ], ylab = "model residual", xlab = "", main = "stoc level")
abline(h = 0)
plot(resids$state.residuals[1, ], ylab = "state residual", xlab = "", main = "stoc level")
abline(h = 0)
 
 #look at outliers with type  "tT"
par(mfrow = c(1, 2))
resids <- residuals(fit_stoch_SDD, type="tT", standardization="marginal")
mresids <- subset(resids, name == "model")
plot(mresids$t, mresids$.std.resids,
     ylab = "model std smoothationl", xlab = "", main = "data outliers")
abline(h = 0)
abline(h = c(2,-2), lty=2)
sresids <- subset(resids, name == "state")
plot(sresids$t, sresids$.std.resids, type="l",
     ylab = "state std smoothation", xlab = "", main = "sudden level changes")
abline(h = 0)
abline(h = c(2,-2), lty=2)

#Sum Days Dry and Extent with Precipitation and Diversion ####
  
##----------view the data
R1_Trans_red %>% 
  select(RiverReach1_Date, scaled_log_SumDaysDry, scaled_log_UpperLocationDry,
         scaled_Precip_LosLunas2_mm, scaled_Diversion_Isleta_Totalcfs) %>% 
  pivot_longer(cols = scaled_log_SumDaysDry:scaled_Diversion_Isleta_Totalcfs,
               names_to = "variables",values_to = "values") %>% 
  ggplot(aes(x = RiverReach1_Date, y = values))+
  geom_line()+
  facet_grid(vars(variables), scales = "free_y")+
  theme_bw()

##------convert data to matrix form ---
 #need SSD and covariates as transposed number
SSD_Upp_t <- t(cbind(R1_Trans_red$scaled_log_SumDaysDry, R1_Trans_red$scaled_log_UpperLocationDry))
rownames(SSD_Upp_t) <- c("SSD", "UpperLoc")

Cov_t <- t(cbind(R1_Trans_red$scaled_Precip_LosLunas2_mm, R1_Trans_red$scaled_Diversion_Isleta_Totalcfs))
rownames(Cov_t) <- c("Precip", "Diversion")

##------observation error only model---
  #this is the same as an lm() with the A at zero because demeaned with scaling
Q <- U <- x0 <- "zero"
B <- Z <- "identity"
d <- Cov_t
A <- "zero" 
D <- "unconstrained"
y <- SSD_Upp_t  # to show relationship between dat & the equation
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, D = D, 
                   d = d, x0 = x0)
mod_obs <- MARSS(y, model = model.list)

##------process error only model---
 #similar to autoregressive but of the model of the data and not observation
R <- A <- U <- "zero"
B <- Z <- "identity"
Q <- "equalvarcov"
C <- "unconstrained"
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   C = C, c = Cov_t)
mod_proc <- MARSS(SSD_Upp_t, model = model.list)

##-----process error only but data fluctuating around a mean ---
 #autoregressive model -- a mean-reverting model 
 #make diagonal element of B to something other than 1
model.list$B <- "diagonal and unequal"
mod_proc_mn <- MARSS(SSD_Upp_t, model = model.list)
  
 #Log-likelihood goes up meaning model fits data better

## -----both process and observation error --
 #covariates only affecting process
 #first 225 observations are NA and have to remove to make x0 "unequal" and initialize x at 1
 #which allows for R to be fully determined by data

SSD_Upp_t2 <- SSD_Upp_t[,-c(1:225)]
Cov_t2 <- Cov_t[,-c(1:225)]

D <- d <- A <- U <- "zero"
Z <- "identity"
B <- "diagonal and unequal"
Q <- "equalvarcov"
C <- "unconstrained"
c <- Cov_t2
R <- diag(0.16, 2)
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
mod_proc_obs <- MARSS(SSD_Upp_t2, model = model.list)

##-----get AIC and loglik into a data frame --
Mod_stats <- data.frame(Model=c("Observation", "Process", "Process_Mean", "Process_Observ"),
           AIC=round(c(mod_obs$AIC,mod_proc$AIC,mod_proc_mn$AIC,mod_proc_obs$AIC),1),
           LogLikely = round(c(mod_obs$logLik,mod_proc$logLik,mod_proc_mn$logLik,mod_proc_obs$logLik),1))

##-----model coefficients for most likely model ---
mod_proc_mnC <- coef(mod_proc_mn, type = "matrix")$C
colnames(mod_proc_mnC) <- c("Precip", "Diversion")

##-----view results ---
  #process with mean 
mod_output <- as.data.frame(t(mod_proc_mn$states)) %>% 
  rename(Pred_mod = 1)
pl_output <- cbind(R1_Trans_red$RiverReach1_Date, mod_output) %>% 
  rename(RiverReach1_Date = 1)

ggplot(data = R1_Trans_red, aes(x = RiverReach1_Date, y = scaled_log_SumDaysDry))+
  geom_line(size = 1)+
  geom_point(data = pl_output, aes(x = RiverReach1_Date, y = Pred_mod, 
                                   color = "red"), size = 1)+
  theme_classic()+
  ggtitle("Best model: stochastic without drift (red line) AIC = 585")+
  xlab("") + ylab("scaled sum of dry days")+
  theme(legend.position = "none")

##-----diagnostics ---
  #MARSS residuals
resids <- MARSSresiduals(mod_proc_mn)

par(mfrow = c(1,2))
plot(resids$model.residuals[1, ],
     ylab = "model residual", xlab = "", main = "process mean")
plot(resids$state.residuals[1, ],
     ylab = "state residual", xlab = "", main = "process mean")

 #Smoothation residuals for outliers
resids_sm <- residuals(mod_proc_mn, type="tT", standardization="marginal")
mresids <- subset(resids_sm, name == "model")

plot(mresids)

plot(mresids$t, mresids$.std.resids,
     ylab = "model std smoothationl", xlab = "", main = "data outliers")
abline(h = 0); abline(h = c(2,-2), lty=2)
sresids <- subset(resids_sm, name == "state")
plot(sresids$t, sresids$.std.resids, type="l",
     ylab = "state std smoothation", xlab = "", main = "sudden level changes")
abline(h = 0); abline(h = c(2,-2), lty=2)

 #acf - innovation residuals
par(mfrow = c(1,1))
acf(resids_sm$.resids, main = "process mean", na.action = na.pass)

#Sum Days Dry and Extent with all covariates ####

##----------view the data
R1_Trans_red %>% 
  select(RiverReach1_Date, scaled_log_SumDaysDry, scaled_log_UpperLocationDry,
         scaled_Precip_LosLunas2_mm, scaled_TempMax_LosLunas2_C,
         scaled_Diversion_Isleta_Totalcfs, scaled_Returns_Totalcfs,
         scaled_RiverDischarge_BosqueFarms_cfs) %>% 
  pivot_longer(cols = scaled_log_SumDaysDry:scaled_RiverDischarge_BosqueFarms_cfs,
               names_to = "variables",values_to = "values") %>% 
  ggplot(aes(x = RiverReach1_Date, y = values))+
  geom_line()+
  facet_grid(vars(variables), scales = "free_y")+
  theme_bw()

##------convert data to matrix form ---
#need SSD and covariates as transposed number
SSD_Upp_t <- t(cbind(R1_Trans_red$scaled_log_SumDaysDry, R1_Trans_red$scaled_log_UpperLocationDry))
rownames(SSD_Upp_t) <- c("SSD", "UpperLoc")

Cov_t <- t(cbind(R1_Trans_red$scaled_Precip_LosLunas2_mm, R1_Trans_red$scaled_TempMax_LosLunas2_C,
                 R1_Trans_red$scaled_Diversion_Isleta_Totalcfs, R1_Trans_red$scaled_Returns_Totalcfs,
                 R1_Trans_red$scaled_RiverDischarge_BosqueFarms_cfs))
rownames(Cov_t) <- c("Precip", "Temp", "Diversion", "Return", "Discharge")

##------observation error only model---
#this is the same as an lm() with the A at zero because demeaned with scaling
Q <- U <- x0 <- "zero"
B <- Z <- "identity"
d <- Cov_t
A <- "zero" 
D <- "unconstrained"
y <- SSD_Upp_t  # to show relationship between dat & the equation
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, D = D, 
                   d = d, x0 = x0)
mod_obs <- MARSS(y, model = model.list)

##------process error only model---
#similar to autoregressive but of the model of the data and not observation
R <- A <- U <- "zero"
B <- Z <- "identity"
Q <- "equalvarcov"
C <- "unconstrained"
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   C = C, c = Cov_t)
mod_proc <- MARSS(SSD_Upp_t, model = model.list)

##-----process error only but data fluctuating around a mean ---
#autoregressive model -- a mean-reverting model 
#make diagonal element of B to something other than 1
model.list$B <- "diagonal and unequal"
mod_proc_mn <- MARSS(SSD_Upp_t, model = model.list)

## -----both process and observation error --
#covariates only affecting process
#first 225 observations are NA and have to remove to make x0 "unequal" and initialize x at 1
#which allows for R to be fully determined by data

SSD_Upp_t2 <- SSD_Upp_t[,-c(1:225)]
Cov_t2 <- Cov_t[,-c(1:225)]

D <- d <- A <- U <- "zero"
Z <- "identity"
B <- "diagonal and unequal"
Q <- "equalvarcov"
C <- "unconstrained"
c <- Cov_t2
R <- diag(0.16, 2)
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
mod_proc_obs <- MARSS(SSD_Upp_t2, model = model.list)

##-----get AIC and loglik into a data frame --
#AIC table
Mod_AIC <- c(mod_obs$AIC, mod_proc$AIC, mod_proc_mn$AIC, mod_proc_obs$AIC)
del_AIC <- Mod_AIC- min(Mod_AIC)  
relLik <- exp(-0.5 * del_AIC)
aicweight <- relLik/sum(relLik)

aic.table <- data.frame(AIC = Mod_AIC, delAIC = del_AIC, relLik = relLik,
                        weight = aicweight)
rownames(aic.table) <- c("Observation", "Process", "Process Mean", "Process Mean Observation" )
aic.table <- round(aic.table, digits = 2)

write.csv(aic.table, "Tables/AICTable_SSD_UpperLoc.csv")

##-----model coefficients for most likely model ---
mod_proc_mnC <- coef(mod_proc_mn, type = "matrix")$C
colnames(mod_proc_mnC) <- c("Precip", "Temp", "Diversion", "Return", "Discharge")

##-----diagnostics ---
#MARSS residuals
resids <- MARSSresiduals(mod_proc_mn)

par(mfrow = c(1,2))
plot(resids$model.residuals[1, ],
     ylab = "model residual", xlab = "", main = "process mean")
plot(resids$state.residuals[1, ],
     ylab = "state residual", xlab = "", main = "process mean")

#Smoothation residuals for outliers
resids_sm <- residuals(mod_proc_mn, type="tT", standardization="marginal")
mresids <- subset(resids_sm, name == "model")

plot(mresids)

par(mfrow = c(2,1))
plot(mresids$t, mresids$.std.resids,
     ylab = "model std smoothationl", xlab = "", main = "data outliers")
abline(h = 0); abline(h = c(2,-2), lty=2)
sresids <- subset(resids_sm, name == "state")
plot(sresids$t, sresids$.std.resids, type="l",
     ylab = "state std smoothation", xlab = "", main = "sudden level changes")
abline(h = 0); abline(h = c(2,-2), lty=2)

#acf - innovation residuals
par(mfrow = c(1,1))
acf(resids_sm$.resids, main = "process mean", na.action = na.pass)

##-----view results ---
#process with mean 
pl_mod_output <- as.data.frame(t(mod_proc_mn$states)) %>% 
  rename(SumDryDays = 1, UpperLoc = 2)
pl_dates_model <- cbind(R1_Trans_red$RiverReach1_Date, pl_mod_output) %>% 
  rename(RiverReach1_Date = 1)

SSD_plot <- ggplot(data = R1_Trans_red, aes(x = RiverReach1_Date, y = scaled_log_SumDaysDry))+
  geom_line(size = 1)+
  geom_point(data = pl_dates_model, aes(x = RiverReach1_Date, y = SumDryDays), 
                                   color = "red", size = 0.3)+
  theme_classic()+
  ggtitle("Best model prediction: process mean - covariates in process only")+
  xlab("") + ylab("Sum of dry days")+
  theme(legend.position = "none")

UpperLoc_plot <- ggplot(data = R1_Trans_red, aes(x = RiverReach1_Date, y = scaled_log_UpperLocationDry))+
  geom_line(size = 1)+
  geom_point(data = pl_dates_model, aes(x = RiverReach1_Date, y = UpperLoc), color = "orange", size = 0.3)+
  theme_classic()+
  ggtitle("")+
  xlab("") + ylab("Upper location dry")+
  theme(legend.position = "none")

pl1 <- gridExtra::grid.arrange(SSD_plot, UpperLoc_plot)
ggsave(plot = pl1, filename = "Figures/ModelSSD_UpperLoc.jpg", units = "in", width = 8, height = 9, dpi = 600)

#save the model output files 
saveRDS(mod_obs, file = "ModelOutput/AllCov_Observ.RData")
saveRDS(mod_proc, file = "ModelOutput/AllCov_Process.RData")
saveRDS(mod_proc_mn, file = "ModelOutput/AllCov_Process_Mean.RData")
saveRDS(mod_proc_obs, file = "ModelOutput/AllCov_Process_Obs.RData")

#Sum Days Dry, Extent, ChangeExtent with all covariates ####
##----------view the data
labels_pl <- as_labeller(c("A" = "SumDays", "B" = "UpperLoc", "C" = "ChngExtent",
                        "D" = "Precip", "E" = "Temp", "F" = "Diversion", 
                        "G" = "Returns", "H" = "Discharge"))

Raw_pl <- R1_Trans_red %>% 
  select(RiverReach1_Date, scaled_log_SumDaysDry, scaled_log_UpperLocationDry, scaled_log_AbsChngExtentDry,
         scaled_Precip_LosLunas2_mm, scaled_TempMax_LosLunas2_C,
         scaled_Diversion_Isleta_Totalcfs, scaled_Returns_Totalcfs,
         scaled_RiverDischarge_BosqueFarms_cfs) %>% 
  rename(Date = 1, A = 2, B = 3, C = 4, D = 5, E = 6, F = 7,
         G = 8, H = 9) %>% 
  pivot_longer(cols = A:H, names_to = "variables",values_to = "values") %>% 
  ggplot(aes(x = Date, y = values))+
  geom_line()+
  facet_grid(rows = vars(variables), labeller = labels_pl, scales = "free_y")+
  theme_bw()

ggsave(plot = Raw_pl, filename = "Figures/NormalizedResponseCovar.jpg", units = "in", width = 8, height = 10, dpi = 600)

##------convert data to matrix form ---
#need SSD, Upper, Chng and covariates as transposed number
SSD_Upp_t <- t(cbind(R1_Trans_red$scaled_log_SumDaysDry, R1_Trans_red$scaled_log_UpperLocationDry,
                     R1_Trans_red$scaled_log_AbsChngExtentDry))
rownames(SSD_Upp_t) <- c("SSD", "UpperLoc", "ChangeExtent")

Cov_t <- t(cbind(R1_Trans_red$scaled_Precip_LosLunas2_mm, R1_Trans_red$scaled_TempMax_LosLunas2_C,
                 R1_Trans_red$scaled_Diversion_Isleta_Totalcfs, R1_Trans_red$scaled_Returns_Totalcfs,
                 R1_Trans_red$scaled_RiverDischarge_BosqueFarms_cfs))
rownames(Cov_t) <- c("Precip", "Temp", "Diversion", "Return", "Discharge")

##------observation error only model---
#this is the same as an lm() with the A at zero because demeaned with scaling
Q <- U <- x0 <- "zero"
B <- Z <- "identity"
d <- Cov_t
A <- "zero" 
D <- "unconstrained"
y <- SSD_Upp_t  # to show relationship between dat & the equation
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, D = D, 
                   d = d, x0 = x0)
mod_obs <- MARSS(y, model = model.list)

##------process error only model---
#similar to autoregressive but of the model of the data and not observation
R <- A <- U <- "zero"
B <- Z <- "identity"
Q <- "equalvarcov"
C <- "unconstrained"
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   C = C, c = Cov_t)
mod_proc <- MARSS(SSD_Upp_t, model = model.list)

##-----process error only but data fluctuating around a mean ---
#autoregressive model -- a mean-reverting model 
#make diagonal element of B to something other than 1
model.list$B <- "diagonal and unequal"
mod_proc_mn <- MARSS(SSD_Upp_t, model = model.list)

## -----both process and observation error --
#covariates only affecting process
#first 225 observations are NA and have to remove to make x0 "unequal" and initialize x at 1
#which allows for R to be fully determined by data

SSD_Upp_t2 <- SSD_Upp_t[,-c(1:225)]
Cov_t2 <- Cov_t[,-c(1:225)]

D <- d <- A <- U <- "zero"
Z <- "identity"
B <- "diagonal and unequal"
Q <- "equalvarcov"
C <- "unconstrained"
c <- Cov_t2
R <- diag(0.16, 3)
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
mod_proc_obs <- MARSS(SSD_Upp_t2, model = model.list)

##-----get AIC and loglik into a data frame --
#AIC table
Mod_AIC <- c(mod_obs$AIC, mod_proc$AIC, mod_proc_mn$AIC, mod_proc_obs$AIC)
del_AIC <- Mod_AIC- min(Mod_AIC)  
relLik <- exp(-0.5 * del_AIC)
aicweight <- relLik/sum(relLik)

aic.table <- data.frame(AIC = Mod_AIC, delAIC = del_AIC, relLik = relLik,
                        weight = aicweight)
rownames(aic.table) <- c("Observation", "Process", "Process Mean", "Process Mean Observation" )
aic.table <- round(aic.table, digits = 2)

write.csv(aic.table, "Tables/AICTable_SSD_UpperLoc_ExtentChhng.csv")

##-----model coefficients for most likely model ---
mod_proc_mnC <- coef(mod_proc_mn, type = "matrix")$C
colnames(mod_proc_mnC) <- c("Precip", "Temp", "Diversion", "Return", "Discharge")

##-----diagnostics ---
#MARSS residuals
resids <- MARSSresiduals(mod_proc_mn)

par(mfrow = c(1,2))
plot(resids$model.residuals[1, ],
     ylab = "model residual", xlab = "", main = "process mean")
plot(resids$state.residuals[1, ],
     ylab = "state residual", xlab = "", main = "process mean")

#Smoothation residuals for outliers
resids_sm <- residuals(mod_proc_mn, type="tT", standardization="marginal")
mresids <- subset(resids_sm, name == "model")

plot(mresids)

resids_inov <- residuals(mod_proc_mn, type = "tt1", standardization="marginal")
plot(subset(resids_inov, name == "model"))

par(mfrow = c(2,1))
plot(mresids$t, mresids$.std.resids,
     ylab = "model std smoothationl", xlab = "", main = "data outliers")
abline(h = 0); abline(h = c(2,-2), lty=2)
sresids <- subset(resids_sm, name == "state")
plot(sresids$t, sresids$.std.resids, type="l",
     ylab = "state std smoothation", xlab = "", main = "sudden level changes")
abline(h = 0); abline(h = c(2,-2), lty=2)

#acf - innovation residuals
par(mfrow = c(1,1))
acf(resids_sm$.resids, main = "process mean", na.action = na.pass)
plot(resids_sm$.resids)

##-----view results ---
#process with mean 
pl_mod_output <- as.data.frame(t(mod_proc_mn$states)) %>% 
  rename(SumDryDays = 1, UpperLoc = 2, ChngExtent = 3)

pl_dates_model <- cbind(R1_Trans_red$RiverReach1_Date, pl_mod_output) %>% 
  rename(RiverReach1_Date = 1)

SSD_plot <- ggplot(data = R1_Trans_red, aes(x = RiverReach1_Date, y = scaled_log_SumDaysDry))+
  geom_line(size = 1)+
  geom_point(data = pl_dates_model, aes(x = RiverReach1_Date, y = SumDryDays), 
             color = "red", size = 0.3)+
  theme_classic()+
  ggtitle("Best model prediction: process mean - covariates in process only")+
  xlab("") + ylab("Sum of dry days")+
  theme(legend.position = "none")

UpperLoc_plot <- ggplot(data = R1_Trans_red, aes(x = RiverReach1_Date, y = scaled_log_UpperLocationDry))+
  geom_line(size = 1)+
  geom_point(data = pl_dates_model, aes(x = RiverReach1_Date, y = UpperLoc), color = "orange", size = 0.3)+
  theme_classic()+
  ggtitle("")+
  xlab("") + ylab("Upper location dry")+
  theme(legend.position = "none")

ChngExtent_plot <- ggplot(data = R1_Trans_red, aes(x = RiverReach1_Date, y = scaled_log_UpperLocationDry))+
  geom_line(size = 1)+
  geom_point(data = pl_dates_model, aes(x = RiverReach1_Date, y = ChngExtent), color = "dark grey", size = 0.3)+
  theme_classic()+
  ggtitle("")+
  xlab("") + ylab("Change extent dry")+
  theme(legend.position = "none")

pl1 <- gridExtra::grid.arrange(SSD_plot, UpperLoc_plot, ChngExtent_plot)
ggsave(plot = pl1, filename = "Figures/ModelSSD_UpperLoc_ChngExtent.jpg", units = "in", width = 8, height = 9, dpi = 600)

#save the model output files 
saveRDS(mod_obs, file = "ModelOutput/AllRespAllCov_Observ.RData")
saveRDS(mod_proc, file = "ModelOutput/AllRespAllCov_Process.RData")
saveRDS(mod_proc_mn, file = "ModelOutput/AllRespAllCov_Process_Mean.RData")
saveRDS(mod_proc_obs, file = "ModelOutput/AllRespAllCov_Process_Obs.RData")

mod_proc_mn <- readRDS("ModelOutput/AllRespAllCov_Process_Mean.RData")
