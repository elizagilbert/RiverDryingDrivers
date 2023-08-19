#read Me ####
#The purpose of this script is to assess the spatial structure of Rio Grande drying
#at two reach levels (drying and diversion)

#library ####
library(MARSS)
library(tidyverse)
library(lubridate)
library(zoo)
library(beepr)
library(forecast)

#read predictor data ####
#testing model output of log transformations with and without 0s as NAs
lambda1 <- BoxCox.lambda(dat_DryR$Extent)

dat_DryR <- read.csv("Data/Processed/DryingSubreachData.csv", header = T) %>% 
  select(Date, Extent, ExtentChng, Reach) %>% 
  mutate(Extent2 = case_when(ExtentChng > 5 ~ NA_real_,
                             ExtentChng < -5 ~ NA_real_,
                             TRUE ~ Extent)) %>% 
  mutate(LogExtent = log(Extent2 + 1.01)) %>% 
  mutate(BoxExtent = BoxCox(Extent, lambda1))

lambda2 <- BoxCox.lambda(dat_DivR$Extent)
dat_DivR <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T) %>% 
  select(Date, Extent, ExtentChng,Reach) %>% 
  mutate(Extent2 = case_when(ExtentChng > 5 ~ NA_real_,
                             ExtentChng < -5 ~ NA_real_,
                             TRUE ~ Extent))%>% 
  mutate(LogExtent = log(Extent + 1.01))%>% 
  mutate(BoxExtent = BoxCox(Extent, lambda2))

sum(is.na(dat_DivR$Extent2))
sum(is.na(dat_DryR$Extent2))

#response distribution plots #####
dat_DryR %>% 
  group_by(Reach) %>% 
  mutate(zLogExtent = (LogExtent - mean(LogExtent, na.rm = TRUE))/ sd(LogExtent, na.rm = TRUE)) %>% 
  mutate(zExtent = (Extent2 - mean(Extent2, na.rm = TRUE))/ sd(Extent2, na.rm = TRUE)) %>% 
  ggplot(aes(zExtent))+
  geom_histogram()+
  facet_wrap(vars(Reach), scales = "free_x")

dat_DivR %>% 
  group_by(Reach) %>% 
  mutate(zLogExtent = scale(LogExtent)) %>% 
  mutate(zExtent = (Extent2 - mean(Extent2, na.rm = TRUE))/ sd(Extent2, na.rm = TRUE)) %>% 
  ggplot(aes(zExtent))+
  geom_histogram()+
  facet_wrap(vars(Reach))

#covariates ####
  #already zscored and reduced to irrigation season (months 4-10)
cov_file_list <- list.files("Data/Processed/MARSS_Covariates/Temp", full.names = T, pattern = "*.csv")
all_cov_data <- lapply(cov_file_list, function(file){
  df <- read.csv(file)
})
names(all_cov_data) <- sub('\\.csv', '', basename(cov_file_list))
all_cov_data <- lapply(all_cov_data, function(x) column_to_rownames(x, var = "X"))
all_cov_matrix <- lapply(all_cov_data, function(x) as.matrix(x))

  #check z-scoring
apply(all_cov_matrix$Pred_Div_2states, 1, var)

#response ####

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
Extent_DryR1 <- predictor_func(dat_DryR, Extent2)
Extent_DryR2 <- predictor_func(dat_DryR, LogExtent)

  #diversion reaches
Extent_DivR1 <- predictor_func(dat_DivR, Extent2)
Extent_DivR2 <- predictor_func(dat_DivR, LogExtent)

#creating a time series of first predictive variable
Extent_DryR_ts <- ts(Extent_DivR1, frequency = 365)
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
  #3 states dry
moddry_3states_qdiaeq <- list(B = "diagonal and equal", U = matrix(0,3,1), Q = "diagonal and equal",
                        c=all_cov_matrix$Pred_Dry_3statesReduced, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                        R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 states dry
moddry_2states_qdiaeq <- list(B = "diagonal and equal", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$Pred_Dry_2statesReduced, C=C_2states, Z = Z_2states, A = matrix(0,3,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #2 states diversion
moddiv_2states_qdiaeq <- list(B = "diagonal and equal", U = matrix(0,2,1), Q = "diagonal and equal",
                              c=all_cov_matrix$Pred_Div_2statesReduced, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                              R = "diagonal and equal", x0 = "equal", tinitx = 0)

  #1 state
mod_1state_qdiaeq <- list(B = "diagonal and equal", U = matrix(0,1,1), Q = "diagonal and equal",
                    c=all_cov_matrix$Pred_Dry_1stateReduced, C=C_1state, Z = matrix(1,3,1), A = matrix(0,3,1), 
                    R = "diagonal and equal", x0 = "equal", tinitx = 0)



#model fits ####

start.time <- Sys.time()
#3 states Extent

Extent_3states_dry <- MARSS(y = Extent_DryR1, model = moddry_3states_qdiaeq, 
                                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                  conv.test.slope.tol = 0.09), fit = T) 
  Extent_3states_dry_BFGS <- MARSS(y = Extent_DryR1, model = moddry_3states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = Extent_3states_dry$par)


#2 states Extent
Extent_2states_dry <- MARSS(Extent_DryR1, model = moddry_2states_qdiaeq, 
                                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                  conv.test.slope.tol = 0.09),fit = T) 
  Extent_2states_dry_BFGS <- MARSS(y = Extent_DryR1, model = moddry_2states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = Extent_2states_dry$par)

Extent_2states_div <- MARSS(Extent_DivR1, model = moddiv_2states_qdiaeq, 
                                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                  conv.test.slope.tol = 0.09),fit = T) 
  Extent_2states_div_BFGS <- MARSS(y = Extent_DivR1, model = moddiv_2states_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = Extent_2states_div$par)

#1 state Extent
Extent_1state <- MARSS(Extent_DryR1, model = mod_1state_qdiaeq, 
                                 control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                conv.test.slope.tol = 0.09),fit = T) 
  Extent_1state_BFGS <- MARSS(y = Extent_DryR1, model = mod_1state_qdiaeq, control = list(maxit = 5000), 
                                 method = "BFGS", inits = Extent_1state$par)

beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))

#residuals ####
autoplot.marssMLE(Extent_3states_dry_BFGS) #logoffset residuals bad, but acf not totally horrible
autoplot.marssMLE(Extent_2states_dry_BFGS) #logoffset residuals bad, and acf horrible
autoplot.marssMLE(Extent_2states_div_BFGS) #logoffset residuals bad, but acf not bad at all 
                                           #slightly better than raw, acf bettern than when -5 to 5 are included
                                           #BoxCox transformation no good
autoplot.marssMLE(Extent_1state_BFGS)      #logoffset residuals not as bad, acf horrible

#save and read models ####
saveRDS(Extent_3states_dry_BFGS, "ModelOutput/Extent/Extent_3states_dry_BFGS.rds") # 
saveRDS(Extent_2states_dry_BFGS, "ModelOutput/Extent/Extent_2states_dry_BFGS.rds") # 
saveRDS(Extent_2states_div_BFGS, "ModelOutput/Extent/Extent_2states_div_BFGS.rds") # 
saveRDS(Extent_1state_BFGS, "ModelOutput/Extent/Extent_1state_BFGS.rds") # 

Extent_3states_dry_BFGS <- readRDS("ModelOutput/Extent/Extent_3states_dry_BFGS.rds")
Extent_2states_dry_BFGS <- readRDS("ModelOutput/Extent/Extent_2states_dry_BFGS.rds")
Extent_2states_div_BFGS <- readRDS("ModelOutput/Extent/Extent_2states_div_BFGS.rds")
Extent_1state_BFGS <- readRDS("ModelOutput/Extent/Extent_1state_BFGS.rds")

#residuals ####
autoplot.marssMLE(Extent_2states_div_BFGS)

plot(Extent_2states_div_BFGS) #this give a slightly different picture than autoplot
Preds <- predict(Extent_2states_div_BFGS)$pred %>% 
  mutate(Resid = y - estimate) 

write.csv(Preds, "Data/Processed/PredsExtDiv.csv", row.names = F)






#AIC ####
Extent_AIC <- c(Extent_3states_dry_BFGS$AICc,
                Extent_2states_dry_BFGS$AICc, 
                Extent_2states_div_BFGS$AICc,  
                Extent_1state_BFGS$AICc)

ExtDelAIC <- Extent_AIC - min(Extent_AIC)
ExtRelLik <- exp(-0.5 * ExtDelAIC)
ExtAICWeight <- ExtRelLik/sum(ExtRelLik)
ExtAICTable <- data.frame(AICc = Extent_AIC, delAIC = ExtDelAIC, relLike = ExtRelLik,
                          weight = ExtAICWeight)
rownames(ExtAICTable) <- c("3dry_diaeq", 
                           "2dry_diaeq", 
                           "2div_diaeq", 
                           "1dry_diaeq")
ExtAICTable %>% mutate(across(where(is.numeric),round,0)) %>% arrange(delAIC)


#model output  ####
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

MARSSparamCIs(Extent_2states_div_BFGS) #tidy.marssMLE does the same thing
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






