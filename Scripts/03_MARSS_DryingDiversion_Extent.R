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
#did not replace 0s with NA and did not use log transform in models because nonconvergence issues
dat_DryR <- read.csv("Data/Processed/DryingSubreachData.csv", header = T) 
dat_DivR <- read.csv("Data/Processed/DiversionSubreachData.csv", header = T) 

#Response distribution plots #####
dat_DryR %>% 
  select(Date, Extent, Reach) %>% 
  group_by(Reach) %>% 
  mutate(LogExtent = log(Extent)) %>% 
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
Extent_DryR <- predictor_func(dat_DryR, Extent)
 #diversion reaches
Extent_DivR <- predictor_func(dat_DivR, Extent)

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
moddry_3states_qdiauneq <- list(B = "identity", U = matrix(0,3,1), Q = "diagonal and unequal",
                           c=all_cov_matrix$Pred_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                           R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_3states_qdiavarcov <- list(B = "identity", U = matrix(0,3,1), Q = "equalvarcov",
                           c=all_cov_matrix$Pred_Dry_3states, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                           R = "diagonal and equal", x0 = "equal", tinitx = 0)
moddry_3states_qdiauncon <- list(B = "identity", U = matrix(0,3,1), Q = "unconstrained",
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
moddry_null_3states_qdiauncon <- list(B = "identity", U = matrix(0,3,1), Q = "unconstrained",
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
moddry_2states_qdiauncon <- list(B = "identity", U = matrix(0,2,1), Q = "unconstrained",
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
moddry_null_2states_qdiauncon <- list(B = "identity", U = matrix(0,2,1), Q = "unconstrained",
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
moddiv_2states_qdiauncon <- list(B = "identity", U = matrix(0,2,1), Q = "unconstrained",
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
moddiv_null_2states_qdiauncon <- list(B = "identity", U = matrix(0,2,1), Q = "unconstrained",
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

#3 states Extent
start.time <- Sys.time()
Extent_3states_dry_qdiaeq <- MARSS(Extent_DryR, model = moddry_3states_qdiaeq, control = list(conv.test.slope.tol = 0.09))
Extent_3states_dry_diauneq <- MARSS(Extent_DryR, model = moddry_3states_qdiauneq, control = list(conv.test.slope.tol = 0.09))
Extent_3states_dry_diavarcov <- MARSS(Extent_DryR, model = moddry_3states_qdiavarcov, control = list(conv.test.slope.tol = 0.09))
Extent_3states_dry_diauncon <- MARSS(Extent_DryR, model = moddry_3states_qdiauncon, control = list(conv.test.slope.tol = 0.09))


Extent_null_3states_dry_qdiaeq <- MARSS(Extent_DryR, model = moddry_null_3states_qdiaeq, control = list(conv.test.slope.tol = 0.09))
Extent_null_3states_dry_diauneq <- MARSS(Extent_DryR, model = moddry_null_3states_qdiauneq, control = list(conv.test.slope.tol = 0.09))
Extent_null_3states_dry_diavarcov <- MARSS(Extent_DryR, model = moddry_null_3states_qdiavarcov, control = list(conv.test.slope.tol = 0.09))
Extent_null_3states_dry_diauncon <- MARSS(Extent_DryR, model = moddry_null_3states_qdiauncon, control = list(conv.test.slope.tol = 0.09))

#2 states Extent
Extent_2states_dry_qdiaeq <- MARSS(Extent_DryR, model = moddry_2states_qdiaeq, control = list(conv.test.slope.tol = 0.09))
Extent_2states_dry_diauneq <- MARSS(Extent_DryR, model = moddry_2states_qdiauneq, control = list(conv.test.slope.tol = 0.09))
Extent_2states_dry_diavarcov <- MARSS(Extent_DryR, model = moddry_2states_qdiavarcov, control = list(conv.test.slope.tol = 0.09))
Extent_2states_dry_diauncon <- MARSS(Extent_DryR, model = moddry_2states_qdiauncon, control = list(conv.test.slope.tol = 0.09))


Extent_null_2states_dry_qdiaeq <- MARSS(Extent_DryR, model = moddry_null_2states_qdiaeq, control = list(conv.test.slope.tol = 0.09))
Extent_null_2states_dry_diauneq <- MARSS(Extent_DryR, model = moddry_null_2states_qdiauneq, control = list(conv.test.slope.tol = 0.09))
Extent_null_2states_dry_diavarcov <- MARSS(Extent_DryR, model = moddry_null_2states_qdiavarcov, control = list(conv.test.slope.tol = 0.09))
Extent_null_2states_dry_diauncon <- MARSS(Extent_DryR, model = moddry_null_2states_qdiauncon, control = list(conv.test.slope.tol = 0.09))

Extent_2states_div_qdiaeq <- MARSS(Extent_DivR, model = moddiv_2states_qdiaeq, control = list(conv.test.slope.tol = 0.09))
Extent_2states_div_diauneq <- MARSS(Extent_DivR, model = moddiv_2states_qdiauneq, control = list(conv.test.slope.tol = 0.09))
Extent_2states_div_diavarcov <- MARSS(Extent_DivR, model = moddiv_2states_qdiavarcov, control = list(conv.test.slope.tol = 0.09))
Extent_2states_div_diauncon <- MARSS(Extent_DivR, model = moddiv_2states_qdiauncon, control = list(conv.test.slope.tol = 0.09))

Extent_null_2states_div_qdiaeq <- MARSS(Extent_DivR, model = moddiv_null_2states_qdiaeq, control = list(conv.test.slope.tol = 0.09))
Extent_null_2states_div_diauneq <- MARSS(Extent_DivR, model = moddiv_null_2states_qdiauneq, control = list(conv.test.slope.tol = 0.09))
Extent_null_2states_div_diavarcov <- MARSS(Extent_DivR, model = moddiv_null_2states_qdiavarcov, control = list(conv.test.slope.tol = 0.09))
Extent_null_2states_div_diauncon <- MARSS(Extent_DivR, model = moddiv_null_2states_qdiauncon, control = list(conv.test.slope.tol = 0.09))

#1 states Extent
Extent_1state_dry_diaeq <- MARSS(Extent_DryR, model = moddry_1state_qdiaeq, control = list(conv.test.slope.tol = 0.09))
Extent_null_1state_dry_diaeq <- MARSS(Extent_DryR, model = moddry_null_1state_qdiaeq, control = list(conv.test.slope.tol = 0.09))


Extent_1state_div_diaeq <- MARSS(Extent_DivR, model = moddiv_1state_qdiaeq, control = list(conv.test.slope.tol = 0.09))
Extent_null_1state_div_diaeq <- MARSS(Extent_DivR, model = moddiv_null_1state_qdiaeq, control = list(conv.test.slope.tol = 0.09))
beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))

#AIC ####
Extent_AIC <- c(Extent_3states_dry_qdiaeq$AICc, Extent_3states_dry_diauneq$AICc, Extent_3states_dry_diavarcov$AICc, Extent_3states_dry_diauncon$AICc,
                Extent_null_3states_dry_qdiaeq$AICc, Extent_null_3states_dry_diauneq$AICc, Extent_null_3states_dry_diavarcov$AICc, Extent_null_3states_dry_diauncon$AICc,
                Extent_2states_dry_qdiaeq$AICc, Extent_2states_dry_diauneq$AICc, Extent_2states_dry_diavarcov$AICc, Extent_2states_dry_diauncon$AICc,
                Extent_null_2states_dry_qdiaeq$AICc, Extent_null_2states_dry_diauneq$AICc, Extent_null_2states_dry_diavarcov$AICc, Extent_null_2states_dry_diauncon$AICc,
                Extent_1state_dry_diaeq$AICc,
                Extent_null_1state_dry_diaeq$AICc,
                Extent_2states_div_qdiaeq$AICc, Extent_2states_div_diauneq$AICc, Extent_2states_div_diavarcov$AICc, Extent_2states_div_diauncon$AICc,
                Extent_null_2states_div_qdiaeq$AICc, Extent_null_2states_div_diauneq$AICc, Extent_null_2states_div_diavarcov$AICc, Extent_null_2states_div_diauncon$AICc,
                Extent_1state_div_diaeq$AICc,
                Extent_null_1state_div_diaeq$AICc)

ExtDelAIC <- Extent_AIC - min(Extent_AIC)
ExtRelLik <- exp(-0.5 * ExtDelAIC)
ExtAICWeight <- ExtRelLik/sum(ExtRelLik)
ExtAICTable <- data.frame(AICc = Extent_AIC, delAIC = ExtDelAIC, relLike = ExtRelLik,
                          weight = ExtAICWeight)
rownames(ExtAICTable) <- c("3dry_diaeq", "3dry_diuneq", "3dry_varcov", "3dry_uncon",
                           "3dry_null_diaeq", "3dry_null_diuneq", "3dry_null_varcov", "3dry_null_uncon",
                           "2dry_diaeq", "2dry_diuneq", "2dry_varcov", "2dry_uncon",
                           "2dry_null_diaeq", "2dry_null_diuneq", "2dry_null_varcov", "2dry_null_uncon",
                           "1dry",
                           "1dry_null",
                           "2div_diaeq", "2div_diuneq", "2div_varcov", "2div_uncon",
                           "2div_null_diaeq", "2div_null_diuneq", "2div_null_varcov", "2div_null_uncon",
                           "1div",
                           "1div_null")
ExtAICTable %>% mutate(across(where(is.numeric),round,0)) %>% arrange(delAIC)

#refit top model with BFGS ####
BFGSfit_TopExtent <- MARSS(y = Extent_DivR, model = moddiv_2states_qdiauncon,
                           control = list(maxit = 5000), method = "BFGS", inits=Extent_2states_div_diauncon$par)
#save and read top model ####
saveRDS(BFGSfit_TopExtent, "ModelOutput/TopExtentMod_BFGS.rds")
mod <- readRDS("ModelOutput/TopExtentMod_BFGS.rds")

#model output  ####
summary(mod)
MARSSparamCIs(mod) #tidy.marssMLE does the same thing
fitted(mod) #data and state fitted values (predictions)
preds <- predict(mod,
                     interval = "confidence",
                     se.fit = TRUE) #another way to get estimate and CIs
#plotting
conf_marss1 <- fitted(mod, type = "ytT", interval = "confidence")
pred_marss1 <- fitted(mod, type = "ytT", interval = "prediction")
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


#Residuals ####
autoplot.marssMLE(BFGSfit_TopExtent)
plot(BFGSfit_TopExtent) #this give a slightly different picture than autoplot
predict(BFGSfit_TopExtent)
plot(BFGSfit_TopExtent, plot.type="model.resids.ytT") #smoothations model residuals as opposed to innovation 



