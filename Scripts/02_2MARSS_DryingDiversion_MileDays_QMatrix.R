#read Me ####
#The purpose of this script is to assess the best Q structure for the two models
#which were 3 independent states of drying and 2 independent states of diversions
#tested diagonal and equal, diagonal and unequal, and unconstrained,
#did not test equal variance covariance because I could not run the BGFS with it

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


#model lists ####

#3 states dry 
moddry_3statescum_diaeq <- list(B = "diagonal and equal", U = matrix(0,3,1), Q = "diagonal and equal",
                                 c=all_cov_matrix$PredCum_Dry_3statesReduced, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)

moddry_3statescum_diauneq <- list(B = "diagonal and equal", U = matrix(0,3,1), Q = "diagonal and unequal",
                                c=all_cov_matrix$PredCum_Dry_3statesReduced, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)

moddry_3statescum_uncon <- list(B = "diagonal and equal", U = matrix(0,3,1), Q = "unconstrained",
                                c=all_cov_matrix$PredCum_Dry_3statesReduced, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)

moddry_3statescum_varcov <- list(B = "diagonal and equal", U = matrix(0,3,1), Q = "equalvarcov",
                                c=all_cov_matrix$PredCum_Dry_3statesReduced, C=C_3states, Z = "identity", A = matrix(0,3,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)


#2 states diversion 
moddiv_2statescum_diaeq <- list(B = "diagonal and equal", U = matrix(0,2,1), Q = "diagonal and equal",
                                 c=all_cov_matrix$PredCum_Div_2statesReduced, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                                 R = "diagonal and equal", x0 = "equal", tinitx = 0)


moddiv_2statescum_diauneq <- list(B = "diagonal and equal", U = matrix(0,2,1), Q = "diagonal and unequal",
                                c=all_cov_matrix$PredCum_Div_2statesReduced, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)

moddiv_2statescum_uncon <- list(B = "diagonal and equal", U = matrix(0,2,1), Q = "unconstrained",
                                c=all_cov_matrix$PredCum_Div_2statesReduced, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)

moddiv_2statescum_varcov <- list(B = "diagonal and equal", U = matrix(0,2,1), Q = "equalvarcov",
                                c=all_cov_matrix$PredCum_Div_2statesReduced, C=C_2states, Z = "identity", A = matrix(0,2,1), 
                                R = "diagonal and equal", x0 = "equal", tinitx = 0)
#model fits ####

start.time <- Sys.time()
#3 states MileDays dry

MD_3states_dry_diaeq <- MARSS(MileDays_DryR, model = moddry_3statescum_diaeq, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
  MD_3states_dry_diaeq_BFGS <- MARSS(y = MileDays_DryR, model = moddry_3statescum_diaeq, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_3states_dry_diaeq$par) 

MD_3states_dry_diauneq <- MARSS(MileDays_DryR, model = moddry_3statescum_diauneq, 
                                control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                               conv.test.slope.tol = 0.09), fit = T) 
  MD_3states_dry_diauneq_BFGS <- MARSS(y = MileDays_DryR, model = moddry_3statescum_diauneq, control = list(maxit = 5000), 
                                     method = "BFGS", inits = MD_3states_dry_diauneq$par) 

MD_3states_dry_uncon <- MARSS(MileDays_DryR, model = moddry_3statescum_uncon, 
                                  control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                 conv.test.slope.tol = 0.09), fit = T) 
  MD_3states_dry_uncon_BFGS <- MARSS(y = MileDays_DryR, model = moddry_3statescum_uncon, control = list(maxit = 5000), 
                                       method = "BFGS", inits = MD_3states_dry_uncon$par) 
  
  #did not use BGFS because there is an error that says Q is not properly constrained 
MD_3states_dry_varcov <- MARSS(MileDays_DryR, model = moddry_3statescum_varcov, 
                                  control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                 conv.test.slope.tol = 0.09), fit = T) 

#2 states MileDays diversion
MD_2states_div_diaeq <- MARSS(MileDays_DivR, model = moddiv_2statescum_diaeq, 
                        control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                       conv.test.slope.tol = 0.09), fit = T) 
  MD_2states_div_diaeq_BFGS <- MARSS(y = MileDays_DivR, model = moddiv_2statescum_diaeq, control = list(maxit = 5000), 
                             method = "BFGS", inits = MD_2states_div_diaeq$par) 

MD_2states_div_diauneq <- MARSS(MileDays_DivR, model = moddiv_2statescum_diauneq, 
                                control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                               conv.test.slope.tol = 0.09), fit = T) 
  MD_2states_div_diauneq_BFGS <- MARSS(y = MileDays_DivR, model = moddiv_2statescum_diauneq, control = list(maxit = 5000), 
                                     method = "BFGS", inits = MD_2states_div_diauneq$par) 

MD_2states_div_uncon <- MARSS(MileDays_DivR, model = moddiv_2statescum_uncon, 
                                  control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                                 conv.test.slope.tol = 0.09), fit = T) 
  MD_2states_div_uncon_BFGS <- MARSS(y = MileDays_DivR, model = moddiv_2statescum_uncon, control = list(maxit = 5000), 
                                       method = "BFGS", inits = MD_2states_div_uncon$par) 

MD_2states_div_varcov <- MARSS(MileDays_DivR, model = moddiv_2statescum_varcov, 
                                control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                               conv.test.slope.tol = 0.09), fit = T) 
beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))

#save models ####
#did not save equal variance covariance because the model did not converge

saveRDS(MD_3states_dry_diaeq_BFGS, "ModelOutput/MileDays/MD_3states_dry_diaeq_BFGS.rds") 
saveRDS(MD_3states_dry_diauneq_BFGS, "ModelOutput/MileDays/MD_3states_dry_diauneq_BFGS.rds") 
saveRDS(MD_3states_dry_uncon_BFGS, "ModelOutput/MileDays/MD_3states_dry_uncon_BFGS.rds") 

saveRDS(MD_2states_div_diaeq_BFGS, "ModelOutput/MileDays/MD_2states_div_diaeq_BFGS.rds") 
saveRDS(MD_2states_div_diauneq_BFGS, "ModelOutput/MileDays/MD_2states_div_diauneq_BFGS.rds") 
saveRDS(MD_2states_div_uncon_BFGS, "ModelOutput/MileDays/MD_2states_div_uncon_BFGS.rds")

#read in model outputs ####
MD_3states_dry_diaeq_BFGS <- readRDS("ModelOutput/MileDays/MD_3states_dry_diaeq_BFGS.rds") 
MD_3states_dry_diauneq_BFGS <- readRDS("ModelOutput/MileDays/MD_3states_dry_diauneq_BFGS.rds") 
MD_3states_dry_uncon_BFGS <- readRDS("ModelOutput/MileDays/MD_3states_dry_uncon_BFGS.rds") 

MD_2states_div_diaeq_BFGS <- readRDS("ModelOutput/MileDays/MD_2states_div_diaeq_BFGS.rds") 
MD_2states_div_diauneq_BFGS <- readRDS("ModelOutput/MileDays/MD_2states_div_diauneq_BFGS.rds")
MD_2states_div_uncon_BFGS <- readRDS("ModelOutput/MileDays/MD_2states_div_uncon_BFGS.rds") 


#residuals ####
#raw data better than log offset
autoplot.marssMLE(MD_3states_dry_diaeq_BFGS)   # good, acf R2 a little out of bounds
autoplot.marssMLE(MD_3states_dry_diauneq_BFGS) # good, acf R2 a little out of bounds
autoplot.marssMLE(MD_3states_dry_uncon_BFGS)   # good, acf R3 a little out of bounds


autoplot.marssMLE(MD_2states_div_diaeq_BFGS)   # good, acf R1 a tiny bit out of bounds
autoplot.marssMLE(MD_2states_div_diauneq_BFGS) # good, acf R1 a tiny bit out of bounds
autoplot.marssMLE(MD_2states_div_uncon_BFGS)   # good

#AIC ####
  #drying
MD_AIC_Dry <- c(MD_3states_dry_diaeq_BFGS$AICc, 
            MD_3states_dry_diauneq_BFGS$AICc,
            MD_3states_dry_uncon_BFGS$AICc
)

ExtDelAIC_Dry <- MD_AIC_Dry - min(MD_AIC_Dry)
ExtRelLik_Dry <- exp(-0.5 * ExtDelAIC_Dry)
ExtAICWeight_Dry <- ExtRelLik_Dry/sum(ExtRelLik_Dry)
ExtAICTable_Dry <- data.frame(AICc = MD_AIC_Dry, delAICc = ExtDelAIC_Dry, relLike = ExtRelLik_Dry,
                          weight = ExtAICWeight_Dry)
rownames(ExtAICTable_Dry) <- c("MD_3states_dry_diaeq_BFGS", 
                           "MD_3states_dry_diauneq_BFGS",
                           "MD_3states_dry_uncon_BFGS"
)
ExtAICTable_Dry %>% mutate(across(where(is.numeric),round,0)) %>% arrange(delAICc)

  #diversion
MD_AIC_Div <- c(MD_2states_div_diaeq_BFGS$AICc, 
                MD_2states_div_diauneq_BFGS$AICc,
                MD_2states_div_uncon_BFGS$AICc
)

ExtDelAIC_Div <- MD_AIC_Div - min(MD_AIC_Div)
ExtRelLik_Div <- exp(-0.5 * ExtDelAIC_Div)
ExtAICWeight_Div <- ExtRelLik_Div/sum(ExtRelLik_Div)
ExtAICTable_Div <- data.frame(AICc = MD_AIC_Div, delAICc = ExtDelAIC_Div, relLike = ExtRelLik_Div,
                              weight = ExtAICWeight_Div)
rownames(ExtAICTable_Div) <- c("MD_2states_div_diaeq_BFGS", 
                               "MD_2states_div_diauneq_BFGS",
                               "MD_2states_div_uncon_BFGS"
)
ExtAICTable_Div %>% mutate(across(where(is.numeric),round,0)) %>% arrange(delAICc)

#model output####
summary(MD_2states_div_uncon_BFGS)
MARSSparamCIs(MD_2states_div_diauneq_BFGS)

summary(MD_3states_dry_uncon_BFGS)
MARSSparamCIs(MD_3states_dry_diauneq_BFGS) 
