#Read me ####
#The purpose of this script is to run MARSS models on 100
#randomly binned 10 sections of the Rio Grande

#Libraries ###
library(tidyverse)
library(MARSS)

#Load Data ####
load("Data/Processed/Random100/matrices_list_MD100.RData")
load("Data/Processed/Random100/matrices_list_Cov100.RData")

#check z-scoring ####
apply(matrices_list_Cov[[1]], 1, var)

#10 states - used 10 random intervals from the 22 river segments of 4.4 miles
C_10states <- matrix(0,10,50)  
diag10 <- function(x) {
  C_10dis <- matrix(list(0),10,10); diag(C_10dis) <- "dis"
  C_10div <-  matrix(list(0),10,10); diag(C_10div) <- "div"
  C_10precip <-  matrix(list(0),10,10); diag(C_10precip) <- "precip"
  C_10ret <-  matrix(list(0),10,10); diag(C_10ret) <- "ret"
  C_10temp <-  matrix(list(0),10,10); diag(C_10temp) <- "temp"
  C_10states <- cbind(C_10dis, C_10div,C_10precip, C_10ret, C_10temp)
  C_10states
}
C_10states <- diag10(C_10states)

#MARSS ####


# Define your modeling function
fit_multivariate_model <- function(df, covariates) {
  
  modlist_10states <- list(B = "diagonal and equal", U = matrix(0,10,1), Q = "diagonal and unequal",
                           c=covariates, C=C_10states, Z = "identity", A = matrix(0,10,1), 
                           R = "diagonal and equal", x0 = "equal", tinitx = 0)
  
  mod_temp<- MARSS(y = df, model = modlist_10states, 
                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                  conv.test.slope.tol = 0.09), fit = T) 
  mod_temp_BFGS <- MARSS(y = df, model = modlist_10states, control = list(maxit = 5000), 
                         method = "BFGS", inits = mod_temp$par)
}


# List of data frames
list_of_data_frames <- matrices_list_MD  

# List of covariates
list_of_covariates <- matrices_list_Cov  

# Loop through the lists and fit models
start.time <- Sys.time()
results_list <- lapply(1:length(list_of_data_frames), function(i) {
  df <- list_of_data_frames[[i]]
  covariates <- list_of_covariates[[i]]
  model <- fit_multivariate_model(df, covariates)  
  return(model)
})
end.time <- Sys.time()
print(round(end.time - start.time,2))

save(results_list, file = "ModelOutput/Random100/ResultsList100Samples.RData")
load("ModelOutput/Random100/ResultsList100Samples.RData")

#calculate mean and CIs ####
# load("ModelOutput/Random100/ResultsList5Samples.RData")

model_params <- lapply(results_list, MARSSparamCIs)
save(model_params, file = "ModelOutput/Random100/ModelParams100samples.RData")
load("ModelOutput/Random100/ModelParams100samples.RData")

model_coefs <- data.frame(lapply(model_params, function (x) `[`(x, c('coef'))))

para_mean_95CI <- model_coefs %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = coef:coef.4,
             names_to = "Parameter",
             values_to = "ML_est") %>% 
  group_by(rowname) %>% 
  summarise(mean.coef = mean(ML_est, na.rm = TRUE),
            sd.coef = sd(ML_est, na.rm = TRUE),
            n.coef = n()) %>%
  mutate(se.coef = sd.coef / sqrt(n.coef),
         lower.ci.ceof = mean.coef - qt(1 - (0.05 / 2), n.coef - 1) * se.coef,
         upper.ci.coef = mean.coef + qt(1 - (0.05 / 2), n.coef - 1) * se.coef)

temp1 <- para_mean_95CI %>% select(c(rowname, mean.coef)) %>%
  rename(Param = 1, MnCoef = 2) %>% 
  group_by(Param) %>% 
  summarise(mean = mean(MnCoef))
  
temp2 <- model_coefs %>% 
  rownames_to_column() %>% 
  rename(Coefficient = 1) %>% 
  pivot_longer(cols = coef:coef.54, names_to = "Coeff", values_to = "values") %>% 
  filter(str_detect(Coefficient, "C."))

temp2 %>% 
  filter(Coefficient == "C.ret") %>% 
  ggplot(aes(x=values)) + 
  geom_histogram()

#there is some potential bootstrapping
#https://www.painblogr.org/2017-10-18-purrring-through-bootstraps.html
# library(dplyr)
# library(tidyr)
library(purrr)
library(boot)


temp3 <- temp2 %>% 
  group_by(Coefficient) %>%
  nest() %>%
  mutate(boot_res = map(data,
                        ~ boot(data = .$values,
                               statistic = function(x, i) mean(x[i]),
                               R = 1000)),
         boot_res_ci = map(boot_res, boot.ci, type = "perc"),
         mean = map(boot_res_ci, ~ .$t0),
         lower_ci = map(boot_res_ci, ~ .$percent[[4]]),
         upper_ci = map(boot_res_ci, ~ .$percent[[5]]),
         n =  map(data, nrow)) %>%
  select(-data, -boot_res, -boot_res_ci) %>%
  unnest(cols = c(n, mean, lower_ci, upper_ci)) %>%
  ungroup()

write.csv(temp3, "Data/Processed/TenReach95CI_booted.csv", row.names = F)


