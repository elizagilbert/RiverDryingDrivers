#Read me ####
#The purpose of this script is to run MARSS models on 100
#randomly binned 10 sections of the Rio Grande

#Libraries ###
library(tidyverse)
library(MARSS)

#Load Data ####
load("Data/Processed/Radomization2/matrices_list_MD100.RData")
load("Data/Processed/Radomization2/matrices_list_Cov100.RData")

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
  
  modlist_10states <- list(B = "diagonal and unequal", U = matrix(0,10,1), Q = "diagonal and unequal",
                           c=covariates, C=C_10states, Z = "identity", A = matrix(0,10,1), 
                           R = "diagonal and equal", x0 = "equal", tinitx = 0)
  
  mod_temp<- MARSS(y = df, model = modlist_10states, 
                   control = list(maxit = 100, allow.degen = T, trace =1, safe = T, 
                                  conv.test.slope.tol = 0.09), fit = T) 
  mod_temp_BFGS <- MARSS(y = df, model = modlist_10states, control = list(maxit = 5000), 
                         method = "BFGS", inits = mod_temp$par)
}


# List of data frames
list_of_data_frames <- matrices_list_MD  # Replace with your actual data frames

# List of covariates
list_of_covariates <- matrices_list_Cov  # Replace with your actual covariates

# Loop through the lists and fit models
start.time <- Sys.time()
results_list <- lapply(1:length(list_of_data_frames), function(i) {
  df <- list_of_data_frames[[i]]
  covariates <- list_of_covariates[[i]]
  model <- fit_multivariate_model(df, covariates)  # Pass any additional parameters
  return(model)
})
end.time <- Sys.time()
print(round(end.time - start.time,2))

save(results_list, file = "ModelOutput/Randomization2/ResultsList100Samples.RData")
load("ModelOutput/Randomization2/ResultsList100Samples.RData")

#calculate RMSE ####

# Create a list to store the dataframes for each model
model_dfs <- list()

# Loop through the 100 models to get fitted values
for (i in 1:100) {
  conf_marss <- fitted(results_list[[i]], type = "ytT", interval = "confidence")
  pred_marss <- fitted(results_list[[i]], type = "ytT", interval = "prediction")
  
  df <- cbind(conf_marss, pred_marss[, c(".lwr", ".upr")]) %>% rename(Reach = 1)
  
  # Store the dataframe in the list
  model_dfs[[i]] <- df
}

# model_dfs now contains a list of dataframes, one for each model
calculate_RMSE <- function(df) {
  RMSE <- sqrt(mean((df$y - df$.fitted)^2))
  return(RMSE)
}

RMSE_list <- lapply(model_dfs, calculate_RMSE)
average_RMSE <- mean(unlist(RMSE_list), na.rm = T)
sd_RMSE <- sd(unlist(RMSE_list), na.rm = T)

average_RMSE
sd_RMSE
#calculate R-squared ####
calculate_Rsquared <- function(df){
  Sum_resids <- sum((df$y - df$.fitted)^2)
  TotalSUm_resids <- sum((df$y - mean(df$y))^2)
  Rsquared <- 1-(Sum_resids/TotalSUm_resids)
  return(Rsquared)
}

Rsquared_list <- lapply(model_dfs, calculate_Rsquared)
average_Rsquared <- mean(unlist(Rsquared_list), na.rm = T)
average_Rsquared

sd_Rsquared <- sd(unlist(Rsquared_list), na.rm = T)
sd_Rsquared

calculate_AdjRsquared <- function(df){
  n <- 10
  k <- 5
  Sum_resids <- sum((df$y - df$.fitted)^2)
  TotalSUm_resids <- sum((df$y - mean(df$y))^2)
  Rsquared <- 1-(Sum_resids/TotalSUm_resids)
  adjusted_Rsquared <- 1-((1-Rsquared) * (n-1) / (n-k-1))
  return(adjusted_Rsquared)
} 

Adj_Rsquared_list <- lapply(model_dfs, calculate_AdjRsquared)
average_AjdRsquared <- mean(unlist(Adj_Rsquared_list), na.rm = T)
average_AjdRsquared



#calculate mean and CIs ####
# load("ModelOutput/Randomization2/ResultsList5Samples.RData")

model_params <- lapply(results_list, MARSSparamCIs)
save(model_params, file = "ModelOutput/Randomization2/ModelParams100samples.RData")

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


#there is some potential bootstrapping
#https://www.painblogr.org/2017-10-18-purrring-through-bootstraps.html
# library(dplyr)
# library(tidyr)
# library(purrr)
# library(boot)
# 
# set.seed(321)
# mtcars %>%
#   group_by(vs) %>%
#   nest() %>% 
#   mutate(boot_res = map(data,
#                         ~ boot(data = .$mpg,
#                                statistic = function(x, i) mean(x[i]),
#                                R = 1000)),
#          boot_res_ci = map(boot_res, boot.ci, type = "perc"),
#          mean = map(boot_res_ci, ~ .$t0),
#          lower_ci = map(boot_res_ci, ~ .$percent[[4]]),
#          upper_ci = map(boot_res_ci, ~ .$percent[[5]]),
#          n =  map(data, nrow)) %>% 
#   select(-data, -boot_res, -boot_res_ci) %>% 
#   unnest(cols = c(n, mean, lower_ci, upper_ci)) %>% 
#   ungroup()


