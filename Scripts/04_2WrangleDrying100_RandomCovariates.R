#Read me###
#the purpose of this script is to join the covariates to each randomly binned
#set of 10 river miles sections

#Library ####
library(tidyverse)

#MD data ####
# load("Data/Processed/Radomization2/dflist_MD_3.RData")
load("Data/Processed/Radomization2/dflist_MD_100.RData")
dataframes_list <- df_100

#data covariates ####

#Discharge
dat_discharge <- read.csv("Data/Processed/Randomization/Discharge_All.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

#Climate
dat_climate <- read.csv("Data/Processed/Randomization/TempPrecip_All.csv")%>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

#Returns
dat_returns <- read.csv("Data/Processed/Randomization/Returns_All.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

#Diversions
dat_diversions <- read.csv("Data/Processed/Randomization/Diversions_All.csv")%>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))



#join covariates ####
cov_md_df <- list()

for (i in 1:length(dataframes_list)){
  joined_df <- dataframes_list[[i]] %>% 
    left_join(dat_discharge, by = c("Date", "GageRM_ID"))  %>%   
    left_join(dat_returns, by = c("Date", "ReturnRM_ID")) %>% 
    left_join(dat_climate, by = c("Date", "ClimateRM_ID")) %>% 
    left_join(dat_diversions, by = c("Date", "DiversionRM_ID"))
  
  cov_md_df[[i]] <- joined_df

}

#create matrices of response data ####
# Function to pivot and convert to matrices
fun_matrixMD <- function(df_list) {
  pivoted_list <- lapply(df_list, function(df) {
    df %>%
      select(Date, zMileDays_Cum, RandSamp) %>% 
      group_by(RandSamp) %>% 
      pivot_wider(names_from = Date, values_from = zMileDays_Cum) %>% 
      ungroup()
  })
  
  matrix_list <- lapply(pivoted_list, function(df) {
    as.matrix(df[, -1])  # Exclude the ID column when converting to a matrix
  })
  
  return(matrix_list)
}

# Apply the function to your list of data frames
matrices_list_MD <- fun_matrixMD(cov_md_df)
# save(matrices_list_MD, file = "Data/Processed/Radomization2/matrices_list_MD100.RData")

#create matrices of covariate data ####
# Function to pivot and convert to matrices
fun_matrix_covs <- function(df_list) {
  pivoted_list <- lapply(df_list, function(df) {
    df %>%
      select(Date, RandSamp, Discharge_Cum:DiversionCum) %>% 
      pivot_longer(cols = Discharge_Cum:DiversionCum, names_to = "Predictor", values_to = "Values") %>% 
      group_by(Predictor, RandSamp) %>% 
      mutate(zValues = (Values - mean(Values))/ sd(Values)) %>% 
      ungroup() %>% 
      arrange(Date, Predictor, RandSamp) %>% 
      mutate(NewRowName = paste0(RandSamp,"_",Predictor)) %>% 
      select(Date, zValues,NewRowName) %>% 
      pivot_wider(names_from = Date, values_from = zValues) %>% 
      column_to_rownames(var = "NewRowName")
  })
  
  matrix_list <- lapply(pivoted_list, function(df) {
    as.matrix(df)
  })
  
  return(matrix_list)
}

# Apply the function to your list of data frames
matrices_list_Cov <- fun_matrix_covs(cov_md_df)
# save(matrices_list_Cov, file = "Data/Processed/Radomization2/matrices_list_Cov100.RData")


