#Read me ####
#The purpose of this code is to generate 100 random samples of 10 nonoverlapping 
#4.4 mile sections of the river

#Libraries ####
library(tidyverse)

#data ####
dat_drying <- read.csv("Data/Processed/2010_2021_WetDryTenths.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(year(Date) >= 2010, RMTenthDry >=74) %>% 
  filter(between(month(Date), 4, 10)) %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,TRUE ~ 0),
         Year = year(Date)) %>% 
  select(Date, Year, RMTenthDry, DryRM2) 

#random bin, cumsum 100 times ####

# Initialize an empty list to store the dataframes
numruns <- 20
dataframes_list <- list()

###loop #this can break because it can go into an infinite loop I think because of some glitch in
#the overlap function or because there are less than 10 bins after the overlap finishes but
#increasing the number of bins increases the likelihood of the overlap glitch happening
#so I made two 40 random dataframes and a 20 to get 100

for (j in 1:numruns) {
  #random river mile bins 
  # Set the range for bin creation
  min_value <- 74
  max_value <- 170
  bin_size <- 4.4
  num_bins <- 15
  
  # Initialize an empty list to store the bins
  bins_list <- list()
  
  # Function to check for overlap
  check_overlap <- function(bin, bins) {
    for (prev_bin in bins) {
      if (bin[1] < prev_bin[2] && bin[2] > prev_bin[1]) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # Generate random non-overlapping bins
  for (i in 1:num_bins) {
    while (TRUE) {
      # Generate a random start point within the range
      start_point <- runif(1, min_value, max_value - bin_size)
      
      # Calculate the end point based on the bin size
      end_point <- start_point + bin_size
      
      # Round the bin boundaries to the nearest tenth of a decimal
      start_point <- round(start_point, 1)
      end_point <- round(end_point, 1)
      
      # Create the bin
      bin <- c(start_point, end_point)
      
      # Check for overlaps with previously generated bins
      if (!check_overlap(bin, bins_list)) {
        bins_list[[i]] <- bin
        break
      }
    }
  }
  
  # Convert the list of bins to a data frame
  bin_data_frame <- as.data.frame(do.call(rbind, bins_list))
  
  # Rename the columns
  colnames(bin_data_frame) <- c("Start", "End")
  
  # Filter out bins that overlap with the specified range (135.5, 113.5)
  filtered_bins <- bin_data_frame[!(bin_data_frame$Start <= 135.5 & bin_data_frame$End >= 113.5), ] 
  selected_bins <- filtered_bins[1:10,]
  
  #filter data using bins ####
  LengthSampSeq <- nrow(selected_bins)
  
  temp <- map2(selected_bins$Start, selected_bins$End, ~filter(dat_drying, RMTenthDry >= .x & RMTenthDry<= .y)%>% #CHANGE TO Full data
                 group_by(Date) %>%                
                 mutate(MileDays_Total = sum(DryRM2)) %>% 
                 ungroup() %>%
                 distinct(Date, MileDays_Total, .keep_all = T) %>% 
                 arrange(Date) %>% 
                 group_by(year(Date)) %>% 
                 mutate(MileDays_Cum = cumsum(MileDays_Total)) %>% 
                 ungroup() %>% 
                 select(Date, RMTenthDry,MileDays_Cum) %>% 
                 mutate(zMileDays_Cum = (MileDays_Cum - mean(MileDays_Cum, na.rm=TRUE)) / sd(MileDays_Cum, na.rm=TRUE)) %>% 
                 select(Date, RMTenthDry, zMileDays_Cum) %>% 
                 mutate(GageRM_ID = case_when(RMTenthDry > 141 ~ "G_Bosque",
                                              RMTenthDry > 116 ~ "G_HWy346",
                                              RMTenthDry > 104 ~ "G_SanAcacia",
                                              RMTenthDry > 88 ~ "G_Escondida",
                                              RMTenthDry <= 88 ~ "G_SanAntonio"),
                        DiversionRM_ID = case_when(RMTenthDry >= 116 ~ "DivIsleta",
                                                   TRUE ~ "DivSanAcacia"),
                        ClimateRM_ID = case_when(RMTenthDry <= 89 ~ "BosqueDelApache",
                                                 RMTenthDry <= 112 ~ "Socorro",
                                                 RMTenthDry <= 128 ~ "SocorroN",
                                                 RMTenthDry <= 145 ~ "Bernardo",
                                                 RMTenthDry <= 170 ~ "LosLunas"),
                        ReturnRM_ID = case_when(RMTenthDry <= 84 ~ "Ret1",
                                                RMTenthDry <= 90.2 ~ "Ret2",
                                                RMTenthDry <= 127 ~ "Ret3",
                                                RMTenthDry <= 127.2 ~ "Ret4",
                                                RMTenthDry <= 137.9 ~ "Ret5",
                                                RMTenthDry <= 140.1 ~ "Ret6",
                                                RMTenthDry <= 142.7 ~ "Ret7",
                                                RMTenthDry <= 144.6 ~ "Ret8",
                                                RMTenthDry <= 148 ~ "Ret9",
                                                RMTenthDry <= 149.5 ~ "Ret10",
                                                RMTenthDry <= 152.5 ~ "Ret11",
                                                RMTenthDry <= 156.7 ~ "Ret12",
                                                RMTenthDry <= 165.2 ~ "Ret13",
                                                RMTenthDry <= 170 ~ "Ret14")))
  
  temp3 <- Map(function(Date,RandSamp) cbind(Date,RandSamp=RandSamp), temp, 1:LengthSampSeq) 
  temp4 <- temp3[1:10]
  temp5 <- Reduce(full_join,temp4)
  
  
  # Store the result in a dataframe with a unique name
  df_name <- paste0("df_", j)
  dataframes_list[[df_name]] <- temp5
  
}

#save(dataframes_list, file = "Data/Processed/Radomization2/dflist_MD_3.RData")
#save(dataframes_list, file = "Data/Processed/Radomization2/dflist_MD_40a.RData")
#save(dataframes_list, file = "Data/Processed/Radomization2/dflist_MD_40b.RData")
#save(dataframes_list, file = "Data/Processed/Radomization2/dflist_MD_20.RData")

load("Data/Processed/Radomization2/dflist_MD_40a.RData")
load("Data/Processed/Radomization2/dflist_MD_40b.RData")
load("Data/Processed/Radomization2/dflist_MD_20.RData")


df_40a <- dataframes_list
df_40b <- dataframes_list
df_20 <- dataframes_list

df_100 <- c(df_40a, df_40b, df_20)
save(df_100, file = "Data/Processed/Radomization2/dflist_MD_100.RData")
