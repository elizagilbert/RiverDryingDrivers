# Sample data to demonstrate
# This will be a list of 55 data frames, each with 8 columns and 25,680 rows
set.seed(123)
data_list <- lapply(1:55, function(x) {
  data.frame(
    Gage = sample(c("Bosque", "San Antonio", "Hwy150", "OtherGage"), 25680, replace = TRUE), # Sample gage names
    RMTenthDry = runif(25680, 0, 1000),            # Random RMTenthDry values
    OtherColumn1 = runif(25680),                   # Other columns
    OtherColumn2 = runif(25680),
    OtherColumn3 = runif(25680),
    OtherColumn4 = runif(25680),
    OtherColumn5 = runif(25680),
    OtherColumn6 = runif(25680)
  )
})


temp <- data_list[[1]]

# Gage recoding mapping with specific values
gage_recode <- c("G_Bosque" = 166, "G_HWy346" = 141, "G_SanAcacia" = 116, "G_Escondida" = 104, "G_SanAntonio" = 88)



# Function to process each data frame
process_data_frame <- function(df, gage_recode) {
  df <- df[df$GageRM_ID %in% names(gage_recode), ]  # Filter only necessary gages
  df$GageNum <- gage_recode[df$GageRM_ID] # Recode GageRM_ID to specific numeric values
  df$AdjustedRMTenthDry <- df$RMTenthDry - df$GageNum # Subtract gage number from RMTenthDry
  df <- df[, c("GageRM_ID", "RMTenthDry", "GageNum", "AdjustedRMTenthDry")] # Keep only relevant columns
  return(df)
}

# Apply the function to each data frame in the list
processed_data_list <- lapply(df_100_final, process_data_frame, gage_recode = gage_recode)

# Optional: To inspect the first processed data frame
head(processed_data_list[[1]])



load("Data/Processed/Random100/dflist_MD_100.RData")
temp <- df_100_final[[1]]

temp2 <- processed_data_list[[1]]

# Assuming your data list is named 'data_list'

# Function to extract unique GageRM_ID and RMTenthDry pairs from a data frame
extract_unique_pairs <- function(df) {
  unique(df[, c("AdjustedRMTenthDry", "RMTenthDry")])
}

# Apply the function to each data frame in the list and combine the results
unique_pairs_list <- lapply(processed_data_list, extract_unique_pairs)

# Combine all data frames into one and get unique pairs
combined_unique_pairs <- unique(do.call(rbind, unique_pairs_list))

temp3 <- combined_unique_pairs %>% 
  summarise(mean(AdjustedRMTenthDry)) %>% 
  rename(MeanDis = 1) %>% 
  mutate(MeanDisKm = MeanDis*1.60934)

