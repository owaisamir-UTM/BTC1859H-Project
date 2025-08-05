################################################################################

# BTC1859-H Project (Group 4)
# Burrak UrRehman, Owais Amir, Shadi Abdul-Sater, Saanika Parkhi, Vianne Cai

################################################################################

# Initialization: Package installation and data ingestion.
install.packages(c("janitor", "dplyr", "table1"))
library("janitor")
library("dplyr")
library("table1")

# setwd() SET YOUR WORKING DIRECTORY HERE.
data <- read.csv("project_data.csv")

# Subsetting out all column names that will be used for further analysis.
relevant_cols <- c(
  "Gender",                          
  "Age",                             
  "BMI",                             
  "Time.from.transplant",           
  "Liver.Diagnosis",                
  "Recurrence.of.disease",          
  "Rejection.graft.dysfunction",    
  "Any.fibrosis",                   
  "Renal.Failure",                  
  "Depression",                     
  "Corticoid",                      
  "Epworth.Sleepiness.Scale",       
  "Pittsburgh.Sleep.Quality.Index.Score",
  "Athens.Insomnia.Scale",          
  "Berlin.Sleepiness.Scale",        
  "SF36.PCS",                       
  "SF36.MCS"                        
)

# Creating a new data frame with the data from the relevant columns only.
data_sub <- data[, relevant_cols]

################################################################################

# Exploring data and cleaning values.
data_sub <- clean_names(data_sub)

str(data_sub)
summary(data_sub)

# Creating new columns that can flag sleep indicator values that are above clinical thresholds
data_sub$ESS_thresh <- FALSE
data_sub$PSQI_thresh <- FALSE
data_sub$AIS_thresh <- FALSE
data_sub$BSS_thresh <- FALSE

data_sub$ESS_thresh[data_sub$epworth_sleepiness_scale > 10] <- TRUE
data_sub$PSQI_thresh[data_sub$pittsburgh_sleep_quality_index_score > 4] <- TRUE
data_sub$AIS[data_sub$athens_insomnia_scale > 5] <- TRUE
data_sub$BSS_thresh[data_sub$berlin_sleepiness_scale == 1] <- TRUE
