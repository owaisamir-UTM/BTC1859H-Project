################################################################################

# BTC1859-H Project (Group 4)
# Burrak UrRehman, Owais Amir, Shadi Abdul-Sater, Saanika Parkhi, Vianne Cai

################################################################################
# Initialization: Package installation and data ingestion.
install.packages(c("janitor", "dplyr", "tidyr", "table1"))
library("janitor")
library("dplyr")
library("tidyr")
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
  "SF36.MCS")

# Creating a new data frame with the data from the relevant columns only.
data_sub <- data[, relevant_cols]

################################################################################

# General data processing and cleaning

################################################################################
# Exploring data and cleaning values.
data_sub <- clean_names(data_sub)

# Converting data to factors for readability.
data_sub$gender <- factor(data_sub$gender, levels = c(1, 2), labels = c("Male", "Female"))
data_sub$liver_diagnosis <- factor(data_sub$liver_diagnosis, levels = c(1:5), labels = c("Hep C", "Hep B", "PSC/PBC/AHA", "Alcohol", "Other"))
data_sub$recurrence_of_disease <- factor(data_sub$recurrence_of_disease, levels = c(0,1), labels = c("No", "Yes"))
data_sub$rejection_graft_dysfunction <- factor(data_sub$rejection_graft_dysfunction, levels = c(0,1), labels = c("No", "Yes"))
data_sub$any_fibrosis <- factor(data_sub$any_fibrosis, levels = c(0,1), labels = c("No", "Yes"))
data_sub$renal_failure <- factor(data_sub$renal_failure, levels = c(0,1), labels = c("No", "Yes"))
data_sub$depression <- factor(data_sub$depression, levels = c(0,1), labels = c("No", "Yes"))
data_sub$corticoid <- factor(data_sub$corticoid, levels = c(0,1), labels = c("No", "Yes"))

str(data_sub)
summary(data_sub)

# Creating new columns that can flag sleep indicator values that are above clinical thresholds.
data_sub <- mutate(data_sub,
                   ESS_thresh = epworth_sleepiness_scale > 10,
                   PSQI_thresh = pittsburgh_sleep_quality_index_score > 4,
                   AIS_thresh = athens_insomnia_scale > 5,
                   BSS_thresh = berlin_sleepiness_scale == 1)

################################################################################

# Preparing data for table1 summary

################################################################################

# Reshaping data from wide to long in order to stratify correctly in table1 summary.
data_long <- pivot_longer(
  data = data_sub,
  cols = c(ESS_thresh, PSQI_thresh, AIS_thresh, BSS_thresh),
  names_to = "Sleep_Scale",
  values_to = "Disturbance")

# Converting sleep disturbance indicators to labeled factors for stratification by sleep scale.
data_long$Sleep_Scale <- factor(data_long$Sleep_Scale,
                                levels = c("ESS_thresh", "PSQI_thresh", "AIS_thresh", "BSS_thresh"),
                                labels = c("ESS", "PSQI", "AIS", "BSS"))

# This is a work around to allow NA's to work as a factor so that they are included in the table1 summary.
data_long$Disturbance[is.na(data_long$Disturbance)] <- "No Data"
data_long$Disturbance <- factor(data_long$Disturbance,
                                levels = c(FALSE, TRUE, "No Data"),
                                labels = c("No Disturbance", "Disturbance", "No Data"))

# Adding labels to all of our variables to make it cleaner.
label(data_long$age) <- "Age"
label(data_long$gender) <- "Gender"
label(data_long$bmi) <- "BMI"
label(data_long$liver_diagnosis) <- "Liver Diagnosis"
label(data_long$recurrence_of_disease) <- "Reccurence of Disease"
label(data_long$rejection_graft_dysfunction) <- "Rejection/Graft Dysfunction"
label(data_long$any_fibrosis) <- "Fibrosis"
label(data_long$renal_failure) <- "Renal Failure"
label(data_long$depression) <- "Depression"
label(data_long$corticoid) <- "Corticosteroid"
label(data_long$sf36_pcs) <- "SF36 PCS Score"
label(data_long$sf36_mcs) <- "SF36 MCS Score"

# Generating a Table 1 summary of all demographic, clinical, and quality of life indicators,
# stratified by sleep scale (ESS, PSQI, AIS, BSS) and whether a sleep disturbance was detected.
table1(~ age + gender + bmi + liver_diagnosis + recurrence_of_disease +
         rejection_graft_dysfunction + any_fibrosis + renal_failure +
         depression + corticoid + sf36_pcs + sf36_mcs |
         Sleep_Scale * Disturbance, 
       data = data_long,
       overall = FALSE)

################################################################################

# NEXT STEP HERE

################################################################################

# Creating a new column that aggregates the binary flags for each sleep indicator above its clinical threshold, resulting in a total score ranging from 0 to 3. 
# Note: PSQI was excluded from the aggregation due to a high proportion of missing values (approximately 85 NAs), which would reduce the number of complete cases.
data_sub <- mutate(data_sub,
                   Sleep_Score = ESS_thresh + AIS_thresh + BSS_thresh)

View(data_sub)

