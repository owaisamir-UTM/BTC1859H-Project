################################################################################

# BTC1859-H Project (Group 4)
# Burrak UrRehman, Owais Amir, Shadi Abdul-Sater, Saanika Parkhi, Vianne Cai

################################################################################
# Initialization: Package installation and data ingestion.
install.packages(c("janitor", "dplyr", "tidyr", "table1", "ggplot2"))
library("janitor")
library("dplyr")
library("tidyr")
library("table1")
library("ggplot2")

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

################################################################################

# Relationship between sleep disturbance and quality of life (QoL)

################################################################################

# Summary and distributions of the mental ("sf36_mcs") and physical ("sf36_pcs") quality of life variables are seen
summary(data_sub$sf36_mcs)       #mental QoL, 21 NAs, mean = 46.22
hist(data_sub$sf36_mcs)          #slightly right-skewed distribution is seen

summary(data_sub$sf36_pcs)       #physical QoL, 21 NAs, mean = 43.05 
hist(data_sub$sf36_pcs)          #roughly normally distributed


# The relationship between "Sleep_Score" and the physical/mental QoLs is seen where the score is treated as a categorical factor;
# this was done to determine whether "Sleep_Score" could be treated as a numerical variable in the subsequent models
# It allows the assessment of whether the effect of increasing sleep disturbance on physical and mental QoL is approximately linear

# Mental QoL ("sf36_mcs") and "Sleep_Score" (factor)
lm(sf36_mcs ~ factor(Sleep_Score), data = data_sub)

# Physical QoL ("sf36_pcs") and "Sleep_Score" (factor)
lm(sf36_pcs ~ factor(Sleep_Score), data = data_sub)

# Summary:
# In both cases, linear regression using "Sleep_Score" as categorical showed a strong negative relationship between sleep disturbance and QoL
# The effect is roughly linear across all levels of the score, with a 5-6 point drop in physical and mental QoL at each level of "Sleep_Score"
# This consistent decline suggests linearity and supports treating "Sleep_Score" as a numerical variable for the next analyses


# Correlation Test: Sleep Disturbance & QoL

# First, the 'cor.test()' function will be used to assess the strength and direction of the relationship between "Sleep_Score" and both types of QoL
# A Spearman correlation is used as "Sleep_Score" is not a continuous variable and as "sf36_mcs" is slightly positively skewed (not normal)
# The null hypothesis for both is the rho=0 (there is no correlation between "Sleep_Score" and the quality of life component)

# Correlation between "Sleep_Score" and mental QoL
cor.test(data_sub$Sleep_Score, data_sub$sf36_mcs, method = "spearman")      #rho = -0.39, p-value = 6.3e-10

# Correlation between "Sleep_Score" and physical QoL
cor.test(data_sub$Sleep_Score, data_sub$sf36_pcs, method = "spearman")      #rho = -0.42, p-value = 2.5e-11

# Summary:
# Spearman correlation revealed a moderately, negative association between "Sleep_Score" and both components of QoL
# At a significance level (alpha value) of 0.05, both of these relationships were significant as the p-value < alpha value and there is evidence to reject the null hypothesis in both cases; 
# namely, higher sleep scores were significantly associated with a lower mental (rho = -0.39, p < 6.3e-10) and a lower physical (rho = -0.42, p = 2.5e-11) QoL
# The findings show that as the sleep disturbance score increases, the mental and physical quality of life decreases in liver transplant patients 


# Linear Regression Model: Sleep Disturbance & QoL

# Linear regression models using "lm()" is used to evaluate the impact of "Sleep_Score" on the physical and mental quality of life
# This is done to quantify the average change in "sf36_mcs"/"sf36_pcs" for every unit increase in "Sleep_Score"
# The null in both models for the intercept is that the intercept estimate (β0) = 0
# The null in both models for the slope is that the slope (β1) = 0, meaning there is no relationship between the "Sleep_Score" and that QoL component

# Linear regression for "Sleep_Score" and mental QoL
lm_mental <- lm(sf36_mcs ~ Sleep_Score, data = data_sub)
summary(lm_mental)      #intercept =  52.53, p-value < 2e-16
                        #sleep_score = -5.03, p-value = 1.9e-10
                        #F-score = 44.51, Adjusted R^2 = 0.158

# Linear regression for "Sleep_Score" and physical QoL
lm_physical <- lm(sf36_pcs ~ Sleep_Score, data = data_sub)
summary(lm_physical)      #intercept =  49.49, p-value < 2e-16
                          #sleep_score = -5.17, p-value = 4.1e-11
                          #F-score = 48.08, Adjusted R^2 = 0.169

# Summary:
# For the mental QoL model, the intercept (β0 = 52.53, p < 2e-16) represents that for patients with a "Sleep_Score" = 0, their average "sf36_mcs" score is 52.53; the intercept is also statistically significant (as p-value<0.05)
# The slope (β1 = -5.03, p = 1.9e-10) indicates that a one-unit increase in "Sleep_Score" is associated with a mean 5.03-point decrease in mental QoL, and that there is evidence to reject the null hypothesis (as p-value < significance level); 
# this means that "Sleep_Score" has a significant relationship with the liver transplant recipient's mental quality of life
# The adjusted R² = 0.158 shows that the model explains about 15.8% of the variation in mental QoL
#
# For the physical QoL model, the intercept (β0 = 49.49, p < 2e-16) is also significant and represents the average "sf36_pcs" score when Sleep_Score = 0 
# The slope (β₁ = -5.17, p = 4.1e-11) suggests a 5.17-point average decrease in physical QoL with every one-unit increase in "Sleep_Score" and that there is evidence to reject the null hypothesis 
# this means that "Sleep_Score" has a significant relationship with the liver transplant recipient's physical quality of life
# The adjusted R² = 0.169 indicates that about 16.9% of the variability in physical QoL can be explained by this model

## NEED TO COMPLETE STEPS BELOW## 

# Exploring the residuals
plot(resid(lm_physical))
plot(resid(lm_mental))

# Exploring the QQ plot
qqnorm(resid(lm_physical))
qqline(resid(lm_physical), col = "red")

qqnorm(resid(lm_mental))
qqline(resid(lm_mental), col = "red")

# Modelling
ggplot(data_sub, aes(x = Sleep_Score, y = sf36_pcs)) + geom_jitter() + geom_smooth(method = "lm") + labs(x = "Sleep Score", y = "Physical QoL Score")
ggplot(data_sub, aes(x = Sleep_Score, y = sf36_mcs)) + geom_jitter() + geom_smooth(method = "lm") + labs(x = "Sleep Score", y = "Mental QoL Score")


manova_model <- manova(cbind(sf36_pcs, sf36_mcs) ~ Sleep_Score, data = data_sub) 
summary(manova_model, test = "Wilks")
