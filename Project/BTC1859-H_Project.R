################################################################################

# BTC1859-H Project (Group 4)
# Burrak Urrehman, Owais Amir, Shadi Abdul-Sater, Saanika Parkhi, Vianne Cai

################################################################################
# Initialization: Package installation and data ingestion.
install.packages(c("janitor", "dplyr", "tidyr", "table1", "ggplot2", "stringr"))
library("janitor")
library("dplyr")
library("tidyr")
library("table1")
library("ggplot2")
library("stringr")

# setwd() SET YOUR WORKING DIRECTORY HERE.
data <- read.csv("project_data.csv")

# PSQI will be excluded from our data analysis due to high degree of missingness
# 85/268 observations are missing ~32% data missingness which is over our cutoff of 30%
summary(data)

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
  "Athens.Insomnia.Scale",          
  "Berlin.Sleepiness.Scale",        
  "SF36.PCS",                       
  "SF36.MCS")

# Creating a new data frame with the data from the relevant columns only.
data_sub <- data[, relevant_cols]
rm("relevant_cols")

################################################################################

# General data processing and cleaning

################################################################################
# Exploring data and cleaning values.
data_sub <- clean_names(data_sub)
any(duplicated(data_sub))

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

# Checking for allowable values
# ESS (0–24) 1 value(s) found
data_sub$epworth_sleepiness_scale[
  data_sub$epworth_sleepiness_scale < 0 | data_sub$epworth_sleepiness_scale > 24
] <- NA

# AIS (0–24) 0 value(s) found
data_sub$athens_insomnia_scale[
  data_sub$athens_insomnia_scale < 0 | data_sub$athens_insomnia_scale > 24
] <- NA

# SF-36 (0–100) 0 value(s) found
data_sub$sf36_pcs[
  data_sub$sf36_pcs < 0 | data_sub$sf36_pcs > 100
] <- NA
data_sub$sf36_mcs[
  data_sub$sf36_mcs < 0 | data_sub$sf36_mcs > 100
] <- NA

# BSS (0/1) 0 value(s) found
data_sub$berlin_sleepiness_scale[
  !data_sub$berlin_sleepiness_scale %in% c(0,1)
] <- NA

# Creating new columns that can flag sleep indicator values that are above clinical thresholds.
data_sub <- mutate(data_sub,
                   ESS_thresh = epworth_sleepiness_scale > 10,
                   AIS_thresh = athens_insomnia_scale > 5,
                   BSS_thresh = berlin_sleepiness_scale == 1)


# Creating a new column that aggregates the binary flags for each sleep indicator
# above its clinical threshold, resulting in a total score ranging from 0 to 3. 
data_sub <- mutate(data_sub, Sleep_Score = ESS_thresh + AIS_thresh + BSS_thresh)

################################################################################

# Generating histogram plots to show the distribution of ESS, AIS, and BSS scores

################################################################################
# ESS Plot
# I first want to calculate the mean and standard deviation - this lets me create a label to easily place in my plot
ess_mean <- mean(data_sub$epworth_sleepiness_scale, na.rm = TRUE) # Mean of 7.76
ess_sd <- sd(data_sub$epworth_sleepiness_scale, na.rm = TRUE) # SD of 4.55
ess_label <- paste0("Mean ± SD\n", round(ess_mean, 2), " ± ", round(ess_sd, 2))

ggplot(data_sub, aes(
  x = epworth_sleepiness_scale,
  fill = cut(epworth_sleepiness_scale,
             breaks = c(-Inf, 10, 12, 15, 24),         
             labels = c("Normal", "Mild Daytime Sleepiness", "Moderate Daytime Sleepiness", "Severe Daytime Sleepiness"),
             right = TRUE)
)
) +
  geom_bar(color = "black", na.rm = TRUE) +
  scale_fill_manual(values = c(
    "Normal" = "darkgreen",
    "Mild Daytime Sleepiness" = "yellow",
    "Moderate Daytime Sleepiness" = "orange",
    "Severe Daytime Sleepiness" = "red"
  )) +
  labs(title = "Distribution of ESS Scores",
       x = "ESS Score",
       y = "Frequency",
       fill = "ESS Ranks") +
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0, 24)) +
  annotate("label", 
           x = 24, 
           y = max(table(data_sub$epworth_sleepiness_scale))/2,
           label = ess_label,
           hjust = 1, vjust = 0, size = 4)
rm("ess_label", "ess_mean", "ess_sd")

# AIS Plot
# Similar to the ESS plot first calculating mean and standard deviation while creating label
ais_mean <- mean(data_sub$athens_insomnia_scale, na.rm = TRUE) # Mean of 7.41
ais_sd <- sd(data_sub$athens_insomnia_scale, na.rm = TRUE) # SD of 5.33
ais_label <- paste0("Mean ± SD\n", round(ais_mean, 2), " ± ", round(ais_sd, 2))

ggplot(data_sub, aes(
  x = athens_insomnia_scale,
  fill = cut(athens_insomnia_scale,
             breaks = c(-Inf, 5, 10, 15, 24),
             labels = c("Normal Sleep", "Mild Insomnia", "Moderate Insomnia", "Severe Insomnia"),
             right = TRUE)
  )
  ) +
  geom_bar(color = "black", na.rm = TRUE) +
  scale_fill_manual(values = c(
    "Normal Sleep" = "darkgreen",
    "Mild Insomnia" = "yellow",
    "Moderate Insomnia" = "orange",
    "Severe Insomnia" = "red"
  )) +
  labs(title = "Distribution of AIS Scores",
       x = "AIS Score",
       y = "Frequency",
       fill = "AIS Ranks") +
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0, 24)) +
  annotate("label", 
           x = 24, 
           y = max(table(data_sub$athens_insomnia_scale))/2,
           label = ais_label,
           hjust = 1, vjust = 0, size = 4)
rm("ais_label", "ais_mean", "ais_sd")

# BSS Plot
# Start out by filtering the NA values out of the data
data_bq <- filter(data_sub, !is.na(berlin_sleepiness_scale))

ggplot(data_bq, aes(x = factor(berlin_sleepiness_scale),
                    fill = factor(berlin_sleepiness_scale))) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = paste0("n = ",after_stat(count))), vjust = -0.5) +
  scale_x_discrete(labels = c("0" = "Low Risk of Sleep Disordered Breathing", "1" = "High Risk of Sleep Disordered Breathing")) +
  scale_fill_manual(values = c("0" = "darkgreen", "1" = "red"),
                    name = "Berlin Sleepiness Scale") +
  labs(title = "Distribution of BSS Scores",
       x = "Category",
       y = "Count") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

rm("data_bq")

# Table showing valid and missing counts for sleep scales
sleep_vars <- c("epworth_sleepiness_scale",
                "athens_insomnia_scale",
                "berlin_sleepiness_scale")

valids <- sapply(data_sub[sleep_vars], function(x) sum(!is.na(x)))
missings <- sapply(data_sub[sleep_vars], function(x) sum(is.na(x)))

missing_summary <- data.frame(
  Valid_Count = valids,
  Missing_Count = missings
)

# ESS missing 18; AIS missing 6; BSS missing 6
missing_summary

################################################################################

# Generating histogram plots to show the distribution of QoL scores

################################################################################

# SF36-PCS Plot
sf36p_mean <- mean(data_sub$sf36_pcs, na.rm = TRUE) # Mean of 43.05
sf36p_sd <- sd(data_sub$sf36_pcs, na.rm = TRUE) # SD of 12.04
sf36p_label <- paste0("Mean ± SD\n", round(sf36p_mean, 2), " ± ", round(sf36p_sd, 2))

ggplot(data_sub, aes(x = as.numeric(sf36_pcs))) +
  geom_histogram(binwidth = 2, color = "black", na.rm = TRUE, fill ="darkgreen") +
  labs(title = "Distribution of SF36-PCS Scores",
       x = "SF36-PCS Score",
       y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0, 100)) +
  annotate("label",
           x = 100,
           y = 11,
           label = sf36p_label,
           hjust = 1, vjust = 0, size = 4)
rm("sf36p_label", "sf36p_mean", "sf36p_sd")

# SF36-MCS Plot
sf36m_mean <- mean(data_sub$sf36_mcs, na.rm = TRUE) # Mean of 46.22
sf36m_sd <- sd(data_sub$sf36_mcs, na.rm = TRUE) # SD of 12.16
sf36m_label <- paste0("Mean ± SD\n", round(sf36m_mean, 2), " ± ", round(sf36m_sd, 2))

ggplot(data_sub, aes(x = as.numeric(sf36_mcs))) +
  geom_histogram(binwidth = 2, color = "black", na.rm = TRUE, fill ="darkgreen") +
  labs(title = "Distribution of SF36-MCS Scores",
       x = "SF36-MCS Score",
       y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0, 100)) +
  annotate("label",
           x = 100,
           y = 11,
           label = sf36m_label,
           hjust = 1, vjust = 0, size = 4)
rm("sf36m_label", "sf36m_mean", "sf36m_sd")

################################################################################

# Preparing data for table1 summary

################################################################################

# Adding labels to all of our variables to make it cleaner.
label(data_sub$age) <- "Age"
label(data_sub$gender) <- "Gender"
label(data_sub$bmi) <- "BMI"
label(data_sub$time_from_transplant) <- "Time Since Transplant (years)"
label(data_sub$liver_diagnosis) <- "Liver Diagnosis"
label(data_sub$recurrence_of_disease) <- "Reccurence of Disease"
label(data_sub$rejection_graft_dysfunction) <- "Rejection/Graft Dysfunction"
label(data_sub$any_fibrosis) <- "Fibrosis"
label(data_sub$renal_failure) <- "Renal Failure"
label(data_sub$depression) <- "Depression"
label(data_sub$corticoid) <- "Corticosteroid"
label(data_sub$sf36_pcs) <- "SF36 PCS Score"
label(data_sub$sf36_mcs) <- "SF36 MCS Score"

table1(~ age + gender + bmi + time_from_transplant + liver_diagnosis + recurrence_of_disease +
         rejection_graft_dysfunction + any_fibrosis + renal_failure + depression + corticoid +
         sf36_pcs + sf36_mcs, data = data_sub, caption = "Demographic and Clinical Summary of Baseline Patient Characteristics")

################################################################################

# 2. Relationship between sleep disturbance and quality of life (QoL)

################################################################################

## 2.1. Relationship between sleep disturbance scales and QoL

# There are three sleep disturbance scales: "epworth_sleepiness_scale" (ESS), "berlin_sleepiness_scale" (BSS) and "athens_insomnia_scale" (AIS); (note PSQI is not being considered due to high level of missingness)
# It is necessary to see whether the three scales separately affect the mental  ("sf36_mcs") and physical  ("sf36_pcs") quality of life scores of the patient
# Independent two-sample t-tests are used to compare mean mental and physical QoL scores between patients above vs. below each clinical threshold for each scale (this test can be used as n > 30 for each scale)
# Null hypothesis is that mean QoL score (mental or physical) is equal between patients scoring above and below the threshold for each scale; significance level = 0.05

# ESS
# Comparing mean mental and physical QoL between patients above vs. below ESS_thresh
t.test(sf36_mcs~ESS_thresh, data=data_sub)    #mean in group FALSE = 48.09, mean in group TRUE = 42.07, p-value = 0.001
t.test(sf36_pcs~ESS_thresh, data=data_sub)    #mean in group FALSE = 45.30, mean in group TRUE = 37.28, p-value = 1.02e-05

# BSS
# Comparing mean mental and physical QoL between patients above vs. below BSS_thresh
t.test(sf36_mcs~BSS_thresh, data=data_sub)    #mean in group FALSE = 47.59, mean in group TRUE = 43.84, p-value = 0.022
t.test(sf36_pcs~BSS_thresh, data=data_sub)    #mean in group FALSE = 45.53, mean in group TRUE = 39.16, p-value = 8.07e-05

# AIS
# Comparing mean mental and physical QoL between patients above vs. below AIS_thresh
t.test(sf36_mcs~AIS_thresh, data=data_sub)    #mean in group FALSE = 52.67, mean in group TRUE = 41.11, p-value = 4.06e-16
t.test(sf36_pcs~AIS_thresh, data=data_sub)    #mean in group FALSE = 47.32, mean in group TRUE = 39.81, p-value = 4.83e-07

# Summary: 
# Across all 3 sleep disturbance scales - the Epworth Sleepiness Scale (ESS), Berlin Sleepiness Scale (BSS), and Athens Insomnia Scale (AIS) - patients above the clinical threshold had significantly lower mental and physical QoL scores than those below it
# AIS had the strongest negative effect with an average drop of 11.56 points in mental QoL and 7.51 points in physical QoL 
# This indicates that meeting the clinical cutoff for any individual sleep measure, particularly insomnia, relates to significantly poorer mental and physical quality of life outcomes


## 2.2. Relationship between Sleep Score and QoL Components

# The relationship between "Sleep_Score" (aggregation of the sleep disturbance scales) and the physical/mental QoLs is seen where the score is treated as a categorical factor;
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

# Creating boxplots to visualize the relationship between QoL and each sleep disturbance scale

# Mental QoL
# Preparing data for boxplots comparing Mental QoL (sf36_mcs) across threshold status for each scale
boxplot_data <- data_sub %>%
  select(sf36_mcs, ESS_thresh, BSS_thresh, AIS_thresh) %>%       #selecting columns
  drop_na() %>%     #dropping NAs
  pivot_longer(cols = c(BSS_thresh, ESS_thresh, AIS_thresh),
               names_to = "Scale",            # the name of the scale (BSS, AIS, ESS)
               values_to = "Threshold")       # TRUE = above threshold, FALSE = below threshold

ggplot(boxplot_data, aes(x = Threshold, y = sf36_mcs, fill = factor(Threshold))) +     #creating boxplot with threshold on x-axis, and sf36_mcs score on y-axis
  geom_boxplot() +
  scale_fill_manual(values = c("FALSE" = "darkgreen", "TRUE" = "red")) +        
  labs(
    title = "Mental QoL by Sleep Disturbance Threshold",
    x = "Threshold (Below / Above)",
    y = "SF-36 Mental Component Score"
  ) + 
  facet_wrap(~Scale) +     #creating separate sections for each scale
  theme_minimal()

# Summary: 
# For all three scales, sf36_mcs median scores are consistently lower when the threshold is met (TRUE) compared to when it is not met (FALSE).                   

# Physical QoL
# Preparing data for boxplots comparing Physical QoL (sf36_pcs) across threshold status for each scale
boxplot_data <- data_sub %>%
  select(sf36_pcs, ESS_thresh, BSS_thresh, AIS_thresh) %>%       #selecting columns
  drop_na() %>%     #dropping NAs
  pivot_longer(cols = c(BSS_thresh, ESS_thresh, AIS_thresh),
               names_to = "Scale",            # the name of the scale (BSS, AIS, ESS)
               values_to = "Threshold")       # TRUE = above threshold, FALSE = below threshold

ggplot(boxplot_data, aes(x = Threshold, y = sf36_pcs, fill = factor(Threshold))) +     #creating boxplot with threshold on x-axis, and sf36_pcs score on y-axis
  geom_boxplot() +
  scale_fill_manual(values = c("FALSE" = "darkgreen", "TRUE" = "red")) +        
  labs(
    title = "Physical QoL by Sleep Disturbance Threshold",
    x = "Threshold (Below / Above)",
    y = "SF-36 Physical Component Score"
  ) + 
  facet_wrap(~Scale) +     #creating separate sections for each scale
  theme_minimal()

# Summary: 
# Across all three scales, sf36_pcs median scores are consistently lower when the threshold is met (TRUE) compared to when it is not met (FALSE)                   
             
# Correlation Test: Sleep Disturbance & QoL

# First, the 'cor.test()' function will be used to assess the strength and direction of the relationship between "Sleep_Score" and both types of QoL
# A Spearman correlation is used as "Sleep_Score" is not a continuous variable and as "sf36_mcs" is slightly positively skewed (not normal)
# The null hypothesis for both is the rho=0 (there is no correlation between "Sleep_Score" and the quality of life component)

# Correlation between "Sleep_Score" and mental QoL
cor.test(data_sub$Sleep_Score, data_sub$sf36_mcs, method = "spearman")      #rho = -0.39, p-value = 9.08e-10

# Correlation between "Sleep_Score" and physical QoL
cor.test(data_sub$Sleep_Score, data_sub$sf36_pcs, method = "spearman")      #rho = -0.42, p-value = 1.47e-11

# Summary:
# Spearman correlation revealed a moderately, negative association between "Sleep_Score" and both components of QoL
# At a significance level (alpha value) of 0.05, both of these relationships were significant as the p-value < alpha value and there is evidence to reject the null hypothesis in both cases; 
# namely, higher sleep scores were significantly associated with a lower mental (rho = -0.39, p = 9.08e-10) and a lower physical (rho = -0.42, p = 1.47e-11) QoL
# The findings show that as the sleep disturbance score increases, the mental and physical quality of life decreases in liver transplant patients 


# Linear Regression Model: Sleep Disturbance & QoL

# Linear regression models using "lm()" is used to evaluate the impact of "Sleep_Score" on the physical and mental quality of life
# This is done to quantify the average change in "sf36_mcs"/"sf36_pcs" for every unit increase in "Sleep_Score"
# The null in both models for the intercept is that the intercept estimate (β0) = 0
# The null in both models for the slope is that the slope (β1) = 0, meaning there is no relationship between the "Sleep_Score" and that QoL component

# Linear regression for "Sleep_Score" and mental QoL
lm_mental <- lm(sf36_mcs ~ Sleep_Score, data = data_sub)
summary(lm_mental)      #intercept =  52.53, p-value < 2e-16
                        #sleep_score = -5.04, p-value = 2.42e-10
                        #F-score = 43.91, Adjusted R^2 = 0.157

# Plotting relationship between "Sleep_Score" and mental QoL (with regression line)    
ggplot(data_sub, aes(x = Sleep_Score, y = sf36_mcs)) +
  geom_point(color = "darkgreen", alpha = 0.4) +  # scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # regression line without confidence interval
  labs(
    title = "Relationship Between Sleep Score and Mental QoL",
    x = "Sleep Disturbance Score",
    y = "SF-36 Mental Component Score"
  ) +
  theme_minimal()
                   
# Linear regression for "Sleep_Score" and physical QoL
lm_physical <- lm(sf36_pcs ~ Sleep_Score, data = data_sub)
summary(lm_physical)      #intercept =  49.49, p-value < 2e-16
                          #sleep_score = -5.28, p-value = 2.07e-11
                          #F-score = 49.7, Adjusted R^2 = 0.174

# Plotting relationship between "Sleep_Score" and physical QoL (with regression line)
ggplot(data_sub, aes(x = Sleep_Score, y = sf36_pcs)) +
  geom_point(color = "darkgreen", alpha = 0.4) +  # scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # regression line without CI
  labs(
    title = "Relationship Between Sleep Score and Physical QoL",
    x = "Sleep Disturbance Score",
    y = "SF-36 Physical Component Score"
  ) +
  theme_minimal()
                   
# Summary:
# For the mental QoL model, the intercept (β0 = 52.53, p < 2e-16) represents that for patients with a "Sleep_Score" = 0, their average "sf36_mcs" score is 52.53; the intercept is also statistically significant (as p-value<0.05)
# The slope (β1 = -5.04, p = 2.42e-10) indicates that a one-unit increase in "Sleep_Score" is associated with a mean 5.04-point decrease in mental QoL, and that there is evidence to reject the null hypothesis (as p-value < significance level); 
# this means that "Sleep_Score" has a significant relationship with the liver transplant recipient's mental quality of life
# The adjusted R^2 = 0.157 shows that the model explains about 15.7% of the variation in mental QoL
#
# For the physical QoL model, the intercept (β0 = 49.49, p < 2e-16) is also significant and represents the average "sf36_pcs" score when Sleep_Score = 0 
# The slope (β1 = -5.28, p = 2.07e-11) suggests a 5.28-point average decrease in physical QoL with every one-unit increase in "Sleep_Score" and that there is evidence to reject the null hypothesis 
# this means that "Sleep_Score" has a significant relationship with the liver transplant recipient's physical quality of life
# The adjusted R^2 = 0.174 indicates that about 17.4% of the variability in physical QoL can be explained by this model

                  
# Evaluating Linear Regression Assumptions 

# The linear regression models are based on a few assumptions, including homoscedasticity (constant variance), normality, and linearity
# Residuals are being plotted to visually check for any patterns
plot(resid(lm_physical), 
     main = "Residual Plot for Physical Quality of Life Model",      #random scatter, no signs of homoscedasticity or non-linearity
     xlab = "Observation Index", 
     ylab = "Residuals", 
     col = "darkgreen", 
     pch = 21)    

plot(resid(lm_mental), 
     main = "Residual Plot for Mental Quality of Life Model",      #random scatter, no signs of homoscedasticity or non-linearity
     xlab = "Observation Index", 
     ylab = "Residuals", 
     col = "darkgreen", 
     pch = 21)    

# Plotting the Q-Q plot to check for Normality of residuals
qqnorm(resid(lm_physical))
qqline(resid(lm_physical), col = "red")    #most of the points lie on the reference line in red; Normality assumption holds true

qqnorm(resid(lm_mental))
qqline(resid(lm_mental), col = "red")      #most of the points lie on the reference line in red, however there are slight deviations at the ends; Normality assumption is still assumed to hold true

                   
################################################################################

# 2. ESTIMATION OF THE PREVALENCE OF SLEEP DISTURBANCE

################################################################################


# 2.1a Prevalence estimates of ESS, AIS, BSS -----------------------------------

# Find prevalence based on each score using mean() since the values are binary.
# All 1 values (TRUE) indicate that the patient was categorized as sleep disturbed.
# taking the mean will provide the prevalence in this case.
ESS_prevalence <- mean(data_sub$ESS_thresh, na.rm = TRUE)*100
AIS_prevalence <- mean(data_sub$AIS_thresh, na.rm = TRUE)*100
BSS_prevalence <- mean(data_sub$BSS_thresh, na.rm = TRUE)*100

# Created a function that will take in a sleep score/measure column, 
# remove NA values and then perform a binomial test to get confidence intervals, 
# which can then be extracted and put in a dataframe alongside the prevalence.
get_ci <- function(x) {
  x <- na.omit(x)
  binom.test(sum(x), length(x))$conf.int * 100
}

# Calculate confidence intervals for each
ESS_ci <- get_ci(data_sub$ESS_thresh)
AIS_ci <- get_ci(data_sub$AIS_thresh)
BSS_ci <- get_ci(data_sub$BSS_thresh)
Agg_ci <- get_ci(data_sub$Sleep_Score >= 1)

# Also, calculated the prevalence by categorizing "Sleep Disturbance"
# for aggregated sleep scores of >=1. 
# The aggregated sleep score sums the scores from ESS, AIS, BSS,
# providing a comprehensive perspective on sleep disturbance.
agg_disturbance_prev <- mean(data_sub$Sleep_Score >=1, na.rm = TRUE) * 100

# Created a separated data frame for plotting the prevalence.
prev_df <- data.frame(
  Measure = c("ESS", "AIS", "BSS", "Aggregated Score"),
  Prevalence = c(ESS_prevalence, AIS_prevalence, BSS_prevalence, agg_disturbance_prev),
  CI_lower = c(ESS_ci[1], AIS_ci[1], BSS_ci[1], Agg_ci[1]),
  CI_upper = c(ESS_ci[2], AIS_ci[2], BSS_ci[2], Agg_ci[2])
)
prev_df

# Bar plot showing prevalence based on 3 sleep scores, and the aggregated measure.
# (PSQI measure was excluded)
# Added percentage labels for prevalence values above each respective bar.
ggplot(prev_df, aes(x = Measure, y = Prevalence, fill = Measure)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, width = 0.1), color = "gray40") +
  geom_text(aes(label = paste0(round(Prevalence, 1), "%"), y = Prevalence / 2),
            size = 4) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(
    title = "Prevalence of Sleep Disturbance by Measurement",
    x = "Sleep Measurement",
    y = "Prevalence (%)",  
    caption = str_wrap("Barplot depicting the prevelance of sleep disturbance in
                       patients with liver transplant based on various sleep
                       measurements (AIS, BSS, ESS and an aggregated score of
                       these measurements), with 95% confidence interval error bars", 100)
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  ) + 
  scale_fill_brewer(palette = "Pastel1")

# 2.1b Creating proportion tables ----------------------------------------------

# Copied data set for proportion tables and plots
data_prop <- data_sub

# Convert Sleep_Score to a factor for nice labeling in table1
data_prop$Sleep_Score_F <- factor(
  data_prop$Sleep_Score,
  levels = 0:3,
  labels = c("0 Measure",
             "1 Measure",
             "2 Measures",
             "3 Measures")
)

# Frequency table including NAs
freq_table <- table(data_prop$Sleep_Score_F, useNA = "ifany")

# Proportion table including NAs
prop_aggregated_score <- prop.table(freq_table)
prop_aggregated_score

# Convert binary variables to factors for table1
data_prop$ESS_thresh <- factor(data_prop$ESS_thresh, levels = c(0, 1), labels = c("No", "Yes"))
data_prop$AIS_thresh <- factor(data_prop$AIS_thresh, levels = c(0, 1), labels = c("No", "Yes"))
data_prop$BSS_thresh <- factor(data_prop$BSS_thresh, levels = c(0, 1), labels = c("No", "Yes"))

# Categorized an aggregated sleep score of >=1 as it highlights those 
# patients whom were scored as having sleep disturbances
# on more than 1 sleep measure
data_prop$Aggregated_Disturbance_F <- factor(data_prop$Sleep_Score >=1, levels = c(FALSE, TRUE), labels = c("No", "Yes"))

# Label
label(data_prop$ESS_thresh) <- "ESS:"
label(data_prop$AIS_thresh) <- "AIS"
label(data_prop$BSS_thresh) <- "BSS"
label(data_prop$Sleep_Score_F) <- "Sleep Scores (0-3)"
label(data_prop$Aggregated_Disturbance_F) <- "Severe Sleep Disturbance (>=1)"

# Make a prevalence table showing proportion of patients categorized
# with either sleep disturbance, no disturbance measured, or missing data) 
table1(~ ESS_thresh + AIS_thresh + BSS_thresh + Sleep_Score_F + Aggregated_Disturbance_F,
       data = data_prop)

# 2.2 Frequency of Aggregated Score -------------------------------------------

head(data_prop)

# For plotting purposes, removed NA values in the Sleep_Score_F column
data_clean <- data_prop %>% filter(!is.na(Sleep_Score_F))
head(data_clean)

# Plot shows proportion of patients based on how many measures scored 
# them as sleep disturbed (i.e. score of 2 means they were categorized 
# as having sleep disturbance on two out of the three measures).
# (n = 249, 19 excluded from total sample of 268 due to missing data)
ggplot(data_clean, aes(x = Sleep_Score_F, fill = Sleep_Score_F)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust = 2) + 
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribution of Sleep Disturbance Scores",
       x = "Sleep Disturbance Score",
       y = "Number of Patients", 
       caption = str_wrap("Barplot showing number of patients per group depending
                          on combined sleep disturbance score, 0: no sleep distburance
                          measured on all surveys, 1: only 1 measurement categorized
                          sleep disturbance in the patient, 2: sleep disturbance scores
                          on 2 out of 3 surveys, 3: sleep disturbance measured 
                          on all 3 surveys (ESS, AIS, BSS).
                          Total patient sample size is 268, where 19 patients
                          were excluded due to missing data", 105)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  )

################################################################################

# 3. IDENTIFYING PREDICTORS THAT ARE ASSOCIATED WITH SLEEP DISTURBANCE

################################################################################

# Creating a new column that categorizes an aggregated sleep score of >= 1
# into a binary that identifies individuals as 'Sleep Disturbed' or not
# as it highlights those patients who were scored as having sleep disturbances on at least 1 sleep measure
data_sub <- mutate(data_sub, Sleep_Disturbance_flag = ifelse(Sleep_Score >= 1, 1, 0))

# According to the literature, age, gender, BMI, time from transplant, liver
# diagnosis and depression are all sleep disturbance predictors for liver 
# transplant patients

# Given that all 3 tests (ESS, BSS, AIS) identify sleep disturbance
# We will identify predictors based on each individual test, and on our combined
# Sleep_Disturbance_flag variable

# We will fit 2 models for each case, one with the predictors found in literature
# and another with all additional predictors. We will compare goodness of fit between
# each model to determine the best one

#### ESS THRESHOLD ####

ess_mod1 <- glm(ESS_thresh ~ gender + age + bmi + time_from_transplant + liver_diagnosis
                + depression, data = data_sub, family = binomial)
nobs(ess_mod1)
summary(ess_mod1)

ess_mod2 <- glm(ESS_thresh ~ gender + age + bmi + time_from_transplant + liver_diagnosis
                + depression + recurrence_of_disease + rejection_graft_dysfunction 
                + any_fibrosis + renal_failure + corticoid, data = data_sub, family = binomial)
nobs(ess_mod2)
summary(ess_mod2)

# Larger model is a better fit
anova(ess_mod2, ess_mod1, test = "Chisq")

#### AIS THRESHOLD ####

ais_mod1 <- glm(AIS_thresh ~ gender + age + bmi + time_from_transplant + liver_diagnosis
                + depression, data = data_sub, family = binomial)
nobs(ais_mod1)
summary(ais_mod1)

ais_mod2 <- glm(AIS_thresh ~ gender + age + bmi + time_from_transplant + liver_diagnosis
                + depression + recurrence_of_disease + rejection_graft_dysfunction 
                + any_fibrosis + renal_failure + corticoid, data = data_sub, family = binomial)
nobs(ais_mod2)
summary(ais_mod2)

# Larger model is not a better fit
anova(ais_mod2, ais_mod1, test = "Chisq")

#### BSS THRESHOLD ####

bss_mod1 <- glm(BSS_thresh ~ gender + age + bmi + time_from_transplant + liver_diagnosis
                + depression, data = data_sub, family = binomial)
nobs(bss_mod1)
summary(bss_mod1)

bss_mod2 <- glm(BSS_thresh ~ gender + age + bmi + time_from_transplant + liver_diagnosis
                + depression + recurrence_of_disease + rejection_graft_dysfunction 
                + any_fibrosis + renal_failure + corticoid, data = data_sub, family = binomial)
nobs(bss_mod2)
summary(bss_mod2)

# Larger model is not a better fit
anova(bss_mod2, bss_mod1, test = "Chisq")

#### SLEEP DISTURBANCE AGGREGATE ####

# model with the predictors from the literature
# number of observations = 226 
# p<226/15; can have 15 predictors
agg_mod1 <- glm(Sleep_Disturbance_flag ~ gender + age + bmi + time_from_transplant + liver_diagnosis 
            + depression, data = data_sub, family = binomial)
nobs(agg_mod1)
summary(agg_mod1)

# model with recurrence of disease, evidence of rejection/graft dysfunction, 
# evidence of any fibrosis, renal failure, corticosteroid
# number of observations = 226 
# p<226/15; can have 15 predictors
agg_mod2 <- glm(Sleep_Disturbance_flag ~ gender + age + bmi + time_from_transplant + liver_diagnosis 
            + depression + recurrence_of_disease + rejection_graft_dysfunction 
            + any_fibrosis + renal_failure + corticoid, data = data_sub, family = binomial)
nobs(agg_mod2)

# alpha = 0.05
# evidence of rejection/graft dysfunction, evidence of any fibrosis, renal failure, 
# corticosteroid are all p> 0.05
# BMI has p<0.05 in both models, mod1 had p<0.05 for liver_diagnosisAlcohol as well
# see which model is a better fit
summary(agg_mod2)

# likelihood ratio tests
# Null hypothesis: the larger model (mod2) does not fit the data better compared to the smaller model (mod1)
# Alternative hypothesis: the larger model (mod2) fits the data better compared to the smaller model (mod1)
# Alpha = 0.05
# Results: Model 1 has a deviance of 271.1 while Model 2 has deviance of 262.85.
# p-value = 0.1429. Since p-value>0.05, we fail to reject the null
# hypothesis. There is evidence to suggest that the larger model (mod2) does not fit the data better compared to 
# the smaller model (mod1)
anova(agg_mod2, agg_mod1, test = "Chisq")

# Likelihood ratio test suggests that agg_mod1 fits the data best

# Identified predictors for sleep disturbance based on the data: BMI, liver_diagnosisAlcohol
# The obtained value from the chosen model (mod1) 
# indicates that there is an increase of 0.098511 in the estimate 
# of the log of odds ratio for the “Sleep Disturbance” outcome, for each unit increase of BMI
