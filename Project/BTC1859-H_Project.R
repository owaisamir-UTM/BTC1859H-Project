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

# Epworth scale only goes up to 24, so values over that will be treated as NA.
data_sub$epworth_sleepiness_scale[data_sub$epworth_sleepiness_scale > 24] <- NA
summary(data_sub)

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
ess_mean <- mean(data_sub$epworth_sleepiness_scale, na.rm = TRUE)
ess_sd <- sd(data_sub$epworth_sleepiness_scale, na.rm = TRUE)
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
       fill = "ESS Interpretation") +
  theme_minimal() +
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
ais_mean <- mean(data_sub$athens_insomnia_scale, na.rm = TRUE)
ais_sd <- sd(data_sub$athens_insomnia_scale, na.rm = TRUE)
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
       fill = "AIS Interpretation") +
  theme_minimal() +
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
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

################################################################################

# Generating histogram plots to show the distribution of QoL scores

################################################################################

# SF36-PCS Plot
sf36p_mean <- mean(data_sub$sf36_pcs, na.rm = TRUE)
sf36p_sd <- sd(data_sub$sf36_pcs, na.rm = TRUE)
sf36p_label <- paste0("Mean ± SD\n", round(sf36p_mean, 2), " ± ", round(sf36p_sd, 2))

ggplot(data_sub, aes(x = as.numeric(sf36_pcs))) +
  geom_histogram(binwidth = 2, color = "black", na.rm = TRUE, fill ="darkgreen") +
  labs(title = "Distribution of SF36-PCS Scores",
       x = "SF36-PCS Score",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0, 100)) +
  annotate("label",
           x = 100,
           y = 11,
           label = sf36p_label,
           hjust = 1, vjust = 0, size = 4)
rm("sf36p_label", "sf36p_mean", "sf36p_sd")

# SF36-MCS Plot
sf36m_mean <- mean(data_sub$sf36_mcs, na.rm = TRUE)
sf36m_sd <- sd(data_sub$sf36_mcs, na.rm = TRUE)
sf36m_label <- paste0("Mean ± SD\n", round(sf36m_mean, 2), " ± ", round(sf36m_sd, 2))

ggplot(data_sub, aes(x = as.numeric(sf36_mcs))) +
  geom_histogram(binwidth = 2, color = "black", na.rm = TRUE, fill ="darkgreen") +
  labs(title = "Distribution of SF36-MCS Scores",
       x = "SF36-MCS Score",
       y = "Frequency") +
  theme_minimal() +
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
         sf36_pcs + sf36_mcs, data = data_sub)

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


# Evaluating Linear Regression Assumptions 

# The linear regression models are based on a few assumptions, including homoscedasticity (constant variance), normality, and linearity
# Residuals are being plotted to visually check for any patterns
plot(resid(lm_physical))    #random scatter, no signs of homoscedasticity or non-linearity
plot(resid(lm_mental))      #random scatter, no signs of homoscedasticity or non-linearity

# Plotting the Q-Q plot to check for Normality of residuals
qqnorm(resid(lm_physical))
qqline(resid(lm_physical), col = "red")    #most of the points lie on the reference line in red; Normality assumption holds true

qqnorm(resid(lm_mental))
qqline(resid(lm_mental), col = "red")      #most of the points lie on the reference line in red, however there are slight deviations at the ends; Normality assumption is still assumed to hold true


# MANOVA Analysis: "Sleep_Score" on both quality of life components

# To look at combined effect, the "sf36_pcs" and "sf36_mcs" scores are NOT combined/aggregated into a global QoL score as the SF-36 tests was designed to measure the two components separately,
# and studies recommend not doing a global QoL score as it leads to bias and lowers the validity of the study (https://pmc.ncbi.nlm.nih.gov/articles/PMC5052926/, https://pubmed.ncbi.nlm.nih.gov/9817107/)
# Thus, the effect of "Sleep_Score" on both quality of life measures together is done using "manova()"; 
# MANOVA is multivariate ANOVA which considers 2+ dependent (continuous) variables together with 1+ independent variable (it also takes into account the correlation between the dependent variables)
# The null hypothesis is that there is no difference in the means of the dependent variables across the groups 

manova_model <- manova(cbind(sf36_pcs, sf36_mcs) ~ Sleep_Score, data = data_sub) 
summary(manova_model, test = "Wilks")       #Wilks is used most commonly to find the variance in the dependent variables NOT explained by the independent variable
                                            #F = 53.42, Wilks = 0.683, p-value < 2.2e-16

# Summary: 
# The MANOVA shows that the "Sleep_Score" has an effect on the combined "sf36_pcs" and "sf36_mcs" scores (F = 53.42)
# Wilk's Lambda = 0.683, which indicates that 68.3% variance in the mental and physical QoL scores is not explained by the sleep score; or that 31.7% variance in the two dependent variables IS explained by the "Sleep_Score"
# As the p-value (< 2.2e-16) is much lesser than the alpha value of 0.05, there is strong evidence to reject the null hypothesis and suggest a significant effect
# Thus, it shows a significant effect of "Sleep_Score" on combined mental and physical QoL, meaning that overall QoL differs significantly across levels of the sleep scores


################################################################################

# 2. ESTIMATION OF THE PREVALENCE OF SLEEP DISTURBANCE

################################################################################

head(data_long)

# Find prevalence based on each score using mean() since the values are binary.
ESS_prevalence <- mean(data_sub$ESS_thresh, na.rm = TRUE)*100
AIS_prevalence <- mean(data_sub$AIS_thresh, na.rm = TRUE)*100
BSS_prevalence <- mean(data_sub$BSS_thresh, na.rm = TRUE)*100

# Also, calculated the prevalence by categorizing "Sleep Disturbance"
# for aggregated sleep scores of >= 2. 
# The aggregated sleep score sums the scores from ESS, AIS, BSS,
# providing a comprehensive perspective on sleep disturbance.
agg_disturbance_prev <- mean(data_sub$Sleep_Score >= 2, na.rm = TRUE) * 100

# Created a separated data frame for plotting the prevalence.
prev_df <- data.frame(
  Measure = c("ESS", "AIS", "BSS", "Aggregated Score"),
  Prevalence = c(ESS_prevalence, AIS_prevalence, BSS_prevalence, agg_disturbance_prev)
)
prev_df

# Bar plot showing prevalence based on 3 sleep scores, and the aggregated measure.
# (PSQI measure was excluded)
# Added percentage labels for prevalence values above each respective bar.
ggplot(prev_df, aes(x = Measure, y = Prevalence, fill = Measure)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(Prevalence, 1), "%")),
            vjust = -0.5, size = 4) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(
    title = "Prevalence of Sleep Disturbance by Measurement",
    x = "Sleep Measurement",
    y = "Prevalence (%)",  
    caption = str_wrap("Barplot depicting the prevelance of sleep disturbance in
                       patients with liver transplant based on various sleep
                       measurements (AIS, BSS, ESS and an aggregated score of
                       these measurements)", 100)
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)
  ) + 
  scale_fill_brewer(palette = "Pastel1")

# Creating proportion tables ----------------------------------------------

# Copied data set for proportion tables and plots
data_prop <- data_sub

# Convert Sleep_Score to a factor for nice labeling in table1
data_prop$Sleep_Score_F <- factor(
  data_prop$Sleep_Score,
  levels = 0:3,
  labels = c("0: None",
             "1: Low",
             "2: Moderate",
             "3: High")
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

# Categorized an aggregated sleep score of >= 2 as it highlights those 
# patients whom were scored as having sleep disturbances
# on more than 1 sleep measure
data_prop$Aggregated_Disturbance_F <- factor(data_prop$Sleep_Score >= 2, levels = c(FALSE, TRUE), labels = c("No", "Yes"))

# Label
label(data_prop$ESS_thresh) <- "ESS:"
label(data_prop$AIS_thresh) <- "AIS"
label(data_prop$BSS_thresh) <- "BSS"
label(data_prop$Sleep_Score_F) <- "Sleep Scores (0-3)"
label(data_prop$Aggregated_Disturbance_F) <- "Severe Sleep Disturbance (>= 2)"

# Make a prevalence table showing proportion of patients categorized
# with either sleep disturbance, no disturbance measured, or missing data) 
table1(~ ESS_thresh + AIS_thresh + BSS_thresh + Sleep_Score_F + Aggregated_Disturbance_F,
       data = data_prop)

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
  geom_text(stat='count', aes(label=..count..), vjust = -0.5) + 
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribution of Sleep Disturbance Scores",
       x = "Sleep Disturbance Score",
       y = "Number of Patients", 
       caption = str_wrap("Barplot showing number of patients per group depending
                          on combined sleep disturbance score, none - high.
                          Total patient sample size is 268, where 19 patients
                          were excluded due to missing data", 105)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0)
    )

