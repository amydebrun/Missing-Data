# Vital data wide 
data_vitalkp <- read_sas("Data/vitalkneepain111623.sas7bdat", NULL)
data_main <- read_sas("Data/VITAL_trial_NEJM_2022.sas7bdat", NULL)
vital_wide <- data_main %>%
  select(Subject_ID, vitdactive, fishoilactive,
         sex, bmi, currsmk, ageyr, Aspirin) %>%
  right_join(data_vitalkp)

# Vital data Long
vital_long <- to_long_format_vital(vital_wide)

# MICE
vital_mice <- mice(vital_wide, m = 5, method = 'pmm', seed = 123)
vital_mice_random <- mice(vital_wide, m = 5, method = 'sample', seed = 123)
vital_mice_midastouch <- mice(vital_wide, m = 5, method = 'midastouch', seed = 123)
vital_mice_predict <- mice(vital_wide, m = 5, method = 'norm.predict', seed = 123)
vital_mice_predict_noise <- mice(vital_wide, m = 5, method = 'norm.nob', seed = 123)
vital_mice_bayesian <- mice(vital_wide, m = 5, method = 'norm', seed = 123)

# MICE FOR LME
vital_mice_data <- complete(vital_mice, action = "long", include = TRUE)
vital_mice_data_long <- to_long_format_vital_mice(vital_mice_data)
# Convert long format back to mids objectives
vital_mice_obj_long <- as.mids(vital_mice_data_long)

#rule of thumb for imputation

vital_miss <- sum(is.na(vital_long))
vital_observed <- sum(!is.na(vital_long))
percent_miss <- vital_miss / (vital_miss + vital_observed)
percent_miss

