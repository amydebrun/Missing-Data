# Vital data wide 
data_vitalkp <- read_sas("Data/vitalkneepain111623.sas7bdat", NULL)
data_main <- read_sas("Data/VITAL_trial_NEJM_2022.sas7bdat", NULL)
vital_wide <- data_main %>%
  select(Subject_ID, vitdactive, fishoilactive,
         sex, bmi, currsmk, ageyr, Aspirin) %>%
  right_join(data_vitalkp)

# Vital data Long
vital_long <- to_long_format_vital(vital_wide)

#rule of thumb for imputation

vital_miss <- sum(is.na(vital_long))
vital_observed <- sum(!is.na(vital_long))
percent_miss <- vital_miss / (vital_miss + vital_observed)
percent_miss

