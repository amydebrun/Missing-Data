
# Load data
acu_wide <- read_csv(".github/workflows/Data/acupuncture.csv",show_col_types = FALSE)

data_vitalkp <- read_sas(".github/workflows/Data/vitalkneepain111623.sas7bdat", NULL)
data_main <- read_sas(".github/workflows/Data/VITAL_trial_NEJM_2022.sas7bdat", NULL)
vital_wide <- data_main %>%
  select(Subject_ID, vitdactive, fishoilactive,
         sex, bmi, currsmk, ageyr, Aspirin) %>%
  right_join(data_vitalkp)

# ACU

# MICE
acu_mice <- mice(acu_wide, m = 5, method = 'pmm', seed = 123, print=FALSE)
acu_mice_random <- mice(acu_wide, m = 5, method = 'sample', seed = 123, print=FALSE)
acu_mice_midastouch <- mice(acu_wide, m = 5, method = 'midastouch', seed = 123, print=FALSE)
acu_mice_predict <- mice(acu_wide, m = 5, method = 'norm.predict', seed = 123, print=FALSE)
acu_mice_predict_noise <- mice(acu_wide, m = 5, method = 'norm.nob', seed = 123, print=FALSE)
acu_mice_bayesian <- mice(acu_wide, m = 5, method = 'norm', seed = 123, print=FALSE)

# MICE for LME
acu_mice_data_wide <- complete(data = acu_mice, action = "long", include = TRUE)
acu_mice_data_long <- to_long_format_acu_cat_MICE(acu_mice_data_wide)
acu_mice_data_obj_long <- as.mids(acu_mice_data_long)

# MICE long
acu_mice_data_long_cont <- to_long_format_acu_cont_MICE(acu_mice_data_wide)
acu_mice_data_long_cont <- acu_mice_data_long_cont %>%
  mutate(time_c = time - 12)
acu_mice_data_obj_long_cont <- as.mids(acu_mice_data_long_cont)

# Impute 20 for acupuncture
acu_mice_20 <- mice(acu_wide, m = 20, method = 'pmm', seed = 123, print=FALSE)
acu_LM_MICE_default_20 <- with(acu_mice_20, lm(pk5 ~ group + pk1)) 
acu_LM_MICE_default_pool_20 <- pool(acu_LM_MICE_default_20)

# Impute 25 for acupuncture
acu_mice_25 <- mice(acu_wide, m = 25, method = 'pmm', seed = 123, print=FALSE)
acu_LM_MICE_default_25 <- with(acu_mice_25, lm(pk5 ~ group + pk1)) 
acu_LM_MICE_default_pool_25 <- pool(acu_LM_MICE_default_25)

# VITAL

# MICE
vital_mice <- mice(vital_wide, m = 5, method = 'pmm', seed = 123, print=FALSE)
vital_mice_random <- mice(vital_wide, m = 5, method = 'sample', seed = 123, print=FALSE)
vital_mice_midastouch <- mice(vital_wide, m = 5, method = 'midastouch', seed = 123, print=FALSE)
vital_mice_predict <- mice(vital_wide, m = 5, method = 'norm.predict', seed = 123, print=FALSE)
vital_mice_predict_noise <- mice(vital_wide, m = 5, method = 'norm.nob', seed = 123, print=FALSE)
vital_mice_bayesian <- mice(vital_wide, m = 5, method = 'norm', seed = 123, print=FALSE)

# MICE FOR LME
vital_mice_data <- complete(vital_mice, action = "long", include = TRUE)
vital_mice_data_long <- to_long_format_vital_mice(vital_mice_data)

# Convert long format back to mids objectives

vital_mice_obj_long <- as.mids(vital_mice_data_long)

# Impute 20 for VITAL
vital_mice_20 <- mice(vital_wide, m = 20, method = 'pmm', seed = 123, print=FALSE)
vital_MI_SLR_20 <- with(vital_mice_20, lm(pain_yr4 ~ fishoilactive + vitdactive  + pain_base ))
vital_MI_SLR_pool_20 <- pool(vital_MI_SLR_20)

# Impute 50 for VITAL
vital_mice_50 <- mice(vital_wide, m = 50, method = 'pmm', seed = 123, print=FALSE)
vital_MI_SLR_50 <- with(vital_mice_50, lm(pain_yr4 ~ fishoilactive + vitdactive  + pain_base ))
vital_MI_SLR_pool_50 <- pool(vital_MI_SLR_50)








# Save impute data

save(
  # ACU
  acu_mice,
  acu_mice_random,
  acu_mice_midastouch,
  acu_mice_predict,
  acu_mice_predict_noise,
  acu_mice_bayesian,
  acu_mice_data_obj_long,
  acu_mice_data_obj_long_cont,
  acu_LM_MICE_default_pool_20,
  acu_LM_MICE_default_pool_25,
     
  #VITAL
  vital_mice,
  vital_mice_random,
  vital_mice_midastouch,
  vital_mice_predict,
  vital_mice_predict_noise,
  vital_mice_bayesian,
  vital_mice_obj_long,
  vital_MI_SLR_pool_20,
  vital_MI_SLR_pool_50,
     
     
  file = ".github/workflows/imputations.RData" 
  )















