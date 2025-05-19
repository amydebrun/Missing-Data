# Acupuncture wide format
acu_wide <- read_csv("Data/acupuncture.csv",show_col_types = FALSE)
head(acu_wide)

# Acupuncture Long format 
acu_long <- to_long_format_acu_cont(acu_wide)

# MICE
acu_mice <- mice(acu_wide, m = 5, method = 'pmm', seed = 123)
acu_mice_random <- mice(acu_wide, m = 5, method = 'sample', seed = 123)
acu_mice_midastouch <- mice(acu_wide, m = 5, method = 'midastouch', seed = 123)
acu_mice_predict <- mice(acu_wide, m = 5, method = 'norm.predict', seed = 123)
acu_mice_predict_noise <- mice(acu_wide, m = 5, method = 'norm.nob', seed = 123)
acu_mice_bayesian <- mice(acu_wide, m = 5, method = 'norm', seed = 123)

# MICE for LME
acu_mice_data_wide <- complete(data = acu_mice, action = "long", include = TRUE)
acu_mice_data_long <- to_long_format_acu_cat_MICE(acu_mice_data_wide)
acu_mice_data_obj_long <- as.mids(acu_mice_data_long)

# Making time continuous
# long
acu_long_cont <- to_long_format_acu_cont(acu_wide)
# MICE long
acu_mice_data_long_cont <- to_long_format_acu_cont_MICE(acu_mice_data_wide)
acu_mice_data_obj_long_cont <- as.mids(acu_mice_data_long_cont)
