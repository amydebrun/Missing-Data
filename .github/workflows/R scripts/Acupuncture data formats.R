# Acupuncture wide format
acu_wide <- read_csv("acupuncture.csv",show_col_types = FALSE)
head(acu_wide)

# Acupuncture Long format 
acu_long <- to_long_format_acu_cat(acu_wide)

# Making time continuous
# long
acu_long_cont <- to_long_format_acu_cont(acu_wide)
acu_long_cont <- acu_long_cont %>%
  mutate(time_c = time - 12)

#rule of thumb for imputation
acu_miss <- sum(is.na(acu_long))
acu_observed <- sum(!is.na(acu_long))
percent_miss <- acu_miss / (acu_miss + acu_observed)
