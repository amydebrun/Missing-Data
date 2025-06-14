# Acupuncture wide format
acu_wide <- read_csv("Data/acupuncture.csv",show_col_types = FALSE)
head(acu_wide)

# Acupuncture Long format 
acu_long <- to_long_format_acu_cat(acu_wide)



#rule of thumb for imputation
acu_miss <- sum(is.na(acu_long))
acu_observed <- sum(!is.na(acu_long))
percent_miss <- acu_miss / (acu_miss + acu_observed)
