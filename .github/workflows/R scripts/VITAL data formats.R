# Vital data wide 
data_vitalkp <- read_sas("Data/vitalkneepain111623.sas7bdat", NULL)
data_main <- read_sas("Data/VITAL_trial_NEJM_2022.sas7bdat", NULL)
vital_wide <- data_main %>%
  select(Subject_ID, vitdactive, fishoilactive,
         sex, bmi, currsmk, ageyr, Aspirin) %>%
  right_join(data_vitalkp)

# Vital data Long
vital_long <- vital_wide %>%
  pivot_longer(cols = matches("_yr[[:digit:]]$"),
               names_to = c(".value", "time"), 
               names_sep = "_") %>%
  mutate(time_contin = as.integer(gsub("yr", "", time)),
         time_contin_cent = time_contin - 4)

# MI
vital_mice <- mice(vital_wide, m = 5, method = 'pmm', seed = 123)
vital_mice_data <- complete(vital_mice, action = "long", include = TRUE)

# Convert MI to long format
vital_mice_data_long <- vital_mice_data %>%
  pivot_longer(cols = matches("_yr[[:digit:]]$"),
               names_to = c(".value", "time"), 
               names_sep = "_") %>%
  group_by(.imp) %>%
  mutate(.id = 1:n()) %>%
  mutate(time_contin = as.integer(gsub("yr", "", time)),
         time_contin_cent = time_contin - 4)
# Convert long format back to mids objectives
vital_mice_obj_long <- as.mids(vital_mice_data_long)