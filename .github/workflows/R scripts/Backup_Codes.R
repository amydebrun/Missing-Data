# Old Vitals

#Vital data wide 
data_vitalkp <- read_sas("Data/vitalkneepain111623.sas7bdat", NULL)
data_main <- read_sas("Data/VITAL_trial_NEJM_2022.sas7bdat", NULL)
vital_wide<- data_main %>%
  dplyr::select(Subject_ID, vitdactive, fishoilactive, sex, ageyr, ) %>%
  right_join(data_vitalkp, by = "Subject_ID") %>%
  select(1:20)
vital_wide$sex <- factor(vital_wide$sex, labels = c("M", "F"))

# Vital data Long
vital_long <- vital_wide %>%
  pivot_longer(
    cols = c('pain_yr1','pain_yr2', 'pain_yr3', 'pain_yr4'), 
    names_to = c('pain_time'),
    values_to = "pain_score"
  ) %>%
  pivot_longer(
    cols = c('stiffness_yr1', 'stiffness_yr2', 'stiffness_yr3', 'stiffness_yr4'), 
    names_to = c('stiffness_time'),
    values_to="stiffness_score",
    names_repair = "unique"
  ) %>%
  pivot_longer(
    cols = c('function_yr1', 'function_yr2', 'function_yr3', 'function_yr4'), 
    names_to = c('function_time'),
    values_to = "function_score",
    names_repair = "unique"
  ) %>%
  filter((pain_time == "pain_yr1" & stiffness_time == "stiffness_yr1" & function_time == "function_yr1")| (pain_time == "pain_yr2" & stiffness_time == "stiffness_yr2" & function_time == "function_yr2") | (pain_time == "pain_yr3" & stiffness_time == "stiffness_yr3" & function_time == "function_yr3") | (pain_time == "pain_yr4" & stiffness_time == "stiffness_yr4" & function_time == "function_yr4")) %>%
  mutate( 
    time=case_when(
      pain_time == "pain_yr1" ~ "Year1",
      pain_time == "pain_yr2" ~ "Year2",
      pain_time == "pain_yr3" ~ "Year3",
      pain_time == "pain_yr4" ~ "Year4",
      stiffness_time == "stiffness_yr1" ~ "Year1",
      stiffness_time == "stiffness_yr2" ~ "Year2",
      stiffness_time == "stiffness_yr3" ~ "Year3",
      stiffness_time == "stiffness_yr4" ~ "Year4",
      function_time == "function_yr1" ~ "Year1",
      function_time == "function_yr2" ~ "Year2",
      function_time == "function_yr3" ~ "Year3",
      function_time == "function_yr4" ~ "Year4"
    )
  ) %>%  select(-c(pain_time, stiffness_time, function_time))

-------------------

# Neil's Vital MI

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
