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
