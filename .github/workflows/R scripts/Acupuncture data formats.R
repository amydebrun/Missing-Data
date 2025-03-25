# Acupuncture wide format
acu_wide <- read_csv("Data/acupuncture.csv",show_col_types = FALSE)
head(acu_wide)

# Acupuncture Long format 
acu_long <- acu_wide %>%
  pivot_longer(
    cols = c('pk2', 'pk5'), 
    names_to = c('pk_time'),
    values_to = "pk_score"
  ) %>%
  pivot_longer(
    cols = c('f2', 'f5'), 
    names_to = c('freq_time'),
    values_to = "freq_score",
    names_repair = "unique"
  ) %>%
  filter((pk_time == "pk2" & freq_time == "f2") | (pk_time == "pk5" & freq_time == "f5")) %>%
  mutate(
    time = case_when(
      pk_time == "pk2" ~ "3m",
      pk_time == "pk5" ~ "12m"
    )
  ) %>%
  select(-c(pk_time, freq_time))


#Acupuncture filtered for LOCF
acu_filtered<- acu_long%>%
  filter(time != "3m")

#MICE
acu_mice <- mice(acu_wide, m = 5, method = 'pmm', seed = 123)
acu_mice_data <- complete(data = acu_mice, action = "long", include = TRUE)

acu_mice_data_long <- acu_mice_data %>%
  pivot_longer(
    cols = c('pk2', 'pk5'), 
    names_to = c('pk_time'),
    values_to = "pk_score"
  ) %>%
  pivot_longer(
    cols = c('f2', 'f5'), 
    names_to = c('freq_time'),
    values_to = "freq_score",
    names_repair = "unique"
  ) %>%
  filter((pk_time == "pk2" & freq_time == "f2") | (pk_time == "pk5" & freq_time == "f5")) %>%
  mutate(
    time = case_when(
      pk_time == "pk2" ~ "3m",
      pk_time == "pk5" ~ "12m"
    )
  ) %>%
  select(-c(pk_time, freq_time))%>%
  group_by(.imp) %>%
  mutate(.id = 1:n())


acu_mi_obj_long <- as.mids(acu_mice_data_long)