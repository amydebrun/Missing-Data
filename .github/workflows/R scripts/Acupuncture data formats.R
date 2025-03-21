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
