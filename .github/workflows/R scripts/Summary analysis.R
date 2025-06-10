
#Summary
acu_summary <- acu_wide %>% tbl_summary(
  by = group,
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n}({p}%)"),
  missing = "ifany",
  missing_text = "Missing"
)

vital_summary <- vital_wide %>% 
  mutate(
    group = case_when(
      vitdactive == 0 & fishoilactive == 0 ~ "Neither",
      vitdactive == 1 & fishoilactive == 0 ~ "Vitamin D only",
      vitdactive == 0 & fishoilactive == 1 ~ "Fish Oil only",
      vitdactive == 1 & fishoilactive == 1 ~ "Both")) %>%
  select(-c(Subject_ID, vitdactive, fishoilactive,
            matches("stiffness_"),
            matches("function_"))) %>%
  tbl_summary(
    by = group,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}({p}%)"),
    missing = "ifany",
    missing_text = "Missing"
  )

# Missing plot
acu_miss_plot <- gg_miss_var(acu_wide) 
vital_miss_plot <- gg_miss_var(vital_wide %>% select(-c(Subject_ID, vitdactive, fishoilactive,
                                     matches("stiffness_"),
                                     matches("function_")))) 

# Missing indicator
acu_wide_miss <- acu_wide %>%
  mutate(pk2_miss = ifelse(is.na(pk2), 1, 0)) %>%
  mutate(pk5_miss = ifelse(is.na(pk5), 1, 0)) %>%
  mutate(miss_pattern = interaction(pk2_miss,pk5_miss,sep="_")) %>%
  select(-c(f1, f2, f5))

vital_wide_miss <- vital_wide %>%
  mutate(pain_base_miss = ifelse(is.na(pain_base), 1, 0)) %>%
  mutate(pain_yr1_miss = ifelse(is.na(pain_yr1), 1, 0)) %>%
  mutate(pain_yr2_miss = ifelse(is.na(pain_yr2), 1, 0)) %>%
  mutate(pain_yr3_miss = ifelse(is.na(pain_yr3), 1, 0)) %>%
  mutate(pain_yr4_miss = ifelse(is.na(pain_yr4), 1, 0)) %>%
  mutate(
    group = case_when(
      vitdactive == 0 & fishoilactive == 0 ~ "Neither",
      vitdactive == 1 & fishoilactive == 0 ~ "Vitamin D only",
      vitdactive == 0 & fishoilactive == 1 ~ "Fish Oil only",
      vitdactive == 1 & fishoilactive == 1 ~ "Both")) %>%
  mutate(miss_pattern = interaction(pain_base_miss,
                                    pain_yr1_miss,
                                    pain_yr2_miss,
                                    pain_yr3_miss,
                                    pain_yr4_miss,
                                    sep = ",")) %>%
  select(-c(matches("stiffness_"), matches("function_")))

# Missing pattern
acu_wide_miss_plot <- acu_wide_miss %>% 
  ggplot(aes(x = miss_pattern, fill = factor(group))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Missing Data Patterns by Treatment group",
    x = "Missing Pattern (pk2_miss_pk5_miss)",
    y = "Count",
    fill = "Treatment Group"
  ) +
  scale_fill_manual(values = c("0" = "#a80050", "1" = "lawngreen"),
                    labels = c("0" = "Placebo", "1" = "Acupuncture")) +  
  theme_minimal()


vital_wide_miss_plot<- vital_wide_miss %>% ggplot(aes(x = miss_pattern, fill = factor(group))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Missing Data Patterns by Group",
    x = "Missing Pattern",
    y = "Count",
    fill = "Group"
  ) + 
  scale_fill_manual(values = c("Neither" = "#a80050", "Vitamin D only" = "lawngreen", 
                               "Fish Oil only" = "olivedrab", "Both" = "#84003d")) +
  theme_minimal() +
  coord_flip()

# MAR
acu_mar_pk2 <- summary(glm(
  pk2_miss ~ age + sex + migraine + chronicity + acupuncturist + practice_id + group + pk1 + pk5,
  data = acu_wide_miss,
  family = binomial
))

acu_mar_pk5 <- summary(glm(
  pk5_miss ~ age + sex + migraine + chronicity + acupuncturist + practice_id + group + pk1 + pk2,
  data = acu_wide_miss,
  family = binomial
))

vital_mar_base <- summary(glm(
  pain_base_miss ~ sex + ageyr + vitdactive + fishoilactive + bmi + currsmk + Aspirin +
    KneesReplacedV2 + unikneepain + bikneepain + kneepainfreq +
    tylenolbase + nsaidsbase + strongerbase + tkrf + group,
  data = vital_wide_miss,
  family = binomial
))

vital_mar_yr1 <- summary(glm(
  pain_yr1_miss ~ sex + ageyr + vitdactive + fishoilactive + bmi + currsmk + Aspirin +
    KneesReplacedV2 + unikneepain + bikneepain + kneepainfreq +
    tylenolbase + nsaidsbase + strongerbase + tkrf + group,
  data = vital_wide_miss,
  family = binomial
))

vital_mar_yr2 <- summary(glm(
  pain_yr2_miss ~ sex + ageyr + vitdactive + fishoilactive + bmi + currsmk + Aspirin +
    KneesReplacedV2 + unikneepain + bikneepain + kneepainfreq +
    tylenolbase + nsaidsbase + strongerbase + tkrf + group,
  data = vital_wide_miss,
  family = binomial
))

vital_mar_yr3 <- summary(glm(
  pain_yr3_miss ~ sex + ageyr + vitdactive + fishoilactive + bmi + currsmk + Aspirin +
    KneesReplacedV2 + unikneepain + bikneepain + kneepainfreq +
    tylenolbase + nsaidsbase + strongerbase + tkrf + group,
  data = vital_wide_miss,
  family = binomial
))

vital_mar_yr4 <- summary(glm(
  pain_yr4_miss ~ sex + ageyr + vitdactive + fishoilactive + bmi + currsmk + Aspirin +
    KneesReplacedV2 + unikneepain + bikneepain + kneepainfreq +
    tylenolbase + nsaidsbase + strongerbase + tkrf + group,
  data = vital_wide_miss,
  family = binomial
))

LRT_glm_missing <- data.frame(Variables = c("pk2", "pk5", "pain_base", "pain_yr1",
                                            "pain_yr2", "pain_yr3", "pain_yr4"),
                              LRT_p = c(lrt_from_glm(acu_mar_pk2),
                                        lrt_from_glm(acu_mar_pk5),
                                        lrt_from_glm(vital_mar_base),
                                        lrt_from_glm(vital_mar_yr1),
                                        lrt_from_glm(vital_mar_yr2),
                                        lrt_from_glm(vital_mar_yr3),
                                        lrt_from_glm(vital_mar_yr4)))


# plots from poster


data_vitalkp <- read_sas("Data/vitalkneepain111623.sas7bdat", 
                         NULL)
data_main <- read_sas("Data/VITAL_trial_NEJM_2022.sas7bdat", 
                      NULL)

data_combined <- data_main %>%
  select(Subject_ID, vitdactive, fishoilactive) %>%
  right_join(data_vitalkp, by = "Subject_ID")

data_miss <- data_combined %>%
  select(Subject_ID, tkrf, vitdactive, fishoilactive, starts_with("pain_")) %>%
  mutate(across(starts_with("pain_"), ~ as.numeric(!is.na(.x)), .names = "missing_{.col}")) %>%
  unite("miss_pattern", starts_with("missing_"), remove = FALSE) %>%
  rowwise() %>% 
  mutate(miss_count = sum(c_across(starts_with("missing_pain_")))) %>%
  group_by(miss_pattern) %>%
  mutate(pattern_freq = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(miss_pattern = gsub("_", ", ", miss_pattern),
         miss_pattern_label = paste0("Pattern: ", miss_pattern, ", (n=", pattern_freq , ")", collapse = "")) %>%
  pivot_longer(starts_with("pain_"), names_to = "time", names_prefix = "pain_", 
               values_to = "pain") %>%
  mutate(
    miss_obs = as.factor(if_else(is.na(pain),0,1)),
    time_cont = case_when(
      time=="base" ~ 0,
      time=="yr1" ~ 1,
      time=="yr2" ~ 2,
      time=="yr3" ~ 3,
      time=="yr4" ~ 4
    )) %>%
  arrange(desc(miss_pattern), Subject_ID) %>%
  mutate(miss_pattern_label = factor(miss_pattern_label, levels = unique(miss_pattern_label)),
         group_vitd = factor(vitdactive, levels = c(0,1), labels = c("Placebo","Vitamin D")),
         group_oil = factor(fishoilactive, levels = c(0,1), labels = c("Placebo","Fish Oil")))

pct_format = scales::label_percent(accuracy = .1)
vital_miss_perc_plot<-suppressMessages(
  data_miss %>%
    group_by(time, miss_obs) %>%
    summarise(count_pat = n(), .groups="keep") %>%
    group_by(time) %>%
    mutate(total_pat = sum(count_pat), perc_pat = pct_format(count_pat/total_pat)) %>%
    ungroup %>%
    ggplot(aes(x = time, fill = miss_obs, y = count_pat)) +
    geom_col() +  
    stat_identity(geom = "text", colour = "white", size = 8,
                  aes(label =perc_pat), 
                  position = position_stack(vjust=0.5)) +
    scale_fill_manual(name="",values=c("olivedrab", "lawngreen"), labels=c("Missing Data", "Measured Data")) +
    scale_x_discrete(name = "Time", labels = c("base" = "Baseline", "yr1" = "Year 1", "yr2" = "Year 2", "yr3" = "Year 3", "yr4"="Year 4")) + 
    scale_y_continuous(name = "Number of Patients") +
    theme(text = element_text(size = 24),
          axis.text.x = element_text(angle = 0, vjust = 0.5),
          legend.position = "top", legend.text=element_text(size=20))
)

vital_miss_perc_plot_fish<-suppressMessages(
  data_miss %>%
    group_by(time, miss_obs, group_oil) %>%
    summarise(count_pat = n(), .groups="keep") %>%
    group_by(time, group_oil) %>%
    mutate(total_pat = sum(count_pat), perc_pat = pct_format(count_pat/total_pat)) %>%
    ungroup %>%
    ggplot(aes(x = time, fill = miss_obs, y = count_pat)) +
    geom_col() +
    facet_grid(~group_oil) + 
    stat_identity(geom = "text", colour = "white", size = 5,
                  aes(label =perc_pat), 
                  position = position_stack(vjust=0.5)) +
    scale_fill_manual(name="",values=c("olivedrab", "lawngreen"))+
    scale_x_discrete(name = "Time", labels = c("base" = "Baseline", "yr1" = "Year 1", "yr2" = "Year 2", "yr3" = "Year 3", "yr4"="Year 4")) + 
    scale_y_continuous(name = "Number of Patients") +
    theme(text = element_text(size = 24),
          axis.text.x = element_text(angle = 0, vjust = 0.5), legend.position = "none")
)

data_miss_count <- data_miss %>%
  select(Subject_ID, miss_pattern, pain, time_cont, group_oil, miss_obs) %>%
  group_by(group_oil, miss_pattern, miss_obs, time_cont) %>%
  summarise(pattern_count = n()) %>%
  ungroup %>%
  mutate(pattern_perc = round(pattern_count/sum(pattern_count)*100*5,1))

vital_miss_pattern_fish<-ggplot(data_miss_count, aes(x = time_cont, y = miss_pattern, fill = miss_obs)) +
  geom_tile(color = "white") + 
  geom_text(data = filter(data_miss_count, time_cont == 4), 
            aes(x = time_cont+1.5, label = paste0(pattern_count," (", pattern_perc, "%)")), size = 4) +
  facet_grid(~group_oil, scales = "free", switch = "y") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, vjust = 0.5) , legend.position = "none") +
  coord_cartesian(xlim = c(-0.5, 6.2), clip = 'off') +
  scale_y_discrete(name = "Missing Data Pattern") +
  scale_fill_discrete(name = "Data Observed\nIndicator (R)", ) +
  scale_x_continuous(name = "Time", breaks = c(0,1,2,3,4,5), 
                     labels = c("Baseline","Year 1","Year 2","Year 3","Year 4","Patient Count\n(%)")) +
  scale_fill_manual(name="",labels=c("Missing Data","Measured Data"),values=c("olivedrab", "lawngreen"))

acupuncture <- read_csv("Data/acupuncture.csv",show_col_types = FALSE)
acupuncture_miss <- acupuncture %>%
  select(id, group, pk1, pk2, pk5) %>%
  mutate(across(starts_with("pk"), ~ as.numeric(!is.na(.x)), .names = "missing_{.col}")) %>%
  unite("miss_pattern", starts_with("missing_"), remove = FALSE) %>%
  rowwise() %>% 
  mutate(miss_count = sum(c_across(starts_with("missing_pain_")))) %>%
  group_by(miss_pattern) %>%
  mutate(pattern_freq = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(miss_pattern = gsub("_", ", ", miss_pattern),
         miss_pattern_label = paste0("Pattern: ", miss_pattern, ", (n=", pattern_freq , ")", collapse = "")) %>%
  pivot_longer(starts_with("pk"), names_to = "time", names_prefix = "pk", 
               values_to = "pk") %>%
  mutate(
    miss_obs = as.factor(if_else(is.na(pk),0,1)),
    time_cont = case_when(
      time==1 ~ 0,
      time==2 ~ 1,
      time==5 ~ 2
    )) %>%
  arrange(desc(miss_pattern), id) %>%
  mutate(miss_pattern_label = factor(miss_pattern_label, levels = unique(miss_pattern_label)))

acu_miss_perc_plot<- acupuncture_miss %>%
  group_by(time, miss_obs) %>%
  summarise(count_pat = n(), .groups = "keep") %>%
  group_by(time) %>%
  mutate(total_pat = sum(count_pat), perc_pat = pct_format(count_pat/total_pat)) %>%
  ungroup %>%
  ggplot(aes(x = time, fill = miss_obs, y = count_pat)) +
  geom_col() +
  stat_identity(geom = "text", colour = "white", size = 10,
                aes(label =perc_pat), 
                position = position_stack(vjust=0.5)) +
  scale_fill_manual(name="",values=c("olivedrab", "lawngreen"), labels=c("Missing Data", "Measured Data")) + 
  scale_x_discrete(name = "Time", labels = c("1" = "Baseline", "2" = "3 Months", "5" = "12 Months")) +
  scale_y_continuous(name = "Number of Patients") +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, vjust = 0.5), legend.position = "top", legend.text=element_text(size=20))

acu_miss_perc_group<-acupuncture_miss %>%
  group_by(time, miss_obs, group) %>%
  summarise(count_pat = n(), .groups="keep") %>%
  group_by(time, group) %>%
  mutate(total_pat = sum(count_pat), perc_pat = pct_format(count_pat/total_pat)) %>%
  ungroup %>%
  ggplot(aes(x = time, fill = miss_obs, y = count_pat)) +
  geom_col() + 
  facet_grid(.~group, labeller=labeller(group=c("0" = "Placebo", "1" = "Acupuncture"))) + 
  stat_identity(geom = "text", colour = "white", size = 10,
                aes(label =perc_pat), 
                position = position_stack(vjust=0.5)) +
  scale_fill_manual(name="Data",values=c("olivedrab", "lawngreen"), labels=c("Missing", "Measured")) + 
  scale_x_discrete(name = "Time", labels = c("1" = "Baseline", "2" = "3 Months", "5" = "12 Months")) +
  scale_y_continuous(name = "Number of Patients") +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, vjust = 0.5),legend.position = "none",strip.text.x=element_text(size=20))

acupuncture_miss_count <- acupuncture_miss %>%
  select(id, miss_pattern, pk, time_cont, group, miss_obs) %>%
  group_by(group, miss_pattern, miss_obs, time_cont) %>%
  summarise(pattern_count = n(), .groups="keep") %>%
  ungroup %>%
  mutate(pattern_perc = round(pattern_count/sum(pattern_count)*100*5,1))


acu_miss_pattern_plot<-ggplot(acupuncture_miss_count, aes(x = time_cont, y = miss_pattern, fill = miss_obs)) +
  geom_tile(color = "white") + 
  geom_text(data = filter(acupuncture_miss_count, time_cont == 2), 
            aes(x = time_cont+1, label = paste0(pattern_count," (", pattern_perc, "%)")), size = 5.5) +
  facet_grid(~group, scales = "free", switch = "y",labeller=labeller(group=c("0" = "Placebo", "1" = "Acupuncture"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, vjust = 0.5), legend.position = "none",strip.text.x=element_text(size=20)) +
  coord_cartesian(xlim = c(-0.5, 3.2), clip = 'off') +
  scale_y_discrete(name = "Missing Data Pattern") +
  scale_fill_manual(name = "", labels=c("Missing","Measured"), values=c("olivedrab","lawngreen")) +
  scale_x_continuous(name = "Time", breaks = c(0.1,1.1,2.1,3.1), 
                     labels = c("Baseline","3 Months","12 Months","Patients \n(%)")) 





save(acu_summary, vital_summary, acu_miss_pattern_plot, acu_miss_perc_plot, acu_miss_perc_group, vital_miss_perc_plot, vital_miss_plot,
     vital_miss_perc_plot_fish, vital_miss_pattern_fish, file = "report_plots.RData")

