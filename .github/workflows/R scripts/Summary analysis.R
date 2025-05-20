
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
acu_wide_miss_plot<- acu_wide_miss %>% ggplot(aes(x = miss_pattern, fill = factor(group))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Missing Data Patterns by Group",
    x = "Missing Pattern (pk2_miss_pk5_miss)",
    y = "Count",
    fill = "Group"
  ) +
  theme_minimal()

vital_wide_miss_plot<- vital_wide_miss %>% ggplot(aes(x = miss_pattern, fill = factor(group))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Missing Data Patterns by Group",
    x = "Missing Pattern",
    y = "Count",
    fill = "Group"
  ) +
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



