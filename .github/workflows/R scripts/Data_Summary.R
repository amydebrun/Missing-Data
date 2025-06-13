

## Basics of Acupuncture dataset
#variable table 

acu_var_table <- tibble::tibble(
  `Variable` = c(
    "id", "age", "sex", "migraine", "chronicity", "acupuncturist", "practice_id", "group",
    "pk1", "pk2", "pk5", "f1", "f2", "f5"
  ),
  `Description` = c(
    "patient ID code",
    "Age",
    "sex; female (1) vs. male (0)",
    "diagnosis ; migraine (1) vs. tension-type (0)",
    "number of years of headache disorder",
    "acupuncturist id code",
    "gp practice id",
    "treatment group; acupuncture (1) vs. control (0)",
    "headache severity score baseline",
    "headache severity score 3 month",
    "headache severity score 1 year",
    "headache frequency baseline",
    "headache frequency 3 month",
    "headache frequency 1 year"
  )
)


vital_var_table <- tibble::tibble(
  `Variable` = c(
    "Subject_ID","age", "bmi", "sex", "vitdactive", "fishoilactive", "pain_base", "pain_yrX", "stiffness_base", "stiffness_yrX", "function_base", 
    "function_yrX", "kneepainfreq"
  ),
  `Description` = c("Patient ID code", "Age of patient", "Body mass index of patient", "Sex of patient", 
  "1=vitamin D, 0=no vitamin D", "1= fish oil, 0= no fish oil",
  "Knee pain at baseline", "Knee pain X years post randomisation", 
  "Knee stiffness at baseline", "Knee stiffness X years post randomisation", "Knee function at baseline",
  "Knee function X years post randomisation", "Frequency of knee pain"
))


missing_pattern_table<- tibble::tibble(
  `Missing Data Pattern` = c("Univariate", "Multivariate", "Monotonic", "General", "File Matching", "Factor Analysis"),
  `Description` = c("Missing values in a single variable",
                    "Missing values present in multiple variables",
                    "variables $Y_j$ can be ordered if missing",
                    "Missing valueshave no structure and scattered throughout data",
                    "Missing data appears to be statistically matched",
                    "")
)

#distribution of age by treatment group
acu_age_plot<-ggplot(acu_long, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "#a80050", color = "black", alpha = 0.8) + 
  facet_wrap(~ group, 
             scales = "fixed", 
             labeller = labeller(group = c("0" = "Control", "1" = "Acupuncture"))) +
  labs(
    title = "",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal() + theme(
    legend.position = "none",  
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)  
  )

#DISTRIBUTION SEX BY TREATMENT GROUP
    
acu_sex_plot<-ggplot(acu_long, aes(
  x = factor(sex, labels = c("Male", "Female")),
  fill = factor(sex, labels = c("Male", "Female"))
)) +
  geom_bar(width = 0.5, color = "black") + 
  facet_wrap(~ group, 
             scales = "fixed", 
             labeller = labeller(group = c("0" = "Control", "1" = "Acupuncture"))) +
  labs(
    title = "",
    x = "Sex",
    y = "Count",
    fill = "Sex"
  ) + 
  theme_minimal() + 
  scale_fill_manual(values = c("Male" = "#a80050", "Female" = "#a80050")) +  
  theme(
    legend.position = "none",  
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)  
  )

x<-acu_long %>%
  summarise(
    missing_age = mean(is.na(age)) * 100,
    missing_sex = mean(is.na(sex)) * 100
  )


#acupuncture pain distribution 

acu_pk1_plot<-ggplot(acu_wide, aes(x = pk1)) +
  geom_histogram(binwidth = 2, fill = "#a80050", color = "black", alpha = 0.8) + 
  facet_wrap(~ group, 
             scales = "fixed", 
             labeller = labeller(group = c("0" = "Control", "1" = "Acupuncture"))) +
  labs(
    title = "Baseline Pain",
    x = "Headache score at baseline",
    y = "Count"
  ) +
  theme_minimal() + theme(
    legend.position = "none",  
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)  
  )

acu_pk2_plot<-ggplot(acu_wide, aes(x = pk2)) +
  geom_histogram(binwidth = 2, fill = "#a80050", color = "black", alpha = 0.8) + 
  facet_wrap(~ group, 
             scales = "fixed", 
             labeller = labeller(group = c("0" = "Control", "1" = "Acupuncture"))) +
  labs(
    title = "3 Month Follow-Up",
    x = "Headache score at 3 months",
    y = "Count"
  ) +
  theme_minimal() + theme(
    legend.position = "none",  
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)  
  )

acu_pk5_plot<-ggplot(acu_wide, aes(x = pk5)) +
  geom_histogram(binwidth = 2, fill = "#a80050", color = "black", alpha = 0.8) + 
  facet_wrap(~ group, 
             scales = "fixed", 
             labeller = labeller(group = c("0" = "Control", "1" = "Acupuncture"))) +
  labs(
    title = "12 Month Follow-Up",
    x = "Headache score at 1 year",
    y = "Count"
  ) +
  theme_minimal() + theme(
    legend.position = "none",  
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)  
  )


## Basics of VITAL dataset


#overall age 
ggplot(vital_long, aes(x = ageyr)) + 
  geom_histogram(binwidth = 1, fill = "#a80050", color = "black", alpha = 0.8) +
  labs(
    title = "Distribution of Age in VITAL dataset",
    x = "Age (years)",
    y = "Count"
  ) +
  theme_minimal()

#overall bmi
ggplot(vital_long, aes(x = bmi)) +
  geom_histogram(binwidth = 1, fill = "lawngreen", color = "black", alpha = 0.8) +
  labs(
    title = "Distribution of BMI in VITAL dataset",
    x = "Body Mass Index",
    y = "Count"
  ) +
  theme_minimal()

#overall sex
ggplot(vital_long, aes(x = factor(sex, labels = c("Male", "Female")), fill = factor(sex, labels = c("Male", "Female")))) +
  geom_bar(width=0.5,color="black") +
  labs(
    title = "Distribution of Sex in Vital dataset",
    x = "Sex",
    y = "Count",
    fill = "Sex"
  ) + theme_minimal() + scale_fill_manual(values=c("green","lawngreen")) + theme(legend.position="none")




vital_long %>%
  summarise(
    missing_age = mean(is.na(ageyr)) * 100,
    missing_sex = mean(is.na(sex)) * 100,
    missing_bmi = mean(is.na(bmi)) * 100
  )


## Seperated by fishoil & vitamin D


#Age distribution of each group 

age_vital_plot<-vital_long %>%
  mutate(treatment = case_when(
    vitdactive == 1 & fishoilactive == 0 ~ "Vitamin D",
    fishoilactive == 1 & vitdactive == 0 ~ "Fish Oil",
    vitdactive == 0 & fishoilactive == 0 ~ "Control",
    vitdactive == 1 & fishoilactive == 1 ~ "Both Treatment"
  )) %>%
  ggplot(aes(x = ageyr)) +
  geom_histogram(binwidth = 1, fill = "#a80050", color = "black", alpha = 0.8) +
  facet_wrap(~ treatment, scales = "fixed") + 
  labs(
    title = "Distribution of Age in VITAL data",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal()+ theme(
        strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#BMI distribution of each group

bmi_vital_plot<-vital_long %>%
  mutate(treatment = case_when(
    vitdactive == 1 & fishoilactive == 0 ~ "Vitamin D",
    fishoilactive == 1 & vitdactive == 0 ~ "Fish Oil",
    vitdactive == 0 & fishoilactive == 0 ~ "Control",
    vitdactive == 1 & fishoilactive == 1 ~ "Both Treatment"
  )) %>%
  ggplot(aes(x = bmi)) +
  geom_histogram(binwidth = 1, fill = "#a80050", color = "black", alpha = 0.8) +
  facet_wrap(~ treatment, scales = "fixed") + 
  labs(
    title = "Distribution of BMI for VITAL data",
    x = "Body Mass Index",
    y = "Count"
  ) +
  theme_minimal()+ theme(
        strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#sex in each group 
sex_vital_plot<-vital_long %>%
  mutate(treatment = case_when(
    vitdactive == 1 & fishoilactive == 0 ~ "Vitamin D",
    fishoilactive == 1 & vitdactive == 0 ~ "Fish Oil",
    vitdactive == 0 & fishoilactive == 0 ~ "Control",
    vitdactive == 1 & fishoilactive == 1 ~ "Both Treatment"
  )) %>%
  ggplot(aes(x = factor(sex, labels = c("Male", "Female")), fill = factor(sex, labels = c("Male", "Female")))) +
  geom_bar(width = 0.5, color = "black") + 
  facet_wrap(~ treatment, scales = "fixed") +
  labs(
    title = "Sex Distribution in VITAL data",
    x = "Sex",
    y = "Count",
    fill = "Sex"
  ) + 
  theme_minimal() + 
  scale_fill_manual(values = c("#a80050", "#a80050")) +  
  theme(
    legend.position = "none",  
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)  
  )


#knee pain score 
pain_base_vital_plot<-vital_wide %>%
  mutate(treatment = case_when(
    vitdactive == 1 & fishoilactive == 0 ~ "Vitamin D",
    fishoilactive == 1 & vitdactive == 0 ~ "Fish Oil",
    vitdactive == 0 & fishoilactive == 0 ~ "Control",
    vitdactive == 1 & fishoilactive == 1 ~ "Both Treatment"
  )) %>%
  ggplot(aes(x = pain_base)) +
  geom_histogram(binwidth = 3, fill = "#a80050", color = "black", alpha = 0.8) +
  facet_wrap(~ treatment, scales = "fixed") + 
  labs(
    title = "Baseline Knee Pain",
    x = "Knee pain score",
    y = "Count"
  ) +
  theme_minimal()+ theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

pain_yr1_vital_plot<-vital_wide %>%
  mutate(treatment = case_when(
    vitdactive == 1 & fishoilactive == 0 ~ "Vitamin D",
    fishoilactive == 1 & vitdactive == 0 ~ "Fish Oil",
    vitdactive == 0 & fishoilactive == 0 ~ "Control",
    vitdactive == 1 & fishoilactive == 1 ~ "Both Treatment"
  )) %>%
  ggplot(aes(x = pain_yr1)) +
  geom_histogram(binwidth = 3, fill = "#a80050", color = "black", alpha = 0.8) +
  facet_wrap(~ treatment, scales = "fixed") + 
  labs(
    title = "Knee Pain 1 Year post-randomisation",
    x = "Knee pain score",
    y = "Count"
  ) +
  theme_minimal()+ theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

pain_yr2_vital_plot<-vital_wide %>%
  mutate(treatment = case_when(
    vitdactive == 1 & fishoilactive == 0 ~ "Vitamin D",
    fishoilactive == 1 & vitdactive == 0 ~ "Fish Oil",
    vitdactive == 0 & fishoilactive == 0 ~ "Control",
    vitdactive == 1 & fishoilactive == 1 ~ "Both Treatment"
  )) %>%
  ggplot(aes(x = pain_yr2)) +
  geom_histogram(binwidth = 3, fill = "#a80050", color = "black", alpha = 0.8) +
  facet_wrap(~ treatment, scales = "fixed") + 
  labs(
    title = "Knee Pain 2 Years post-randomisation",
    x = "Knee pain score",
    y = "Count"
  ) +
  theme_minimal()+ theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

pain_yr3_vital_plot<-vital_wide %>%
  mutate(treatment = case_when(
    vitdactive == 1 & fishoilactive == 0 ~ "Vitamin D",
    fishoilactive == 1 & vitdactive == 0 ~ "Fish Oil",
    vitdactive == 0 & fishoilactive == 0 ~ "Control",
    vitdactive == 1 & fishoilactive == 1 ~ "Both Treatment"
  )) %>%
  ggplot(aes(x = pain_yr3)) +
  geom_histogram(binwidth = 3, fill = "#a80050", color = "black", alpha = 0.8) +
  facet_wrap(~ treatment, scales = "fixed") + 
  labs(
    title = "Knee Pain 3 Years post-randomisation",
    x = "Knee pain score",
    y = "Count"
  ) +
  theme_minimal()+ theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

pain_yr4_vital_plot<-vital_wide %>%
  mutate(treatment = case_when(
    vitdactive == 1 & fishoilactive == 0 ~ "Vitamin D",
    fishoilactive == 1 & vitdactive == 0 ~ "Fish Oil",
    vitdactive == 0 & fishoilactive == 0 ~ "Control",
    vitdactive == 1 & fishoilactive == 1 ~ "Both Treatment"
  )) %>%
  ggplot(aes(x = pain_yr4)) +
  geom_histogram(binwidth = 3, fill = "#a80050", color = "black", alpha = 0.8) +
  facet_wrap(~ treatment, scales = "fixed") + 
  labs(
    title = "Knee Pain 4 Years post-randomisation",
    x = "Knee pain score",
    y = "Count"
  ) +
  theme_minimal()+ theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )




