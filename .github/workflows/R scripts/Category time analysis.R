# Acupuncture

# CAA
acu_CAA <- lm(pk5 ~ group + pk1 , data = acu_wide)
acu_CAA_result <- tidy(acu_CAA, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "CAA", .before = estimate)

# LOCF
acu_LOCF_data <- LOCF(data = acu_wide, columns = c(10,11))
acu_LOCF <- lm(pk5 ~ group + pk1, data = acu_LOCF_data)
acu_LOCF_result <- tidy(acu_LOCF, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LOCF_LR", .before = estimate)

# Simple mean imputation
acu_SPM_data <- mean_impute(acu_wide)
acu_SPM <- lm(pk5 ~ group + pk1, data = acu_SPM_data)
acu_SPM_result <- tidy(acu_SPM, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MOCF_LR", .before = estimate)

# MI with mice default method
acu_LM_MICE_default <- with(acu_mice, lm(pk5 ~ group + pk1)) 
acu_LM_MICE_default_pool <- pool(acu_LM_MICE_default)
acu_LM_MICE_default_result <- tidy(acu_LM_MICE_default_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LM_MICE_default", .before = estimate)

# LME with no imputation
acu_long$time <- relevel(factor(acu_long$time), ref = "12m")
acu_LME <- lmer(pk_score ~ group + pk1 + time + (1| id), data = acu_long)
acu_LME_result <- tidy(acu_LME, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error) %>%
  mutate(Method = "LME", .before = estimate) %>%
  mutate(p.value = NA_real_)

# LME + MI with mice default
acu_LME_MI_default <- with(acu_mice_data_obj_long, 
                           lmer(pk_score ~ group + pk1 + time + (1|id), data = acu_long))
acu_LME_MI_default_pool <- pool(acu_LME_MI_default)
acu_LME_MI_default_result <- tidy(acu_LME_MI_default_pool, conf.int = TRUE, conf.method = "Wald") %>%
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LME_MICE_default", .before = estimate)






# VITAL FISHOIL

# CCA
vital_complete <- lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base, data = vital_wide) 
vital_CAA_result_oil <- tidy(vital_complete, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "CAA", .before = estimate)

# MOCF
vital_MOCF <- mean_impute(vital_wide)
vital_MOCF <- lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base, data = vital_MOCF)
vital_MOCF_SLR_result_oil<- tidy(vital_MOCF, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MOCF_LR", .before = estimate)

# LOCF
vital_LOCF_data <- LOCF(data = vital_wide, columns = c(5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29))
vital_LOCF <- lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base, data = vital_LOCF_data)
vital_LOCF_result_oil <- tidy(vital_LOCF, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LOCF_LR", .before = estimate)

# MI+SLR
vital_MI_SLR <- with(vital_mice, lm(pain_yr4 ~ fishoilactive + vitdactive  + pain_base ))
vital_MI_SLR_pool <- pool(vital_MI_SLR)
vital_MI_SLR_result_oil <- summary(vital_MI_SLR_pool, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LR_default", .before = estimate)

# LME
vital_LME <- lmer(pain ~ fishoilactive + vitdactive + pain_base + time_contin_cent + (1|Subject_ID), 
                  data = vital_long)
vital_LME_result_oil <- tidy(vital_LME, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error) %>%
  mutate(Method = "LME", .before = estimate) %>%
  mutate(p.value = NA_real_)

# MI+LME
fishoil_MI_LME <- with(vital_mice_obj_long, 
                       lmer(pain ~ fishoilactive + vitdactive + pain_base + time_contin_cent + (1|Subject_ID)))
vital_MI_LME_pool <- pool(fishoil_MI_LME)
vital_MI_LME_result_oil <- tidy(vital_MI_LME_pool, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LME_default", .before = estimate)






# VITAL VITAMIN D

# CCA
vital_complete <- lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base, data = vital_wide) 
vital_CAA_result_vitd <- tidy(vital_complete, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "CAA", .before = estimate)

# MOCF
vital_MOCF <- mean_impute(vital_wide)
vital_MOCF <- lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base, data = vital_MOCF)
vital_MOCF_SLR_result_vitd<- tidy(vital_MOCF, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MOCF_LR", .before = estimate)

# LOCF
vital_LOCF <- lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base, data = vital_LOCF_data)
vital_LOCF_result_vitd <- tidy(vital_LOCF, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LOCF_LR", .before = estimate)

# MI+SLR
vital_MI_SLR <- with(vital_mice, lm(pain_yr4 ~ fishoilactive + vitdactive  + pain_base ))
vital_MI_SLR_pool <- pool(vital_MI_SLR)
vital_MI_SLR_result_vitd <- summary(vital_MI_SLR_pool, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LR_default", .before = estimate)

# LME
vital_LME <- lmer(pain ~ fishoilactive + vitdactive + pain_base + time_contin_cent + (1|Subject_ID), 
                  data = vital_long)
vital_LME_result_vitd <- tidy(vital_LME, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error) %>%
  mutate(Method = "LME", .before = estimate) %>%
  mutate(p.value = NA_real_)

# MI+LME
fishoil_MI_LME <- with(vital_mice_obj_long, 
                       lmer(pain ~ fishoilactive + vitdactive + pain_base + time_contin_cent + (1|Subject_ID), 
                            data = vital_long))
vital_MI_LME_pool <- pool(fishoil_MI_LME)
vital_MI_LME_result_vitd <- tidy(vital_MI_LME_pool, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LME_default", .before = estimate)





# Result and ordering

# Result table tools
Method <- factor(c("CAAcat", "LOCFcat", "MOCFcat", "LRcat+MI+pmm", "LMEcat", "LMEcat+MI+pmm"),
                 levels = rev(c("CAAcat", "LOCFcat", "MOCFcat", "LRcat+MI+pmm", "LMEcat", "LMEcat+MI+pmm")))

properties <- data.frame(Estimate = c("LR + category time", "LR + category time", "LR + category time", "LR + category time", "LME + category time", "LME + category time"),
                         Missing_method = c("N/A", "Simple", "Simple", "Multiple", "N/A", "Multiple"),
                         Imputation_method = c("N/A", "Last", "Mean", "pmm", "N/A", "pmm"))

# ACU Result table
acu_result <- rbind(acu_CAA_result, 
                    acu_LOCF_result,
                    acu_SPM_result, 
                    acu_LM_MICE_default_result,
                    acu_LME_result,
                    acu_LME_MI_default_result)

acu_result$Method <- Method
acu_result <- acu_result %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2))

# Fish oil Result Table
vital_result_oil <- rbind(vital_CAA_result_oil,
                          vital_LOCF_result_oil,
                          vital_MOCF_SLR_result_oil,
                          vital_MI_SLR_result_oil,
                          vital_LME_result_oil,
                          vital_MI_LME_result_oil)

vital_result_oil$Method <- Method
vital_result_oil <- vital_result_oil %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2))

# VitD Result table
vital_result_vitd <- rbind(vital_CAA_result_vitd,
                           vital_LOCF_result_vitd,
                           vital_MOCF_SLR_result_vitd,
                           vital_MI_SLR_result_vitd,
                           vital_LME_result_vitd,
                           vital_MI_LME_result_vitd)

vital_result_vitd$Method <- Method
vital_result_vitd <- vital_result_vitd %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2))





# FOREST PLOTS OF RESULTS 

# VITAL Forest plot
vital_result_vitd$treatment <- "Vitamin D"
vital_result_oil$treatment <- "Fish Oil"
vital_result_all <- rbind(vital_result_vitd, vital_result_oil)
vital_plot_categorical <- ggplot(vital_result_all, aes(x = estimate, y = Method, xmin = conf.low, xmax = conf.high)) +
  geom_point(size = 4, aes(color = treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +
  facet_wrap(~ treatment, scales = "free_x") +
  labs(
    x = "Treatment Effect",
    y = "Method",
    title = "Treatment Effects with 95% CI for Fish oil and Vitamin D treatment"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),     
    plot.background = element_rect(fill = "white", color = NA),       
    strip.background = element_rect(fill = "lawngreen", color = "black"),
    panel.spacing = unit(1, "lines")                                  
  ) +
  scale_color_manual(values = c("Vitamin D" = "#a80050", "Fish Oil" = "#a80050"))


# ACU Forest plot
acu_result$group<-"Acupuncture Treatment"
acu_plot_categorical <- ggplot(acu_result, aes(x = estimate, y = Method, xmin = conf.low, xmax = conf.high)) +
  geom_point(size = 4, color = "#a80050") + geom_vline(xintercept = 0, linetype="dashed", color="red") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, color = "black") + 
  facet_wrap(~ group)+
  labs(
    x = "Treatment Effect",
    y = "Method",
    title = "Treatment effect with 95% Confidence Interval") +
  theme_minimal() + 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
