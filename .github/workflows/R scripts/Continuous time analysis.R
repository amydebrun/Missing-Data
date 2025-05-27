# Acupuncture

# CAA
acu_CAA_cont <- lm(pk_score ~ group*time + pk1 , data = acu_long_cont)
acu_CAA_cont_result <- tidy(acu_CAA_cont, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "CAA", .before = estimate)

# LOCF
acu_LOCF_data <- LOCF(data = acu_wide, columns = c(10,11))
acu_LOCF_data_long <- to_long_format_acu_cont(acu_LOCF_data)
acu_LOCF_cont <- lm(pk_score ~ group*time + pk1 , data = acu_LOCF_data_long)
acu_LOCF_cont_result <- tidy(acu_LOCF_cont, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LOCF_LR", .before = estimate)

# MOCF
acu_SPM_data <- mean_impute(acu_wide)
acu_SPM_data_long <- to_long_format_acu_cont(acu_SPM_data)
acu_SPM_cont <- lm(pk_score ~ group*time + pk1, data = acu_SPM_data_long)
acu_SPM_cont_result <- tidy(acu_SPM, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MOCF_LR", .before = estimate)

# MI with mice default method
acu_LM_MICE_cont <- with(acu_mice_data_obj_long_cont, lm(pk_score ~ group*time + pk1))
acu_LM_MICE_cont_pool <- pool(acu_LM_MICE_cont)
acu_LM_MICE_default_cont_result <- tidy(acu_LM_MICE_cont_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LM_MICE_cont", .before = estimate)


# LME with no imputation
acu_LME_cont <- lmer(pk_score ~ group*(time_c) + pk1 + (1|id), data = acu_long_cont)
acu_LME_cont_result <- tidy(acu_LME_cont, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error) %>%
  mutate(Method = "LME_cont", .before = estimate) %>%
  mutate(p.value = NA_real_)

# LME + MI with mice default
acu_LME_MI_default_cont <- with(acu_mice_data_obj_long_cont, 
                           lmer(pk_score ~ group*time + pk1 + (1|id), data = acu_long_cont))
acu_LME_MI_default_cont_pool <- pool(acu_LME_MI_default_cont)
acu_LME_MI_default_cont_result <- tidy(acu_LME_MI_default_cont_pool, conf.int = TRUE, conf.method = "Wald") %>%
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LME_MICE_default", .before = estimate)






# VITAL FISHOIL

# CCA
vital_complete_cont <- lm(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base, data = vital_long) 
vital_CAA_result_oil_cont <- tidy(vital_complete_cont, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "CAA", .before = estimate)

# MOCF
vital_MOCF <- mean_impute(vital_wide)
vital_MOCF_long <- to_long_format_vital(vital_MOCF)
vital_MOCF_cont <- lm(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base, data = vital_MOCF_long)
vital_MOCF_SLR_result_oil_cont <- tidy(vital_MOCF_cont, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MOCF_LR", .before = estimate)

# LOCF
vital_LOCF_data <- LOCF(data = vital_wide,
                        columns = c(5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29))
vital_LOCF_long <- to_long_format_vital(vital_LOCF_data)
vital_LOCF_cont <- lm(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base, data = vital_LOCF_long)
vital_LOCF_result_oil_cont <- tidy(vital_LOCF_cont, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LOCF_LR", .before = estimate)

# MI+SLR
vital_MI_SLR_cont <- with(vital_mice_obj_long,
                          lm(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base))
vital_MI_SLR_cont_pool <- pool(vital_MI_SLR_cont)
vital_MI_SLR_result_oil_cont <- summary(vital_MI_SLR_cont_pool, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LR_default", .before = estimate)

# LME
vital_LM_cont <- lmer(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base + 
                    (time_contin|Subject_ID), 
                  data = vital_long)
vital_LME_result_oil_cont <- tidy(vital_LM_cont, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error) %>%
  mutate(Method = "LME", .before = estimate) %>%
  mutate(p.value = NA_real_)

# MI+LME
fishoil_MI_LME_cont <- with(vital_mice_obj_long, 
                       lmer(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base + 
                              (time_contin|Subject_ID)))
vital_MI_LME_pool_cont <- pool(fishoil_MI_LME_cont)
vital_MI_LME_result_oil_cont <- tidy(vital_MI_LME_pool_cont, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LME_default", .before = estimate)






# VITAL VITAMIN D

# CCA
vital_complete_cont_vitd <- lm(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base, data = vital_long) 
vital_CAA_result_vitd_cont <- tidy(vital_complete_cont_vitd, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "CAA", .before = estimate)

# MOCF
vital_MOCF <- mean_impute(vital_wide)
vital_MOCF_long <- to_long_format_vital(vital_MOCF)
vital_MOCF_cont_vitd <- lm(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base, data = vital_MOCF_long)
vital_MOCF_SLR_result_vitd_cont <- tidy(vital_MOCF_cont_vitd, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MOCF_LR", .before = estimate)

# LOCF
vital_LOCF_data <- LOCF(data = vital_wide,
                        columns = c(5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29))
vital_LOCF_long <- to_long_format_vital(vital_LOCF_data)
vital_LOCF_cont_vitd <- lm(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base, data = vital_LOCF_long)
vital_LOCF_result_vitd_cont <- tidy(vital_LOCF_cont_vitd, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LOCF_LR", .before = estimate)

# MI+SLR
vital_MI_SLR_cont_vitd <- with(vital_mice_obj_long,
                               lm(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base))
vital_MI_SLR_cont_pool_vitd <- pool(vital_MI_SLR_cont_vitd)
vital_MI_SLR_result_vitd_cont <- summary(vital_MI_SLR_cont_pool_vitd, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LR_default", .before = estimate)

# LME
vital_LME_cont_vitd <- lmer(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base + 
                              (time_contin|Subject_ID), 
                            data = vital_long)
vital_LME_result_vitd_cont <- tidy(vital_LME_cont_vitd, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error) %>%
  mutate(Method = "LME", .before = estimate) %>%
  mutate(p.value = NA_real_)

# MI+LME
vitd_MI_LME_cont <- with(vital_mice_obj_long, 
                         lmer(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base + 
                                (time_contin|Subject_ID)))
vital_MI_LME_pool_vitd_cont <- pool(vitd_MI_LME_cont)
vital_MI_LME_result_vitd_cont <- tidy(vital_MI_LME_pool_vitd_cont, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LME_default", .before = estimate)





# Result and ordering

# Result table tools
Method_cont <- factor(c("CAAcont", "LOCFcont", "MOCFcont", "LRcont+MI+pmm", "LMEcont", "LMEcont+MI+pmm"),
                 levels = rev(c("CAAcont", "LOCFcont", "MOCFcont", "LRcont+MI+pmm", "LMEcont", "LMEcont+MI+pmm")))

properties_cont <- data.frame(Estimate = c("LR + continuous time", "LR + continuous time", "LR + continuous time", "LR + continuous time", "LME + continuous time", "LME + continuous time"),
                         Missing_method = c("N/A", "Simple", "Simple", "Multiple", "N/A", "Multiple"),
                         Imputation_method = c("N/A", "Last", "Mean", "pmm", "N/A", "pmm"))

# ACU Result table
acu_cont_result <- rbind(acu_CAA_cont_result, 
                    acu_LOCF_cont_result,
                    acu_SPM_cont_result, 
                    acu_LM_MICE_default_cont_result,
                    acu_LME_cont_result,
                    acu_LME_MI_default_cont_result)

acu_cont_result$Method <- Method_cont
acu_cont_result <- acu_cont_result %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2))
# Compare with ACU cat
acu_compare_result <- cbind(acu_result, acu_cont_result)[,c(1,2,5,8,9,12)]

# Fish oil Result Table
vital_result_oil_cont <- rbind(vital_CAA_result_oil_cont,
                               vital_LOCF_result_oil_cont,
                               vital_MOCF_SLR_result_oil_cont,
                               vital_MI_SLR_result_oil_cont,
                               vital_LME_result_oil_cont,
                               vital_MI_LME_result_oil_cont)

vital_result_oil_cont$Method <- Method_cont
vital_result_oil_cont <- vital_result_oil_cont %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2)
  )
# Compare with fish oil cat
oil_compare_result <- cbind(vital_result_oil, vital_result_oil_cont)[,c(1,2,5,8,9,12)]


# VitD Result table
vital_result_vitd_cont <- rbind(vital_CAA_result_vitd_cont,
                                vital_LOCF_result_vitd_cont,
                                vital_MOCF_SLR_result_vitd_cont,
                                vital_MI_SLR_result_vitd_cont,
                                vital_LME_result_vitd_cont,
                                vital_MI_LME_result_vitd_cont)

vital_result_vitd_cont$Method <- Method_cont
vital_result_vitd_cont <- vital_result_vitd_cont %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2)
  )
# Compare with vitamine D cat
vitd_compare_result <- cbind(vital_result_vitd, vital_result_vitd_cont)[,c(1,2,5,8,9,12)]






# FOREST PLOTS OF RESULTS 

# VITAL Forest plot
vital_result_vitd_cont$treatment <- "Vitamin D"
vital_result_oil_cont$treatment <- "Fish Oil"
vital_result_all <- rbind(vital_result_vitd, vital_result_oil)
vital_result_all_cont <- rbind(vital_result_vitd_cont, vital_result_oil_cont)
vital_plot_table <- cbind(vital_result_all, vital_result_all_cont)[,-c(7,8)]
vital_plot_table[,1] <- factor(c("CAA", "LOCF", "MOCF", "LR_MI_pmm", "LME", "LME_MI_pmm"),
                             levels = rev(c("CAA", "LOCF", "MOCF", "LR_MI_pmm", "LME", "LME_MI_pmm")))

vital_plot_compare <- ggplot(vital_plot_table, aes(x = estimate, y = Method, xmin = conf.low, xmax = conf.high)) +
  geom_point(size = 4, aes(color = treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, color="#a80050") +
  geom_point(aes(x = estimate.1), 
             size = 4, color = "lawngreen", shape = 17) + 
  geom_errorbarh(aes(xmin = conf.low.1, xmax = conf.high.1), 
                 height = 0.4, color = "lawngreen") +
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
acu_plot_table <- cbind(acu_result, acu_cont_result)[,-c(7,8,14)]
acu_plot_table[,1] <- factor(c("CAA", "LOCF", "MOCF", "LR_MI_pmm", "LME", "LME_MI_pmm"),
                             levels = rev(c("CAA", "LOCF", "MOCF", "LR_MI_pmm", "LME", "LME_MI_pmm")))
acu_plot_table$group<-"Acupuncture"
acu_plot_compare <- ggplot(acu_plot_table, aes(x = estimate, y = Method, xmin = conf.low, xmax = conf.high)) +
  geom_point(size = 4, color = "#a80050") + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, color = "#a80050") + 
  geom_point(aes(x = estimate.1), 
             size = 4, color = "lawngreen", shape = 17) + 
  geom_errorbarh(aes(xmin = conf.low.1, xmax = conf.high.1), 
                 height = 0.4, color = "lawngreen") + facet_wrap(~group)+
  geom_vline(xintercept = 0, linetype="dashed", color="red") +
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
