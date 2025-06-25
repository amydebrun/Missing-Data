# Acupuncture

# LME with no imputation
acu_LME_cont <- lmer(pk_score ~ group*(time_c) + pk1 + (1|id), data = acu_long_cont)
acu_LME_cont_result <- tidy(acu_LME_cont, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error) %>%
  mutate(Method = "LME_cont", .before = estimate) %>%
  mutate(p.value = NA_real_)

# LME + MI with mice default
acu_LME_MI_default_cont <- with(acu_mice_data_obj_long_cont, 
                                lmer(pk_score ~ group*(time_c) + pk1 + (1|id)))
acu_LME_MI_default_cont_pool <- pool(acu_LME_MI_default_cont)
acu_LME_MI_default_cont_result <- tidy(acu_LME_MI_default_cont_pool, conf.int = TRUE, conf.method = "Wald") %>%
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LME_MICE_default", .before = estimate)






# VITAL FISHOIL

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
Method_cont <- factor(c("LMEcont", "LMEcont+MI+pmm"),
                 levels = rev(c("LMEcont", "LMEcont+MI+pmm")))

properties_cont <- data.frame(Estimate = c("LME + continuous time", "LME + continuous time"),
                         Missing_method = c("N/A", "Multiple"),
                         Imputation_method = c("N/A", "pmm"))

# ACU Result table
acu_cont_result <- rbind(acu_LME_cont_result,
                    acu_LME_MI_default_cont_result)

acu_cont_result$Method <- Method_cont
acu_cont_result <- acu_cont_result %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2),
    group = "Acupuncture Treatment",
    estimand = "continuous")

acu_result <- acu_result %>% mutate(estimand = "categorical")

# Compare with ACU cat
acu_compare_result <- rbind(acu_result[5:6,], acu_cont_result)


# Fish oil Result Table
vital_result_oil_cont <- rbind(vital_LME_result_oil_cont,
                               vital_MI_LME_result_oil_cont)

vital_result_oil_cont$Method <- Method_cont
vital_result_oil_cont <- vital_result_oil_cont %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2),
    treatment = "Fish Oil",
    estimand = "continuous"
  )

vital_result_oil <- vital_result_oil %>% mutate(estimand = "categorical")

# Compare with fish oil cat
oil_compare_result <- rbind(vital_result_oil[5:6,], vital_result_oil_cont)


# VitD Result table
vital_result_vitd_cont <- rbind(vital_LME_result_vitd_cont,
                                vital_MI_LME_result_vitd_cont)

vital_result_vitd_cont$Method <- Method_cont
vital_result_vitd_cont <- vital_result_vitd_cont %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2),
    treatment = "Vitamin D",
    estimand = "continuous"
    )

vital_result_vitd <- vital_result_vitd %>% mutate(estimand = "categorical")

# Compare with vitamine D cat
vitd_compare_result <- rbind(vital_result_vitd[5:6,], vital_result_vitd_cont)










# ACU Forest plot
acu_compare_result[,1] <- c("LME", "LME+MI", "LME", "LME+MI")
acu_compare_result$group<-"Acupuncture"
acu_plot_compare <- ggplot(acu_compare_result, 
                           aes(x = estimate, y = Method, xmin = conf.low, xmax = conf.high, color = estimand)) +
  geom_point(size = 4, position = position_dodge(width = 0.6)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, linetype="dashed", color="red") +
  facet_wrap(~ group) +
  scale_shape_manual(values = c("categorical" = 16, "continuous" = 17)) + 
  scale_colour_manual(values=c("categorical"="#a80050", "continuous"="lawngreen"))+
  labs(
    x = "Mean difference in pain score by the end of study",
    y = "Method") +
  theme_minimal() + 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


# FOREST PLOTS OF RESULTS 

# VITAL Forest plot
vital_result_all <- rbind(oil_compare_result, vitd_compare_result)
vital_result_all[,1] <- c("LME", "LME+MI", "LME", "LME+MI", "LME", "LME+MI", "LME", "LME+MI")

vital_plot_compare <- ggplot(vital_result_all, 
                             aes(x = estimate, y = Method, xmin = conf.low, xmax = conf.high, color = estimand)) +
  geom_point(size = 4, position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_dodge(width = 0.6)) +
  facet_wrap(~ treatment, scales = "free_x") +
  scale_shape_manual(values = c("categorical" = 16, "continuous" = 17)) + 
  scale_colour_manual(values=c("categorical"="#a80050", "continuous"="lawngreen"))+
  labs(
    x = "Mean difference in pain score by the end of study",
    y = "Method") +
  theme_minimal() + 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
