# Acupuncture

# Imputation with predict
acu_LM_MICE_predict <- with(acu_mice_predict, lm(pk5 ~ group + pk1))
acu_LM_MICE_predict_pool <- pool(acu_LM_MICE_predict)
acu_LM_MICE_predict_result <- tidy(acu_LM_MICE_predict_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LM_MICE_predict", .before = estimate)

# MI with predict + noise
acu_LM_MICE_predict_noise <- with(acu_mice_predict_noise, lm(pk5 ~ group + pk1))
acu_LM_MICE_predict_noise_pool <- pool(acu_LM_MICE_predict_noise)
acu_LM_MICE_predict_noise_result <- tidy(acu_LM_MICE_predict_noise_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LM_MICE_predict_noise", .before = estimate)

# MI with Bayesian
acu_LM_MICE_bayesian <- with(acu_mice_bayesian, lm(pk5 ~ group + pk1))
acu_LM_MICE_bayesian <- pool(acu_LM_MICE_bayesian)
acu_LM_MICE_bayesian_result <- tidy(acu_LM_MICE_bayesian, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LM_MICE_bayesian", .before = estimate)

# MI with mice default method
acu_LM_MICE_default <- with(acu_mice, lm(pk5 ~ group + pk1)) 
acu_LM_MICE_default_pool <- pool(acu_LM_MICE_default)
acu_LM_MICE_default_result <- tidy(acu_LM_MICE_default_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LM_MICE_default", .before = estimate)

# MI random sample
acu_LM_MICE_random <- with(acu_mice_random, lm(pk5 ~ group + pk1)) 
acu_LM_MICE_random_pool <- pool(acu_LM_MICE_random)
acu_LM_MICE_random_result <- tidy(acu_LM_MICE_random_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LM_MICE_random", .before = estimate)

# MI weighted predictive mean matching
acu_LM_MICE_midastouch <- with(acu_mice_midastouch, lm(pk5 ~ group + pk1)) 
acu_LM_MICE_midastouch_pool <- pool(acu_LM_MICE_midastouch)
acu_LM_MICE_midastouch_result <- tidy(acu_LM_MICE_midastouch_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "LM_MICE_midastouch", .before = estimate)






# VitD

# Imputation with predict
vital_MI_MICE_predict <- with(vital_mice_predict, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base))
vital_MI_MICE_predict_pool <- pool(vital_MI_MICE_predict)
vital_MI_MICE_predict_result_vitd <- tidy(vital_MI_MICE_predict_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_predict", .before = estimate)

# MI with predict + noise
vital_MI_MICE_predict_noise <- with(vital_mice_predict_noise, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base))
vital_MI_MICE_predict_noise_pool <- pool(vital_MI_MICE_predict_noise)
vital_MI_MICE_predict_noise_result_vitd <- tidy(vital_MI_MICE_predict_noise_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_predict_noise", .before = estimate)

# MI with Bayesian
vital_MI_MICE_bayesian <- with(vital_mice_bayesian, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base))
vital_MI_MICE_bayesian_pool <- pool(vital_MI_MICE_bayesian)
vital_MI_MICE_bayesian_result_vitd <- tidy(vital_MI_MICE_bayesian_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_bayesian", .before = estimate)

# MI with mice default method
vital_MI_MICE_default <- with(vital_mice, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base)) 
vital_MI_MICE_default_pool <- pool(vital_MI_MICE_default)
vital_MI_MICE_default_result_vitd <- tidy(vital_MI_MICE_default_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_default", .before = estimate)

# MI random sample
vital_MI_MICE_random <- with(vital_mice_random, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base)) 
vital_MI_MICE_random_pool <- pool(vital_MI_MICE_random)
vital_MI_MICE_random_result_vitd <- tidy(vital_MI_MICE_random_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_random", .before = estimate)

# MI weighted predictive mean matching
vital_MI_MICE_midastouch <- with(vital_mice_midastouch, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base)) 
vital_MI_MICE_midastouch_pool <- pool(vital_MI_MICE_midastouch)
vital_MI_MICE_midastouch_result_vitd <- tidy(vital_MI_MICE_midastouch_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_midastouch", .before = estimate)






# Fishoil

# Imputation with predict
vital_MI_MICE_predict <- with(vital_mice_predict, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base))
vital_MI_MICE_predict_pool <- pool(vital_MI_MICE_predict)
vital_MI_MICE_predict_result_oil <- tidy(vital_MI_MICE_predict_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_predict", .before = estimate)

# MI with predict + noise
vital_MI_MICE_predict_noise <- with(vital_mice_predict_noise, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base))
vital_MI_MICE_predict_noise_pool <- pool(vital_MI_MICE_predict_noise)
vital_MI_MICE_predict_noise_result_oil <- tidy(vital_MI_MICE_predict_noise_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_predict_noise", .before = estimate)

# MI with Bayesian
vital_MI_MICE_bayesian <- with(vital_mice_bayesian, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base))
vital_MI_MICE_bayesian_pool <- pool(vital_MI_MICE_bayesian)
vital_MI_MICE_bayesian_result_oil <- tidy(vital_MI_MICE_bayesian_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_bayesian", .before = estimate)

# MI with mice default method
vital_MI_MICE_default <- with(vital_mice, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base)) 
vital_MI_MICE_default_pool <- pool(vital_MI_MICE_default)
vital_MI_MICE_default_result_oil <- tidy(vital_MI_MICE_default_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_default", .before = estimate)

# MI random sample
vital_MI_MICE_random <- with(vital_mice_random, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base)) 
vital_MI_MICE_random_pool <- pool(vital_MI_MICE_random)
vital_MI_MICE_random_result_oil <- tidy(vital_MI_MICE_random_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_random", .before = estimate)

# MI weighted predictive mean matching
vital_MI_MICE_midastouch <- with(vital_mice_midastouch, lm(pain_yr4 ~ fishoilactive + vitdactive + pain_base)) 
vital_MI_MICE_midastouch_pool <- pool(vital_MI_MICE_midastouch)
vital_MI_MICE_midastouch_result_oil <- tidy(vital_MI_MICE_midastouch_pool, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_midastouch", .before = estimate)








# Result table

Method_2 <- factor(c("predict", "predict+noise", "bayesian predict+noise", 
                   "predictive mean", "random draw", "weighted predictive mean"),
                 levels = rev(c("predict", "predict+noise", 
                                "bayesian predict+noise", "predictive mean", 
                                "random draw", "weighted predictive mean")))

acu_impt_result <- rbind(acu_LM_MICE_predict_result,
                         acu_LM_MICE_predict_noise_result,
                         acu_LM_MICE_bayesian_result,
                         acu_LM_MICE_default_result,
                         acu_LM_MICE_random_result,
                         acu_LM_MICE_midastouch_result)
acu_impt_result$Method <- Method_2
acu_impt_result <- acu_impt_result %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2))

vital_impt_result_vitd <- rbind(vital_MI_MICE_predict_result_vitd,
                                vital_MI_MICE_predict_noise_result_vitd,
                                vital_MI_MICE_bayesian_result_vitd,
                                vital_MI_MICE_default_result_vitd,
                                vital_MI_MICE_random_result_vitd,
                                vital_MI_MICE_midastouch_result_vitd)
vital_impt_result_vitd$Method <- Method_2
vital_impt_result_vitd <- vital_impt_result_vitd %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2))

vital_impt_result_oil <- rbind(vital_MI_MICE_predict_result_oil,
                               vital_MI_MICE_predict_noise_result_oil,
                               vital_MI_MICE_bayesian_result_oil,
                               vital_MI_MICE_default_result_oil,
                               vital_MI_MICE_random_result_oil,
                               vital_MI_MICE_midastouch_result_oil)
vital_impt_result_oil$Method <- Method_2
vital_impt_result_oil <- vital_impt_result_oil %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2))








# Forest plot

# Acupuncture
acu_impt_result$group<-"Acupuncture Treatment"
acu_plot_imp <- ggplot(acu_impt_result, aes(x = estimate, y = Method, xmin = conf.low, xmax = conf.high)) +
  geom_point(size = 4, color = "#a80050") + geom_vline(xintercept = 0, linetype="dashed", color="red") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, color = "black") + 
  facet_wrap(~ group)+
  labs(
    x = "Mean differnce in pain score by the end of study",
    y = "Method",
    title = "Fig5.5. Acupuncture, change FCS methods") +
  theme_minimal() + 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# VITAL
vital_impt_result_vitd$treatment <- "Vitamin D"
vital_impt_result_oil$treatment <- "Fish Oil"
vital_impt_result_all <- rbind(vital_impt_result_vitd, vital_impt_result_oil)
vital_plot_impt <- ggplot(vital_impt_result_all, aes(x = estimate, y = Method, xmin = conf.low, xmax = conf.high)) +
  geom_point(size = 4, aes(color = treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +
  facet_wrap(~ treatment, scales = "free_x") +
  labs(
    x = "Mean differnce in pain score by the end of study",
    y = "Method",
    title = "Fig5.6. VITAL, change FCS methods"
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











# Different imputation number

# Acu

# K=20
acu_LM_MICE_default_20_result <- tidy(acu_LM_MICE_default_pool_20, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "K=20", .before = estimate)

# K=25
acu_LM_MICE_default_25_result <- tidy(acu_LM_MICE_default_pool_20, conf.int = TRUE, conf.method = "Wald") %>% 
  filter(term == "group") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "K=25", .before = estimate)

# VitD

# K=20
vital_MI_SLR_20_result_vitd <- summary(vital_MI_SLR_pool_20, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LR_default", .before = estimate)

# K=50
vital_MI_SLR_50_result_vitd <- summary(vital_MI_SLR_pool_50, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "vitdactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LR_default", .before = estimate)

# Fishoil

# K=20
vital_MI_SLR_20_result_oil <- summary(vital_MI_SLR_pool_20, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LR_default", .before = estimate)

# K=50
vital_MI_SLR_50_result_oil <- summary(vital_MI_SLR_pool_50, conf.int = TRUE, conf.method = "Wald")%>% 
  filter(term == "fishoilactive") %>%
  select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(Method = "MICE_LR_default", .before = estimate)

# Result table

Method_3 <- factor(c("K=5","K=20","K=25"),
                   levels = rev(c("K=5","K=20","K=25")))

Method_3 <- factor(c("K=5","K=20","K=50"),
                   levels = rev(c("K=5","K=20","K=50")))

acu_impt_k_result <- rbind(acu_LM_MICE_default_result,
                           acu_LM_MICE_default_20_result,
                           acu_LM_MICE_default_25_result)
acu_impt_k_result$Method <- Method_3
acu_impt_k_result <- acu_impt_k_result %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2))

vital_impt_k_result_vitd <- rbind(vital_MI_SLR_result_vitd,
                                vital_MI_SLR_20_result_vitd,
                                vital_MI_SLR_50_result_vitd)
vital_impt_k_result_vitd$Method <- Method_3
vital_impt_k_result_vitd <- vital_impt_k_result_vitd %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2))

vital_impt_k_result_oil <- rbind(vital_MI_SLR_result_oil,
                               vital_MI_SLR_20_result_oil,
                               vital_MI_SLR_50_result_oil)
vital_impt_k_result_oil$Method <- Method_4
vital_impt_k_result_oil <- vital_impt_k_result_oil %>%
  mutate(
    p.value = round(p.value, 5),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    std.error = round(std.error, 2))

# Forest plot

# Acupuncture
acu_impt_k_result$group<-"Acupuncture Treatment"
acu_plot_imp_k <- ggplot(acu_impt_k_result, aes(x = estimate, y = Method, xmin = conf.low, xmax = conf.high)) +
  geom_point(size = 4, color = "#a80050") + geom_vline(xintercept = 0, linetype="dashed", color="red") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, color = "black") + 
  facet_wrap(~ group)+
  labs(
    x = "Mean differnce in pain score by the end of study",
    y = "Imputation number K",
    title = "Fig5.7. Acupuncture, change imputation number") +
  theme_minimal() + 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# VITAL
vital_impt_k_result_vitd$treatment <- "Vitamin D"
vital_impt_k_result_oil$treatment <- "Fish Oil"
vital_impt_k_result_all <- rbind(vital_impt_k_result_vitd, vital_impt_k_result_oil)
vital_plot_impt_k <- ggplot(vital_impt_k_result_all, aes(x = estimate, y = Method, xmin = conf.low, xmax = conf.high)) +
  geom_point(size = 4, aes(color = treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +
  facet_wrap(~ treatment, scales = "free_x") +
  labs(
    x = "Mean differnce in pain score by the end of study",
    y = "Imputation number K",
    title = "Fig5.8. VITAL, change imputation number"
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




