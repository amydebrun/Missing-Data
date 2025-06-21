#COMBINING THE TWO PLOTS TOGETHER 
#delta values categorical and continuous
#acu_delta_cat<-c(-6.859139,-3.429569,0,3.429569,6.859139)
#acu_control_delta_cat<-c(-8.505446,-4.252723,0,4.252723,8.505446)
#fishoil_delta_cat<- c(-9.569774,-4.784887,0,4.784887,9.569774)
#vitd_delta_cat<-c(-9.624281,-4.812141,0,4.812141,9.624281)
#vital_control_delta<-c(-9.147397,-4.573698,0,4.573698,9.147397)

#acu_delta_cont<-c(-7.397829,-3.698915,0,3.698915,7.397829)
#acu_control_delta_cont<-c(-8.660196,-4.330098,0, 4.330098,8.660196)
#fishoil_delta_cont<-c(-9.783807,-4.891904,0,4.891904,9.783807)
#vitd_delta <-c(-9.967777,-4.983889,0,4.983889,9.967777)
#vital_control_delta<-c(-9.646493,-4.823247,0, 4.823247,9.646493)


#categorical
acu_treatment_cat <- delta_results_cat %>%
  rename(delta = acu_delta) %>%
  mutate(treatment = "Acupuncture")
acu_control_cat  <- delta_results_cat_placebo %>%
  rename(delta = acu_control_delta) %>%
  mutate(treatment = "Control")
acu_combined_cat <- bind_rows(acu_treatment_cat, acu_control_cat)


ggplot(acu_combined_cat , aes(x = estimate, y = delta)) +
  geom_point(aes(color = treatment, shape = treatment), size = 3, position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = treatment),
                 height = 0.3,
                 position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  labs(
    title = "Sensitivity Analysis: δ-Adjustment (Categorical Estimand)",
    x = "Estimates",
    y = "Delta"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "none",  
    panel.background = element_rect(fill = "white")
  )

#continous
acu_treatment_cont <- delta_results_cont_acu %>%
  rename(delta = acu_delta) %>%
  mutate(treatment = "Acupuncture")
acu_control_cont <- delta_results_cont_acu_placebo %>%
  rename(delta = acu_control_delta) %>%
  mutate(treatment = "Control")
acu_combined_cont <- bind_rows(acu_treatment_cont, acu_control_cont)

ggplot(acu_combined_cont, aes(x = estimate, y = delta)) +
  geom_point(aes(color = treatment, shape = treatment), size = 3, position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = treatment),
                 height = 0.3,
                 position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  labs(
    title = "Sensitivity Analysis: δ-Adjustment (Continous Estimand)",
    x = "Estimates",
    y = "Delta"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "none",  
    panel.background = element_rect(fill = "white")
  )

#combined plot
acu_treatment_cat <- delta_results_cat %>%
  rename(delta = acu_delta) %>%
  mutate(treatment = "Acupuncture", estimand = "Categorical")

acu_control_cat <- delta_results_cat_placebo %>%
  rename(delta = acu_control_delta) %>%
  mutate(treatment = "Control", estimand = "Categorical")

acu_treatment_cont <- delta_results_cont_acu %>%
  rename(delta = acu_delta) %>%
  mutate(treatment = "Acupuncture", estimand= "Continuous")

acu_control_cont <- delta_results_cont_acu_placebo %>%
  rename(delta = acu_control_delta) %>%
  mutate(treatment = "Control", estimand = "Continuous")

acu_combined <- bind_rows(
  acu_treatment_cat, acu_control_cat,
  acu_treatment_cont, acu_control_cont
)

#combined plot 
SA_acu_all<-ggplot(acu_combined, aes(x = estimate, y = delta, color = estimand, shape = estimand)) +
  geom_point(size = 3, position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  scale_shape_manual(values = c("Categorical" = 16, "Continuous" = 17)) + 
  scale_colour_manual(values=c("Categorical"="#a80050", "Continuous"="lawngreen"))+
  labs(
    title = "Sensitivity Analysis: δ-Adjustment (Combined Estimands)",
    x = "Estimates",
    y = "Delta"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white")
  )





#vital data set up 
vitd_cont <- delta_results_cont_vitd %>%
  rename(delta = vitd_delta) %>%
  mutate(treatment = "Vitamin D", estimand = "Continuous")

fishoil_cont <- delta_results_cont_fishoil %>%
  rename(delta = fishoil_delta) %>%
  mutate(treatment = "Fish Oil", estimand = "Continuous")

vital_control_cont <- delta_results_cont_vital_placebo %>%
  rename(delta = vital_control_delta) %>%
  mutate(treatment = "Control", estimand = "Continuous")

vitd_cat <- delta_results_cat_vitd %>%
  rename(delta = vitd_delta) %>%
  mutate(treatment = "Vitamin D", estimand= "Categorical")

fishoil_cat <- delta_results_cat_fishoil %>%
  rename(delta = fishoil_delta) %>%
  mutate(treatment = "Fish Oil", estimand = "Categorical")

vital_control_cat <- delta_results_cat_vital_placebo %>%
  rename(delta = vital_control_delta) %>%
  mutate(treatment = "Control", estimand = "Categorical")

vital_cont_combined <- bind_rows(vitd_cont, fishoil_cont, vital_control_cont)
vital_cat_combined<- bind_rows(vitd_cat, fishoil_cat, vital_control_cat)
vital_all_combined<-bind_rows(vital_cont_combined,vital_cat_combined)

#continous plot
SA_vital_cont_plot <- ggplot(vital_cont_combined, aes(x = estimate, y = delta)) +
  geom_point(aes(color = treatment, shape = treatment), size = 3, position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = treatment),
                 height = 0.3,
                 position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  labs(
    title = "Sensitivity Analysis: δ-Adjustment (Continous Estimand)",
    x = "Estimates",
    y = "Delta"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "none",  
    panel.background = element_rect(fill = "white")
  )

# categorical plot
SA_vital_cat_plot <- ggplot(vital_cat_combined, aes(x = estimate, y = delta)) +
  geom_point(aes(color = treatment, shape = treatment), size = 3, position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = treatment),
                 height = 0.3,
                 position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  labs(
    title = "Sensitivity Analysis: δ-Adjustment (Categorical Estimand)",
    x = "Estimates",
    y = "Delta"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "none",  
    panel.background = element_rect(fill = "white")
  )

#combining cat and cont vital 

SA_vital_all_plot <- ggplot(vital_all_combined, aes(x = estimate, y = delta, shape = estimand, colour = estimand)
) + geom_point( data = subset(vital_all_combined, estimand == "Categorical"),
  size = 4, position = position_nudge(y = -0.15)
) + geom_errorbarh(data = subset(vital_all_combined, estimand == "Categorical"), aes(xmin = conf.low, xmax = conf.high),
  height = 0.4, position = position_nudge(y = -0.15)
) + geom_point( data = subset(vital_all_combined, estimand == "Continuous"),
  size = 4, position = position_nudge(y = 0.15)
) + geom_errorbarh(data = subset(vital_all_combined, estimand == "Continuous"),
  aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)
) + geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  scale_shape_manual(values = c("Continuous" = 17, "Categorical" = 16)) +
  scale_color_manual(values = c("Continuous" = "lawngreen", "Categorical" = "#a80050")) +
  labs(
    title = "VITAL δ-Adjustment by Estimand and Treatment",
    x = "Estimates",
    y = "Delta",
    shape = "Estimand"
  ) +
  theme_minimal() + theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )



## heatmap for acupuncture

cat_control <- acu_combined %>%
  filter(estimand == "Categorical", treatment == "Control")
cat_acu <- acu_combined %>%
  filter(estimand == "Categorical", treatment == "Acupuncture")
cont_control <- acu_combined %>%
  filter(estimand == "Continuous", treatment == "Control")
cont_acu <- acu_combined %>%
  filter(estimand == "Continuous", treatment == "Acupuncture")
cat_heatmap_data <- tidyr::expand_grid(
  delta_1 = cat_control$delta,  
  delta_2 = cat_acu$delta      
) %>%
  left_join(cat_control %>% select(delta_1 = delta, placebo_est = estimate), by = "delta_1") %>%
  left_join(cat_acu %>% select(delta_2 = delta, acu_est = estimate), by = "delta_2") %>%
  mutate(
    treatment_effect = acu_est - placebo_est,
    estimand = "Categorical"
  )
cont_heatmap_data <- tidyr::expand_grid(
  delta_1 = cont_control$delta,
  delta_2 = cont_acu$delta
) %>%
  left_join(cont_control %>% select(delta_1 = delta, placebo_est = estimate), by = "delta_1") %>%
  left_join(cont_acu %>% select(delta_2 = delta, acu_est = estimate), by = "delta_2") %>%
  mutate(
    treatment_effect = acu_est - placebo_est,
    estimand = "Continuous"
  )

range_est_cat <- range(cat_heatmap_data$treatment_effect, na.rm = TRUE)
midpoint_cat <- mean(range_est_cat)
spread_cat <- max(abs(range_est_cat - midpoint_cat))

cat_heatmap_acu_plot<- ggplot(cat_heatmap_data, aes(x = delta_1, y = delta_2, fill = treatment_effect)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#a80050", mid = "white", high = "lawngreen",
    midpoint = midpoint_cat,
    limits = c(midpoint_cat - spread_cat, midpoint_cat + spread_cat)
  ) +
  labs(
    title = "Map1; original acu estimand",
    x = "Control Delta ",
    y = "Acupuncture Delta",
    fill = "Effect"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

range_est_cont <- range(cont_heatmap_data$treatment_effect, na.rm = TRUE)
midpoint_cont <- mean(range_est_cont)
spread_cont <- max(abs(range_est_cont - midpoint_cont))

cont_heatmap_acu_plot<-ggplot(cont_heatmap_data, aes(x = delta_1, y = delta_2, fill = treatment_effect)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#a80050", mid = "white", high = "lawngreen",
    midpoint = midpoint_cont,
    limits = c(midpoint_cont - spread_cont, midpoint_cont + spread_cont)
  ) +
  labs(
    title = "Map2; changed acu estimand",
    x = "δ control group",
    y = "δ acupuncture group",
    fill = "Effect"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())



#vital heatmap fishoil

cat_control <- vital_all_combined %>%
  filter(estimand == "Categorical", treatment == "Control")
cat_fishoil <- vital_all_combined %>%
  filter(estimand == "Categorical", treatment == "Fish Oil")
cont_control <- vital_all_combined %>%
  filter(estimand == "Continuous", treatment == "Control")
cont_fishoil <- vital_all_combined %>%
  filter(estimand == "Continuous", treatment == "Fish Oil")
cat_heatmap_data <- tidyr::expand_grid(
  delta_1 = cat_control$delta,  
  delta_2 = cat_fishoil$delta      
) %>%
  left_join(cat_control %>% select(delta_1 = delta, placebo_est = estimate), by = "delta_1") %>%
  left_join(cat_fishoil %>% select(delta_2 = delta, fishoil_est = estimate), by = "delta_2") %>%
  mutate(
    treatment_effect = fishoil_est - placebo_est,
    estimand = "Categorical"
  )
cont_heatmap_data <- tidyr::expand_grid(
  delta_1 = cont_control$delta,
  delta_2 = cont_fishoil$delta
) %>%
  left_join(cont_control %>% select(delta_1 = delta, placebo_est = estimate), by = "delta_1") %>%
  left_join(cont_fishoil %>% select(delta_2 = delta, fishoil_est = estimate), by = "delta_2") %>%
  mutate(
    treatment_effect = fishoil_est - placebo_est,
    estimand = "Continuous"
  )

range_est_cat <- range(cat_heatmap_data$treatment_effect, na.rm = TRUE)
midpoint_cat <- mean(range_est_cat)
spread_cat <- max(abs(range_est_cat - midpoint_cat))

cat_heatmap_fishoil_plot<- ggplot(cat_heatmap_data, aes(x = delta_1, y = delta_2, fill = treatment_effect)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#a80050", mid = "white", high = "lawngreen",
    midpoint = midpoint_cat,
    limits = c(midpoint_cat - spread_cat, midpoint_cat + spread_cat)
  ) +
  labs(
    title = "Map3; original fishoil estimand",
    x = "Control Delta ",
    y = "Fish Oil Delta",
    fill = "Effect"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

range_est_cont <- range(cont_heatmap_data$treatment_effect, na.rm = TRUE)
midpoint_cont <- mean(range_est_cont)
spread_cont <- max(abs(range_est_cont - midpoint_cont))

cont_heatmap_fishoil_plot<-ggplot(cont_heatmap_data, aes(x = delta_1, y = delta_2, fill = treatment_effect)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#a80050", mid = "white", high = "lawngreen",
    midpoint = midpoint_cont,
    limits = c(midpoint_cont - spread_cont, midpoint_cont + spread_cont)
  ) +
  labs(
    title ="Map4; changed fishoil estimand",
    x = "Control Delta ",
    y = "Fish Oil Delta",
    fill = "Effect"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())


#vital heatmap vitamin D

cat_control<- vital_all_combined %>%
  filter(estimand == "Categorical", treatment == "Control")
cat_vitd <- vital_all_combined %>%
  filter(estimand == "Categorical", treatment == "Vitamin D")
cont_control <- vital_all_combined %>%
  filter(estimand == "Continuous", treatment == "Control")
cont_vitd <- vital_all_combined %>%
  filter(estimand == "Continuous", treatment == "Vitamin D")
cat_heatmap_data <- tidyr::expand_grid(
  delta_1 = cat_control$delta,  
  delta_2 = cat_vitd$delta      
) %>%
  left_join(cat_control %>% select(delta_1 = delta, placebo_est = estimate), by = "delta_1") %>%
  left_join(cat_vitd %>% select(delta_2 = delta, vitd_est = estimate), by = "delta_2") %>%
  mutate(
    treatment_effect = vitd_est - placebo_est,
    estimand = "Categorical"
  )
cont_heatmap_data <- tidyr::expand_grid(
  delta_1 = cont_control$delta,
  delta_2 = cont_vitd$delta
) %>%
  left_join(cont_control %>% select(delta_1 = delta, placebo_est = estimate), by = "delta_1") %>%
  left_join(cont_vitd %>% select(delta_2 = delta, vitd_est = estimate), by = "delta_2") %>%
  mutate(
    treatment_effect = vitd_est - placebo_est,
    estimand = "Continuous"
  )

range_est_cat <- range(cat_heatmap_data$treatment_effect, na.rm = TRUE)
midpoint_cat <- mean(range_est_cat)
spread_cat <- max(abs(range_est_cat - midpoint_cat))

cat_heatmap_vitd_plot<- ggplot(cat_heatmap_data, aes(x = delta_1, y = delta_2, fill = treatment_effect)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#a80050", mid = "white", high = "lawngreen",
    midpoint = midpoint_cat,
    limits = c(midpoint_cat - spread_cat, midpoint_cat + spread_cat)
  ) +
  labs(
    title = "Map5; original vitd estimand",
    x = "Control Delta ",
    y = "VitD Delta",
    fill = "Effect"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

range_est_cont <- range(cont_heatmap_data$treatment_effect, na.rm = TRUE)
midpoint_cont <- mean(range_est_cont)
spread_cont <- max(abs(range_est_cont - midpoint_cont))

cont_heatmap_vitd_plot<-ggplot(cont_heatmap_data, aes(x = delta_1, y = delta_2, fill = treatment_effect)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#a80050", mid = "white", high = "lawngreen",
    midpoint = midpoint_cont,
    limits = c(midpoint_cont - spread_cont, midpoint_cont + spread_cont)
  ) +
  labs(
    title ="Map6; changed vitd estimand",
    x = "Control Delta ",
    y = "VitD Delta",
    fill = "Effect"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

