#COMBINING THE TWO PLOTS TOGETHER - Acupuncture 
delta_acu <- c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5)
delta_vital <- c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5)
#cont
delta_combined_cont_acu <- bind_rows(delta_results_cont_acu, delta_results_cont_acu_placebo)
delta_combined_cont_acu_plot <- ggplot(delta_combined_cont_acu, aes(x = estimate, y = delta_acu)) +
  geom_point(aes(x = estimate, y = delta_acu), size = 4, color = "#a80050", position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, 
                 position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  labs(
    title = "δ-Adjustment Sensitivity Analysis (Continuous)",
    x = "Estimated Treatment Effect",
    y = "Delta"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#cat
delta_combined_cat_acu <- bind_rows(delta_results_cat, delta_results_cat_placebo)
delta_combined_cat_acu_plot <- ggplot(delta_combined_cat_acu, aes(x = estimate, y = delta_acu)) +
  geom_point(size = 4, color = "#a80050", position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, 
                 position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  labs(
    title = "δ-Adjustment Sensitivity Analysis (Categorical)",
    x = "Estimated Treatment Effect",
    y = "Delta"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#combining cat and cont 
delta_combined_cat_acu$estimand<- "Categorical time"
delta_combined_cont_acu$estimand<- "Continuous time"
delta_combined_acu_cont_cat<- bind_rows(delta_combined_cat_acu,delta_combined_cont_acu)
delta_combined_acu_cont_cat$delta_factor <- factor(delta_combined_acu_cont_cat$delta_acu,
                                                   levels = sort(unique(delta_combined_acu_cont_cat$delta_acu)))

SA_combined_acu_plot <- ggplot(
  delta_combined_acu_cont_cat, 
  aes(x = estimate, y = delta_factor, shape = estimand, colour = estimand)
) + geom_point(
    data = subset(delta_combined_acu_cont_cat, estimand == "Categorical time"),
    size = 4, position = position_nudge(y = -0.15)
  ) + geom_errorbarh(
    data = subset(delta_combined_acu_cont_cat, estimand == "Categorical time"),
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.4,
    position = position_nudge(y = -0.15)
  ) + geom_point(
    data = subset(delta_combined_acu_cont_cat, estimand == "Continuous time"),
    size = 4, position = position_nudge(y = 0.15)
  ) + geom_errorbarh(
    data = subset(delta_combined_acu_cont_cat, estimand == "Continuous time"),
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.4,
    position = position_nudge(y = 0.15)
  ) + geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  scale_shape_manual(values = c("Continuous time" = 17, "Categorical time" = 16)) +
  scale_color_manual(values = c("Continuous time" = "lawngreen", "Categorical time" = "#a80050")) +
  labs(
    title = "δ-Adjustment by Estimand and Treatment",
    x = "Estimated Treatment Effect",
    y = "Delta",
    shape = "Estimand",
    colour = "Estimand"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

#vital data continous 

SA_combined_vital_cont <- bind_rows(delta_results_cont_fishoil, delta_results_cont_vitd, delta_results_cont_vital_placebo)
SA_combined_vital_cont_plot <- ggplot(SA_combined_vital_cont, aes(x = estimate, y = delta_vital)) +
  geom_point(aes(x = estimate, y = delta_vital), size = 4, color = "#a80050", position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, 
                 position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  labs(
    title = "δ-Adjustment Sensitivity Analysis (Continuous)",
    x = "Estimated Treatment Effect",
    y = "Delta"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#vital data categorical 

SA_combined_vital_cat <- bind_rows(delta_results_cat_fishoil, delta_results_cat_vitd, delta_results_cat_vital_placebo)
SA_combined_vital_cat_plot <- ggplot(SA_combined_vital_cat, aes(x = estimate, y = delta_vital)) +
  geom_point(aes(x = estimate, y = delta_vital), size = 4, color = "#a80050", position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, 
                 position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  labs(
    title = "δ-Adjustment Sensitivity Analysis (Categorical)",
    x = "Estimated Treatment Effect",
    y = "Delta"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#combining cat and cont vital 

SA_combined_vital_cat$estimand<- "Categorical"
SA_combined_vital_cont$estimand<- "Continuous"
SA_combined_vital_all<- bind_rows(SA_combined_vital_cat,SA_combined_vital_cont)

SA_combined_vital_all_plot <- ggplot(SA_combined_vital_all, aes(x = estimate, y = delta_vital, shape = estimand, colour = estimand)
) + geom_point( data = subset(SA_combined_vital_all, estimand == "Categorical"),
  size = 4, position = position_nudge(y = -0.15)
) + geom_errorbarh(data = subset(SA_combined_vital_all, estimand == "Categorical"), aes(xmin = conf.low, xmax = conf.high),
  height = 0.4, position = position_nudge(y = -0.15)
) + geom_point( data = subset(SA_combined_vital_all, estimand == "Continuous"),
  size = 4, position = position_nudge(y = 0.15)
) + geom_errorbarh(data = subset(SA_combined_vital_all, estimand == "Continuous"),
  aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)
) + geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  scale_shape_manual(values = c("Continuous" = 17, "Categorical" = 16)) +
  scale_color_manual(values = c("Continuous" = "lawngreen", "Categorical" = "#a80050")) +
  labs(
    title = "VITAL δ-Adjustment by Estimand and Treatment",
    x = "Estimated Treatment Effect",
    y = "Delta",
    shape = "Estimand",
    colour = "Estimand"
  ) +
  theme_minimal() + theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )







## heatmap for acupuncture

cat_placebo <- delta_combined_acu_cont_cat %>%
  filter(estimand == "Categorical time", treatment == "Placebo")
cat_acu <- delta_combined_acu_cont_cat %>%
  filter(estimand == "Categorical time", treatment == "Acupuncture")
cont_placebo <- delta_combined_acu_cont_cat %>%
  filter(estimand == "Continuous time", treatment == "Placebo")
cont_acu <- delta_combined_acu_cont_cat %>%
  filter(estimand == "Continuous time", treatment == "Acupuncture")
cat_heatmap_data <- tidyr::expand_grid(
  delta_1 = cat_placebo$delta_acu,  
  delta_2 = cat_acu$delta_acu       
) %>%
  left_join(cat_placebo %>% select(delta_1 = delta_acu, placebo_est = estimate), by = "delta_1") %>%
  left_join(cat_acu %>% select(delta_2 = delta_acu, acu_est = estimate), by = "delta_2") %>%
  mutate(
    treatment_effect = acu_est - placebo_est,
    estimand = "Categorical time"
  )
cont_heatmap_data <- tidyr::expand_grid(
  delta_1 = cont_placebo$delta_acu,
  delta_2 = cont_acu$delta_acu
) %>%
  left_join(cont_placebo %>% select(delta_1 = delta_acu, placebo_est = estimate), by = "delta_1") %>%
  left_join(cont_acu %>% select(delta_2 = delta_acu, acu_est = estimate), by = "delta_2") %>%
  mutate(
    treatment_effect = acu_est - placebo_est,
    estimand = "Continuous time"
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
    title = "Map1; original estimand",
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
    title = "Map2; changed estimand",
    x = "Control Delta ",
    y = "Acupuncture Delta",
    fill = "Effect"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())



#vital heatmap fishoil

cat_placebo <- SA_combined_vital_all %>%
  filter(estimand == "Categorical", treatment == "Control")
cat_fishoil <- SA_combined_vital_all %>%
  filter(estimand == "Categorical", treatment == "Fish Oil")
cont_placebo <- SA_combined_vital_all %>%
  filter(estimand == "Continuous", treatment == "Control")
cont_fishoil <- SA_combined_vital_all %>%
  filter(estimand == "Continuous", treatment == "Fish Oil")
cat_heatmap_data <- tidyr::expand_grid(
  delta_1 = cat_placebo$delta_vital,  
  delta_2 = cat_fishoil$delta_vital      
) %>%
  left_join(cat_placebo %>% select(delta_1 = delta_vital, placebo_est = estimate), by = "delta_1") %>%
  left_join(cat_fishoil %>% select(delta_2 = delta_vital, fishoil_est = estimate), by = "delta_2") %>%
  mutate(
    treatment_effect = fishoil_est - placebo_est,
    estimand = "Categorical"
  )
cont_heatmap_data <- tidyr::expand_grid(
  delta_1 = cont_placebo$delta_vital,
  delta_2 = cont_fishoil$delta_vital
) %>%
  left_join(cont_placebo %>% select(delta_1 = delta_vital, placebo_est = estimate), by = "delta_1") %>%
  left_join(cont_fishoil %>% select(delta_2 = delta_vital, fishoil_est = estimate), by = "delta_2") %>%
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

cat_placebo <- SA_combined_vital_all %>%
  filter(estimand == "Categorical", treatment == "Control")
cat_vitd <- SA_combined_vital_all %>%
  filter(estimand == "Categorical", treatment == "Vitamin D")
cont_placebo <- SA_combined_vital_all %>%
  filter(estimand == "Continuous", treatment == "Control")
cont_vitd <- SA_combined_vital_all %>%
  filter(estimand == "Continuous", treatment == "Vitamin D")
cat_heatmap_data <- tidyr::expand_grid(
  delta_1 = cat_placebo$delta_vital,  
  delta_2 = cat_vitd$delta_vital      
) %>%
  left_join(cat_placebo %>% select(delta_1 = delta_vital, placebo_est = estimate), by = "delta_1") %>%
  left_join(cat_vitd %>% select(delta_2 = delta_vital, vitd_est = estimate), by = "delta_2") %>%
  mutate(
    treatment_effect = vitd_est - placebo_est,
    estimand = "Categorical"
  )
cont_heatmap_data <- tidyr::expand_grid(
  delta_1 = cont_placebo$delta_vital,
  delta_2 = cont_vitd$delta_vital
) %>%
  left_join(cont_placebo %>% select(delta_1 = delta_vital, placebo_est = estimate), by = "delta_1") %>%
  left_join(cont_vitd %>% select(delta_2 = delta_vital, vitd_est = estimate), by = "delta_2") %>%
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

