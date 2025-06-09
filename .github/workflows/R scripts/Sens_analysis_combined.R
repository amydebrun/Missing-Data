#COMBINING THE TWO PLOTS TOGETHER - Acupuncture 
delta_vital <- c(-10,-5,-2,0,2,5,10)
delta_acu <- c(-5,-2,0,2,5)
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

delta_combined_cont_cat_plot <- ggplot(
  delta_combined_acu_cont_cat, 
  aes(x = estimate, y = delta_acu, shape = estimand, colour = estimand)
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
    legend.position = "bottom"
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
