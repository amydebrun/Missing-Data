#COMBINING THE TWO PLOTS TOGETHER - Acupuncture 
#cont
delta_combined_cont_acu <- bind_rows(delta_results_cont_acu, delta_results_cont_acu_placebo)
delta_combined_acu_plot <- ggplot(delta_combined_cont_acu, aes(x = estimate, y = delta)) +
  geom_point(size = 4, color = "#a80050", position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, 
                 position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  labs(
    title = "δ-Adjustment Sensitivity Analysis (Continuous Outcomes)",
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
delta_combined_cat_acu_plot <- ggplot(delta_combined_cat_acu, aes(x = estimate, y = delta)) +
  geom_point(size = 4, color = "#a80050", position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, 
                 position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ treatment) +
  labs(
    title = "δ-Adjustment Sensitivity Analysis (Categorical Outcomes)",
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

delta_combined_cont_acu$estimand <- "Continuous time"
delta_combined_cat_acu$estimand <- "Categorical time"
delta_combined_cont_cat <- bind_rows(delta_combined_cont_acu, delta_combined_cat_acu)

delta_combined_cont_cat_plot <- ggplot(delta_combined_cont_cat, aes(x = estimate, y = delta)) +
  geom_point(size = 4, color = "#a80050", position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4,
                 position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_grid(outcome_type ~ treatment) +
  labs(
    title = "δ-Adjustment Sensitivity Analysis: Continuous & Categorical estimand",
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




































delta_results_cat$tme<- "Categorical time"
delta_results_cont_acu$tme <- "Continuous time"
delta_combined_acu <- bind_rows(delta_results_cat, delta_results_cont_acu)
delta_combined_acu$group<-"Acupuncture"

delta_combined_acu <- delta_combined_acu %>%
  mutate(delta_jittered = case_when(
    tme == "Categorical time" ~ delta + 0.15,
    tme == "Continuous time"  ~ delta - 0.15,
    TRUE ~ delta
  ))

delta_compare_acu <- ggplot(delta_combined_acu, aes(x = estimate, y = delta_jittered)) +
  geom_point(aes(color = tme, shape = tme), size = 4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = tme), height = 0.4, position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + facet_wrap(~group, scales="free") +
  scale_color_manual(
    values = c(
      "Categorical time" = "lawngreen",   
      "Continuous time" = "#a80050"     
    )
  ) +
  scale_shape_manual(
    values = c(
      "Categorical time" = 17,  
      "Continuous time" = 16    
    )
  ) +
  labs(
    title = "",
    x = "Treatment Effect",
    y = "Delta",
    color = "Time Type",
    shape = "Time Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "lawngreen", color = "black")
  )



#COMBINING THE TWO PLOTS TOGETHER - PLACEBO

delta_results_cat_placebo$tme<- "Categorical time"
delta_results_cont_acu_placebo$tme <- "Continuous time"
delta_combined_acu <- bind_rows(delta_results_cat_placebo, delta_results_cont_acu_placebo)
delta_combined_acu_placebo$group<-"Placebo"

delta_combined_acu_placebo <- delta_combined_acu_placebo %>%
  mutate(delta_jittered = case_when(
    tme == "Categorical time" ~ delta + 0.15,
    tme == "Continuous time"  ~ delta - 0.15,
    TRUE ~ delta
  ))

delta_compare_acu_placebo <- ggplot(delta_combined_acu_placebo, aes(x = estimate, y = delta_jittered)) +
  geom_point(aes(color = tme, shape = tme), size = 4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = tme), height = 0.4, position = position_nudge(y = 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + facet_wrap(~group, scales="free") +
  scale_color_manual(
    values = c(
      "Categorical time" = "lawngreen",   
      "Continuous time" = "#a80050"     
    )
  ) +
  scale_shape_manual(
    values = c(
      "Categorical time" = 17,  
      "Continuous time" = 16    
    )
  ) +
  labs(
    title = "Placebo effect comparison",
    x = "Treatment Effect",
    y = "Delta",
    color = "Time Type",
    shape = "Time Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "lawngreen", color = "black")
  )







#combine vitamin d plots 
delta_results_cat_vitd$tme<-"Categorical Time"
delta_results_cont_vitd$tme<- "Continuous Time"
delta_combined_vitd<-bind_rows(delta_results_cat_vitd, delta_results_cont_vitd)
delta_combined_vitd$treatment<-"Vitamin D"

delta_combined_vitd<- delta_combined_vitd %>%
  mutate(delta_jittered = case_when(
    tme == "Categorical Time" ~ delta + 0.15,
    tme == "Continuous Time"  ~ delta - 0.15,
    TRUE ~ delta
  ))

delta_combined_vitd_plot <- ggplot(delta_combined_vitd, aes(x = estimate, y = delta_jittered, color = tme, shape = tme)) +
  geom_point(size = 4,position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + facet_wrap(~treatment)+
  scale_color_manual(
    values = c(
      "Categorical Time" = "lawngreen",   
      "Continuous Time" = "#a80050"     
    )
  ) +
  scale_shape_manual(
    values = c(
      "Categorical Time" = 17,  
      "Continuous Time" = 16   
    )
  ) +
  labs(
    title = "Treatment Effect with δ-Adjustment in Vitamin D Data",
    x = "Treatment Effect",
    y = "Delta",
    color = "Time Type",
    shape = "Time Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "lawngreen", color = "black")
  )


# facetting vitamin d and fishoil delta approach 

delta_combined_vitd$treatment <- "Vitamin D"
delta_combined_fishoil$treatment <- "Fish Oil"
delta_combined_vital <- bind_rows(delta_combined_vitd, delta_combined_fishoil)

delta_combined_vital <- delta_combined_vital %>%
  mutate(delta_jittered = case_when(
    tme == "Categorical Time" ~ delta + 0.15,
    tme == "Continuous Time"  ~ delta - 0.15,
    TRUE ~ delta
  ))

delta_combined_vital_plot <- ggplot(delta_combined_vital, aes(x = estimate, y = delta_jittered, color = tme, shape = tme)) +
  geom_point(size = 4,position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  scale_color_manual(
    values = c(
      "Categorical Time" = "lawngreen",   
      "Continuous Time" = "#a80050"     
    )
  ) +
  scale_shape_manual(
    values = c(
      "Categorical Time" = 17,  
      "Continuous Time" = 16   
    )
  ) +
  facet_wrap(~ treatment) +
  labs(
    title = "",
    x = "Treatment Effect",
    y = "Delta",
    color = "Time Type",
    shape = "Time Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA), 
    strip.background = element_rect(fill = "lawngreen", color = "black"),
  )


# combine fishoil sensitivity analysis plots

delta_results_cat_fishoil$tme<-"Categorical Time"
delta_results_cont_fishoil$tme<- "Continuous Time"
delta_combined_fishoil<-bind_rows(delta_results_cat_fishoil, delta_results_cont_fishoil)
delta_combined_fishoil$treatment<- "Fish Oil"

delta_combined_fishoil <- delta_combined_fishoil %>%
  mutate(delta_jittered = case_when(
    tme == "Categorical Time" ~ delta + 0.15,
    tme == "Continuous Time"  ~ delta - 0.15,
    TRUE ~ delta
  ))

delta_combined_fishoil_plot <- ggplot(delta_combined_fishoil, aes(x = estimate, y = delta_jittered, color = tme, shape = tme)) +
  geom_point(size = 4,position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  facet_wrap(~ treatment) +  
  scale_color_manual(
    values = c(
      "Categorical Time" = "lawngreen",   
      "Continuous Time" = "#a80050"     
    )
  ) +
  scale_shape_manual(
    values = c(
      "Categorical Time" = 17,  
      "Continuous Time" = 16   
    )
  ) +
  labs(
    title = "Treatment Effect with δ-Adjustment in Fish Oil Data",
    x = "Treatment Effect",
    y = "Delta",
    color = "Time Type",
    shape = "Time Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "lawngreen", color = "black")
  )




















