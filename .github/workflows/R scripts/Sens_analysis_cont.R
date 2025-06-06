# SENSITIVITY ANALYSIS : DELTA ADJUSTMENT 
#continuous acupuncutre
delta_acu <- c(-5, -2, 0, 2, 5)
inlist <- c("group", "pk1", "pk5", "time_c", "id")
pred_cont <- quickpred(acu_wide, minpuc = 0.5, include = inlist)
imp.all.undamped_cont <- vector("list", length(delta_acu))
delta_results_cont_acu <- data.frame()

for (i in seq_along(delta_acu)) {
  d <- delta_acu[i]
  imp_init <- mice(acu_wide, m = 5, maxit = 1, predictorMatrix = pred_cont, seed = 100 + i, print = FALSE)
  post_cont <- imp_init$post
  post_cont["pk5"] <- paste0(
    "idx <- which(data[,'group'] == 1 & is.na(data[,'pk5'])); ",
    "imp[[j]]$pk5[idx] <- imp[[j]]$pk5[idx] + ", d, ";"
  )
  imp_wide <- mice(acu_wide, m = 5, maxit = 10, predictorMatrix = pred_cont,
                   post = post_cont, seed = 200 + i, print = FALSE)
  imp_data_long <- complete(imp_wide, action = "long", include = TRUE) %>%
    to_long_format_acu_cont_MICE() %>%
    mutate(time_c = time - 12)
  imp_data_list <- split(imp_data_long, imp_data_long$.imp)
  fit_list <- lapply(imp_data_list, function(df) {
    lmer(pk_score ~ group * time_c + pk1 + (1 | id), data = df)
  })
  pooled_cont <- pool(fit_list)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "group") %>%
    select(estimate, std.error, conf.low, conf.high) %>%
    mutate(delta_acu = d)
  
  delta_results_cont_acu <- bind_rows(delta_results_cont_acu, est_cont)
}

delta_results_cont_acu$treatment<-"Acupuncture"
delta_results_cont_acu_plot <- ggplot(delta_results_cont_acu, aes(x = estimate, y = delta_acu)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) +
  labs(
    title = "Treatment effect with δ-Adjustment (continuous)",
    x = "Treatment Effect",
    y = "Delta"
  ) +
  theme_minimal()+ 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )



#continuous acu placebo
delta_acu <- c(-5, -2, 0, 2, 5)
inlist <- c("group", "pk1", "pk5", "time_c", "id")
acu_wide_placebo<-acu_wide%>%filter(group==0)
pred_cont_placebo <- quickpred(acu_wide_placebo, minpuc = 0.5, include = inlist)
imp.all.undamped_cont <- vector("list", length(delta_acu))
delta_results_cont_acu_placebo <- data.frame()

for (i in seq_along(delta_acu)) {
  d <- delta_acu[i]
  imp_init <- mice(acu_wide_placebo, m = 5, maxit = 1, predictorMatrix = pred_cont_placebo, seed = 100 + i, print = FALSE)
  post_cont <- imp_init$post
  post_cont["pk5"] <- paste0(
    "idx <- which(is.na(data[,'pk5'])); ",
    "imp[[j]]$pk5[idx] <- imp[[j]]$pk5[idx] + ", d, ";"
  )
  imp_wide <- mice(acu_wide_placebo, m = 5, maxit = 10, predictorMatrix = pred_cont_placebo,
                   post = post_cont, seed = 200 + i, print = FALSE)
  imp_data_long <- complete(imp_wide, action = "long", include = TRUE) %>%
    to_long_format_acu_cont_MICE() %>%
    mutate(time_c = time - 12)
  imp_data_list <- split(imp_data_long, imp_data_long$.imp)
  fit_list <- lapply(imp_data_list, function(df) {
    lmer(pk_score ~ time_c + pk1 + (1 | id), data = df)
  })
  pooled_cont <- pool(fit_list)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "(Intercept)") %>%
    select(estimate, std.error, conf.low, conf.high) %>%
    mutate(delta_acu = d)
  
  delta_results_cont_acu_placebo <- bind_rows(delta_results_cont_acu_placebo, est_cont)
}


delta_results_cont_acu_placebo$treatment<-"Placebo"
delta_results_cont_acu_placebo_plot <- ggplot(delta_results_cont_acu_placebo, aes(x = estimate, y = delta_acu)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) +
  labs(
    title = "Placebo effect with δ-Adjustment (continuous)",
    x = "Treatment Effect",
    y = "Delta"
  ) +
  theme_minimal()+ 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )



##VITAL FISHOIL

#continuous fishoil

delta_vital <- c(-10,-5,-2,0,2,5,10)
inlist <- c("fishoilactive", "vitdactive", "pain", "pain_base", "time_contin", "Subject_ID")
pred_cont <- quickpred(vital_wide, minpuc = 0.5, include = inlist)
imp.all.undamped_cont <- vector("list", length(delta_vital))
delta_results_cont_fishoil <- data.frame()

for (i in seq_along(delta_vital)) {
  d <- delta_vital[i]
  imp_init <- mice(vital_wide, m = 5, maxit = 1, predictorMatrix = pred_cont, seed = 100 + i, print = FALSE)
  post_cont <- imp_init$post
  post_cont["pain_yr4"] <- paste0(
    "idx <- which(data[,'fishoilactive'] == 1 & data[,'vitdactive'] == 0 & is.na(data[,'pain_yr4'])); ",
    "imp[[j]]$pain_yr4[idx] <- imp[[j]]$pain_yr4[idx] + ", d
  )
  imp_wide <- mice(vital_wide, m = 5, maxit = 10, predictorMatrix = pred_cont,
                   post = post_cont, seed = 200 + i, print = FALSE)
  imp_data_long <- complete(imp_wide, action = "long", include = TRUE) %>%
    to_long_format_vital_mice()
  imp_data_list <- split(imp_data_long, imp_data_long$.imp)
  fit_list <- lapply(imp_data_list, function(df) {
    lmer(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base + 
           (1|Subject_ID), data = df)
  })
  pooled_cont <- pool(fit_list)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "fishoilactive") %>%
    select(estimate, std.error, conf.low, conf.high) %>%
    mutate(delta_vital = d)
  
  delta_results_cont_fishoil <- bind_rows(delta_results_cont_fishoil, est_cont)
}

delta_results_cont_fishoil$treatment<-"Fish Oil"
delta_results_cont_fishoil_plot <- ggplot(delta_results_cont_fishoil, aes(x = estimate, y = delta_vital)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) + 
  labs(
    title = "Treatment effect with δ-Adjustment (continuous)",
    x = "Treatment Effect",
    y = "Delta"
  ) +
  theme_minimal()+ 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


#continuous placebo

inlist <- c("fishoilactive", "vitdactive", "pain", "pain_base", "time_contin", "Subject_ID")
pred_cont <- quickpred(vital_wide, minpuc = 0.5, include = inlist)
imp.all.undamped_cont <- vector("list", length(delta_vital))
delta_results_cont_vital_placebo <- data.frame()

for (i in seq_along(delta_vital)) {
  d <- delta_vital[i]
  imp_init <- mice(vital_wide, m = 5, maxit = 1, predictorMatrix = pred_cont, seed = 100 + i, print = FALSE)
  post_cont <- imp_init$post
  post_cont["pain_yr4"] <- paste0(
    "idx <- which(data[,'fishoilactive'] == 0 & data[,'vitdactive'] == 0 & is.na(data[,'pain_yr4'])); ",
    "imp[[j]]$pain_yr4[idx] <- imp[[j]]$pain_yr4[idx] + ", d
  )
  imp_wide <- mice(vital_wide, m = 5, maxit = 10, predictorMatrix = pred_cont,
                   post = post_cont, seed = 200 + i, print = FALSE)
  imp_data_long <- complete(imp_wide, action = "long", include = TRUE) %>%
    to_long_format_vital_mice()
  imp_data_list <- split(imp_data_long, imp_data_long$.imp)
  fit_list <- lapply(imp_data_list, function(df) {
    df_placebo <- df %>%
      filter(fishoilactive == 0 & vitdactive == 0)
    lmer(pain ~ time_contin + pain_base + (1 | Subject_ID), data = df_placebo)
})
  pooled_cont <- pool(fit_list)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "time_contin") %>%
    select(estimate, std.error, conf.low, conf.high) %>%
    mutate(delta_vital = d)
  
  delta_results_cont_vital_placebo <- bind_rows(delta_results_cont_vital_placebo, est_cont)
}
delta_results_cont_vital_placebo$treatment<-"Placebo"
delta_results_cont_vital_placebo_plot <- ggplot(delta_results_cont_vital_placebo, aes(x = estimate, y = delta_vital)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) + 
  labs(
    title = "VITAL Placebo with δ-Adjustment (continuous)",
    x = "Treatment Effect",
    y = "Delta"
  ) +
  theme_minimal()+ 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )





##VITAL VITAMIN D

#continuous
inlist <- c("fishoilactive", "vitdactive", "pain", "pain_base", "time_contin", "Subject_ID")
pred_cont <- quickpred(vital_wide, minpuc = 0.5, include = inlist)
imp.all.undamped_cont <- vector("list", length(delta_vital))
delta_results_cont_vitd <- data.frame()

for (i in seq_along(delta_vital)) {
  d <- delta_vital[i]
  imp_init <- mice(vital_wide, m = 5, maxit = 1, predictorMatrix = pred_cont, seed = 100 + i, print = FALSE)
  post_cont <- imp_init$post
  post_cont["pain_yr4"] <- paste0(
    "idx <- which(data[,'fishoilactive'] == 0 & data[,'vitdactive'] == 1 & is.na(data[,'pain_yr4'])); ",
    "imp[[j]]$pain_yr4[idx] <- imp[[j]]$pain_yr4[idx] + ", d
  )
  imp_wide <- mice(vital_wide, m = 5, maxit = 10, predictorMatrix = pred_cont,
                   post = post_cont, seed = 200 + i, print = FALSE)
  imp_data_long <- complete(imp_wide, action = "long", include = TRUE) %>%
    to_long_format_vital_mice()
  imp_data_list <- split(imp_data_long, imp_data_long$.imp)
  fit_list <- lapply(imp_data_list, function(df) {
    lmer(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base + 
           (1|Subject_ID), data = df)
  })
  pooled_cont <- pool(fit_list)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "vitdactive") %>%
    select(estimate, std.error, conf.low, conf.high) %>%
    mutate(delta_vital = d)
  
  delta_results_cont_vitd <- bind_rows(delta_results_cont_vitd, est_cont)
}

delta_results_cont_vitd$treatment<-"Vitamin D"
delta_results_cont_vitd_plot <- ggplot(delta_results_cont_vitd, aes(x = estimate, y = delta_vital)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) +
  labs(
    title = "Treatment effect of VitD with δ-Adjustment (continuous)",
    x = "Treatment Effect",
    y = "Delta"
  ) +
  theme_minimal()+ 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


