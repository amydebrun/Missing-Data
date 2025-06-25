
#standard deviations vital
vital_fish<-vital_long%>%
  filter(fishoilactive==1 & vitdactive==0)

vital_vitd<-vital_long%>%
  filter(fishoilactive==0 & vitdactive==1)

vital_control<-vital_long%>%
  filter(fishoilactive==0 & vitdactive==0)

vital_fish_SD<-sd(vital_fish$pain, na.rm = TRUE)
vital_vitd_SD<-sd(vital_vitd$pain, na.rm = TRUE)
vital_control_SD<-sd(vital_control$pain, na.rm = TRUE)

#standard deviations in both groups acupuncture trial
acu<-acu_long%>%
  filter(group==1)
acu_SD<-sd(acu$pk_score, na.rm=TRUE)

acu_control<-acu_long%>%
  filter(group==0)
acu_control_SD<-sd(acu_control$pk_score, na.rm=TRUE)

#delta acu
acu_delta<-c(-0.5*acu_SD,-0.25*acu_SD,0,0.25*acu_SD,0.5*acu_SD)
acu_control_delta<-c(-0.5*acu_control_SD,-0.25*acu_control_SD,0,0.25*acu_control_SD,0.5*acu_control_SD)

#delta vital
#fishoil
fishoil_delta<-c(-0.5*vital_fish_SD,-0.25*vital_fish_SD,0,0.25*vital_fish_SD,0.5*vital_fish_SD)
vitd_delta<-c(-0.5*vital_vitd_SD,-0.25*vital_vitd_SD,0,0.25*vital_vitd_SD,0.5*vital_vitd_SD)
vital_control_delta<-c(-0.5*vital_control_SD,-0.25*vital_control_SD,0,0.25*vital_control_SD,0.5*vital_control_SD)



#continuous acupuncutre

inlist <- c("group", "pk1", "pk5", "time_c", "id")
pred_cont <- quickpred(acu_wide, minpuc = 0.5, include = inlist)
imp.all.undamped_cont <- vector("list", length(acu_delta))
delta_results_cont_acu <- data.frame()

for (i in seq_along(acu_delta)) {
  d <- acu_delta[i]
  imp_init <- mice(acu_wide, m = 5, maxit = 1, predictorMatrix = pred_cont, seed = 100 + i, print = FALSE)
  method_cont <- imp_init$method
  method_cont[c("pk2", "pk5")] <- "pmm"
  post_cont <- imp_init$post
  post_cont["pk5"] <- paste0(
    "idx <- which(data[,'group'] == 1 & is.na(data[,'pk5'])); ",
    "imp[[j]]$pk5[idx] <- imp[[j]]$pk5[idx] + ", d, ";"
  )
  imp_wide <- mice(acu_wide, m = 5, maxit = 10, predictorMatrix = pred_cont,
                   post = post_cont, method=method_cont, seed = 200 + i, print = FALSE)
  imp.all.undamped_cont[[i]] <- imp_wide 
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
    mutate(acu_delta = d)
  
  delta_results_cont_acu <- bind_rows(delta_results_cont_acu, est_cont)
}

delta_results_cont_acu$treatment<-"Acupuncture"
delta_results_cont_acu_plot <- ggplot(delta_results_cont_acu, aes(x = estimate, y = acu_delta)) +
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

inlist <- c("group", "pk1", "pk5", "time_c", "id")
acu_wide_placebo<-acu_wide%>%filter(group==0)
pred_cont_placebo <- quickpred(acu_wide_placebo, minpuc = 0.5, include = inlist)
imp.all.undamped_cont <- vector("list", length(acu_control_delta))
delta_results_cont_acu_placebo <- data.frame()

for (i in seq_along(acu_control_delta)) {
  d <- acu_control_delta[i]
  imp_init <- mice(acu_wide_placebo, m = 5, maxit = 1, predictorMatrix = pred_cont_placebo, seed = 100 + i, print = FALSE)
  method_cont <- imp_init$method
  post_cont <- imp_init$post
  method_cont[c("pk2", "pk5")] <- "pmm"
  post_cont <- imp_init$post
  post_cont["pk5"] <- paste0(
    "idx <- which(is.na(data[,'pk5'])); ",
    "imp[[j]]$pk5[idx] <- imp[[j]]$pk5[idx] + ", d, ";"
  )
  imp_wide <- mice(acu_wide_placebo, m = 5, maxit = 10, predictorMatrix = pred_cont_placebo, method=method_cont,
                   post = post_cont, seed = 200 + i, print = FALSE)
  imp.all.undamped_cont[[i]] <- imp_wide 
  imp_data_long <- complete(imp_wide, action = "long", include = TRUE) %>%
    to_long_format_acu_cont_MICE() %>%
    mutate(time_c = time - 12)
  imp_data_list <- split(imp_data_long, imp_data_long$.imp)
  fit_list <- lapply(imp_data_list, function(df) {
    lmer(pk_score ~ time_c + pk1 + (1 | id), data = df)
  })
  pooled_cont <- pool(fit_list)
  pooled_summary <- tidy(pooled_cont, conf.int = TRUE)
  intercept_est <- pooled_summary$estimate[pooled_summary$term == "(Intercept)"]
  slope_est <- pooled_summary$estimate[pooled_summary$term == "pk1"]
  var_int <- (pooled_summary$std.error[pooled_summary$term == "(Intercept)"])^2
  var_slope <- (pooled_summary$std.error[pooled_summary$term == "pk1"])^2
  cov_int_slope <- 0
  mean_pk1 <- mean(complete(imp_wide, action = 1)$pk1, na.rm = TRUE)
  est_val <- intercept_est + slope_est * mean_pk1
  var_val <- var_int + (mean_pk1^2) * var_slope + 2 * mean_pk1 * cov_int_slope
  se_val <- sqrt(var_val)
  est_cont <- data.frame(
    estimate = est_val,
    std.error = se_val,
    conf.low = est_val - 1.96 * se_val,
    conf.high = est_val + 1.96 * se_val,
    acu_control_delta = d
  )
  
  delta_results_cont_acu_placebo <- bind_rows(delta_results_cont_acu_placebo, est_cont)
}


delta_results_cont_acu_placebo$treatment<-"Placebo"
delta_results_cont_acu_placebo_plot <- ggplot(delta_results_cont_acu_placebo, aes(x = estimate, y = acu_control_delta)) +
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

#continuous placebo
 
inlist <- c("fishoilactive", "vitdactive", "pain", "pain_base", "time_contin", "Subject_ID")
vital_wide_placebo <- vital_wide %>% filter(fishoilactive == 0, vitdactive == 0)
pred_cont_placebo <- quickpred(vital_wide_placebo, minpuc = 0.5, include = inlist)
imp.all.undamped_cont <- vector("list", length(vital_control_delta))
delta_results_cont_vital_placebo <- data.frame()

for (i in seq_along(vital_control_delta)) {
  d <- vital_control_delta[i]
  imp_init <- mice(vital_wide_placebo, m = 5, maxit = 1, predictorMatrix = pred_cont_placebo, seed = 100 + i, print = FALSE)
  post_cont <- imp_init$post
  method_cont <- imp_init$method
  years <- paste0("pain_yr", 1:4)
  method_cont[years] <- "pmm"
  method_cont["pain_base"] <- "pmm"
  for (yr in years) {
    post_cont[yr] <- paste0(
      "idx <- which(is.na(data[,'", yr, "'])); ",
      "imp[[j]]$", yr, "[idx] <- imp[[j]]$", yr, "[idx] + ", d, ";"
    )
  }
  
  imp_wide <- mice(vital_wide_placebo, m = 5, maxit = 10, predictorMatrix = pred_cont_placebo,
                   post = post_cont, method = method_cont, seed = 200 + i, print = FALSE)
  
  imp.all.undamped_cont[[i]] <- imp_wide
  imp_data_long <- complete(imp_wide, action = "long", include = TRUE) %>%
    to_long_format_vital_mice_SA()
  imp_data_list <- split(imp_data_long, imp_data_long$.imp)
  fit_list <- lapply(imp_data_list, function(df) {
    lmer(pain ~ time_contin + pain_base + (1 | Subject_ID), data = df)
  })
  pooled_cont <- pool(fit_list)
  pooled_summary <- tidy(pooled_cont, conf.int = TRUE)
  intercept_est <- pooled_summary$estimate[pooled_summary$term == "(Intercept)"]
  slope_est <- pooled_summary$estimate[pooled_summary$term == "pain_base"]
  var_int <- pooled_summary$std.error[pooled_summary$term == "(Intercept)"]^2
  var_slope <- pooled_summary$std.error[pooled_summary$term == "pain_base"]^2
  cov_int_slope <- 0
  mean_pain_base <- mean(complete(imp_wide, action = 1)$pain_base, na.rm = TRUE)
  est_val <- intercept_est + slope_est * mean_pain_base
  var_val <- var_int + (mean_pain_base^2) * var_slope + 2 * mean_pain_base * cov_int_slope
  se_val <- sqrt(var_val)
  est_cont <- data.frame(
    estimate = est_val,
    std.error = se_val,
    conf.low = est_val - 1.96 * se_val,
    conf.high = est_val + 1.96 * se_val,
    vital_control_delta = d
  )
  
  delta_results_cont_vital_placebo <- bind_rows(delta_results_cont_vital_placebo, est_cont)
}
delta_results_cont_vital_placebo$treatment<-"Control"
delta_results_cont_vital_placebo_plot <- ggplot(delta_results_cont_vital_placebo, aes(x = estimate, y = vital_control_delta)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) + 
  labs(
    title = "VITAL Placebo with δ-Adjustment (continuous)",
    x = "Mean Knee pain at year 4",
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
imp.all.undamped_cont <- vector("list", length(vitd_delta))
delta_results_cont_vitd <- data.frame()

for (i in seq_along(vitd_delta)) {
  d <- vitd_delta[i]
  imp_init <- mice(vital_wide, m = 5, maxit = 1, predictorMatrix = pred_cont, seed = 100 + i, print = FALSE)
  post_cont <- imp_init$post
  method_cont <- imp_init$method
  years <- paste0("pain_yr", 1:4)
  method_cont[years] <- "pmm"
  method_cont["pain_base"] <- "pmm"
  for (yr in years) {
    post_cont[yr] <- paste0(
      "idx <- which(data[,'fishoilactive'] == 0 & data[,'vitdactive'] == 1 & is.na(data[,'", yr, "'])); ",
      "imp[[j]]$", yr, "[idx] <- imp[[j]]$", yr, "[idx] + ", d, ";"
    )
  }
  imp_wide <- mice(vital_wide, m = 5, maxit = 10, predictorMatrix = pred_cont,
                   post = post_cont, method=method_cont, seed = 200 + i, print = FALSE)
  imp.all.undamped_cont[[i]] <- imp_wide 
  imp_data_long <- complete(imp_wide, action = "long", include = TRUE) %>%
    to_long_format_vital_mice_SA()
  imp_data_list <- split(imp_data_long, imp_data_long$.imp)
  
  fit_list <- lapply(imp_data_list, function(df) {
    lmer(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base + 
           (1 | Subject_ID), data = df)
  })
  pooled_cont <- pool(fit_list)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "vitdactive") %>%
    select(estimate, std.error, conf.low, conf.high) %>%
    mutate(vitd_delta = d)
  
  delta_results_cont_vitd <- bind_rows(delta_results_cont_vitd, est_cont)
}

delta_results_cont_vitd$treatment<-"Vitamin D"
delta_results_cont_vitd_plot <- ggplot(delta_results_cont_vitd, aes(x = estimate, y = vitd_delta)) +
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


#continuous fishoil


inlist <- c("fishoilactive", "vitdactive", "pain", "pain_base", "time_contin", "Subject_ID")
pred_cont <- quickpred(vital_wide, minpuc = 0.5, include = inlist)
imp.all.undamped_cont <- vector("list", length(fishoil_delta))
delta_results_cont_fishoil <- data.frame()

for (i in seq_along(fishoil_delta)) {
  d <- fishoil_delta[i]
  imp_init <- mice(vital_wide, m = 5, maxit = 1, predictorMatrix = pred_cont, seed = 100 + i, print = FALSE)
  post_cont <- imp_init$post
  method_cont <- imp_init$method
  years <- paste0("pain_yr", 1:4)
  method_cont[years] <- "pmm"
  method_cont["pain_base"] <- "pmm"
  for (yr in years) {
    post_cont[yr] <- paste0(
      "idx <- which(data[,'fishoilactive'] == 1 & data[,'vitdactive'] == 0 & is.na(data[,'", yr, "'])); ",
      "imp[[j]]$", yr, "[idx] <- imp[[j]]$", yr, "[idx] + ", d, ";"
    )
  }
  imp_wide <- mice(vital_wide, m = 5, maxit = 10, predictorMatrix = pred_cont,
                   post = post_cont, method=method_cont, seed = 200 + i, print = FALSE)
  imp.all.undamped_cont[[i]] <- imp_wide 
  imp_data_long <- complete(imp_wide, action = "long", include = TRUE) %>%
    to_long_format_vital_mice_SA()
  imp_data_list <- split(imp_data_long, imp_data_long$.imp)
  
  fit_list <- lapply(imp_data_list, function(df) {
    lmer(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base + 
           (1 | Subject_ID), data = df)
  })
  pooled_cont <- pool(fit_list)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "fishoilactive") %>%
    select(estimate, std.error, conf.low, conf.high) %>%
    mutate(fishoil_delta = d)
  
  delta_results_cont_fishoil <- bind_rows(delta_results_cont_fishoil, est_cont)
}

delta_results_cont_fishoil$treatment<-"Fish Oil"
delta_results_cont_fishoil_plot <- ggplot(delta_results_cont_fishoil, aes(x = estimate, y = fishoil_delta)) +
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

