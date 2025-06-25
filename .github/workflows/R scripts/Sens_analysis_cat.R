
#standard deviations vital
vital_fish<-vital_wide%>%
  filter(fishoilactive==1 & vitdactive==0)

vital_vitd<-vital_wide%>%
  filter(fishoilactive==0 & vitdactive==1)

vital_control<-vital_wide%>%
  filter(fishoilactive==0 & vitdactive==0)

vital_fish_SD<-sd(vital_fish$pain_yr4, na.rm = TRUE)
vital_vitd_SD<-sd(vital_vitd$pain_yr4, na.rm = TRUE)
vital_control_SD<-sd(vital_control$pain_yr4, na.rm = TRUE)

#standard deviations in both groups acupuncture trial
acu<-acu_wide%>%
  filter(group==1)
acu_SD<-sd(acu$pk5, na.rm=TRUE)

acu_control<-acu_wide%>%
  filter(group==0)
acu_control_SD<-sd(acu_control$pk5, na.rm=TRUE)

#delta acu
acu_delta<-c(-0.5*acu_SD,-0.25*acu_SD,0,0.25*acu_SD,0.5*acu_SD)
acu_control_delta<-c(-0.5*acu_control_SD,-0.25*acu_control_SD,0,0.25*acu_control_SD,0.5*acu_control_SD)

#delta vital
#fishoil
fishoil_delta<-c(-0.5*vital_fish_SD,-0.25*vital_fish_SD,0,0.25*vital_fish_SD,0.5*vital_fish_SD)
vitd_delta<-c(-0.5*vital_vitd_SD,-0.25*vital_vitd_SD,0,0.25*vital_vitd_SD,0.5*vital_vitd_SD)
vital_control_delta<-c(-0.5*vital_control_SD,-0.25*vital_control_SD,0,0.25*vital_control_SD,0.5*vital_control_SD)



# Acupuncture 
# Acupuncture categorical treatment
inlist <- c("group", "pk5", "pk1")
pred_cat <- quickpred(acu_wide, minpuc = 0.5, include = inlist)
imp.default_cat <- mice(acu_wide, m = 1, maxit = 1, predictorMatrix = pred_cat, seed = 123, print= FALSE)
post_cat <- imp.default_cat$post
imp.all.undamped_cat <- vector("list", length(acu_delta))

for (i in 1:length(acu_delta)) {
  d <- acu_delta[i]
  cmd <- paste0(
    "idx <- which(data[,'group'] == 1 & is.na(data[,'pk5'])); ",
    "imp[[j]]$pk5[idx] <- imp[[j]]$pk5[idx] + ",d, ";")
  post_cat["pk5"] <- cmd
  imp_cat <- mice(acu_wide, pred = pred_cat, post = post_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat[[i]] <- imp_cat
}
delta_results_cat <- data.frame()

for (i in seq_along(imp.all.undamped_cat)) {
  imp_cat <- imp.all.undamped_cat[[i]]
  d <- acu_delta[i]
  fit_cat <- with(imp_cat, lm(pk5 ~ group + pk1))
  pooled_cat <- pool(fit_cat)
  est_cat <- tidy(pooled_cat, conf.int = TRUE) %>%
    filter(term == "group") %>% 
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(acu_delta = d)
    
  delta_results_cat <- bind_rows(delta_results_cat, est_cat)
}

delta_results_cat$treatment<-"Acupuncture"
delta_result_cat_plot <- ggplot(delta_results_cat, aes(x = estimate, y = acu_delta)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) +
  labs(
    title = "Treatment effect with δ-Adjustment (categorical)",
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


# Acupuncture categorical placebo
inlist <- c("group", "pk1", "pk5")
acu_wide_placebo<-acu_wide%>%filter(group==0)
pred_cat_placebo <- quickpred(acu_wide_placebo, minpuc = 0.5, include = inlist)
imp.default_cat <- mice(acu_wide_placebo, m = 1, maxit = 1, predictorMatrix = pred_cat_placebo, seed = 123, print= FALSE)
post_cat <- imp.default_cat$post
imp.all.undamped_cat_placebo <- vector("list", length(acu_control_delta))

for (i in 1:length(acu_control_delta)) {
  d <- acu_control_delta[i]
  cmd <- paste0(
    "idx <- which(is.na(data[,'pk5'])); ",
    "imp[[j]]$pk5[idx] <- imp[[j]]$pk5[idx] + ", d
  )
  post_cat["pk5"] <- cmd
  imp_cat <- mice(acu_wide_placebo, pred = pred_cat_placebo, post = post_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat_placebo[[i]] <- imp_cat
}
delta_results_cat_placebo <- data.frame()

for (i in seq_along(imp.all.undamped_cat_placebo)) {
  imp_cat <- imp.all.undamped_cat_placebo[[i]]
  d <- acu_control_delta[i]
  fit_cat <- with(imp_cat, lm(pk5 ~ pk1))
  pooled_cat <- pool(fit_cat)
  mean_pk1 <- mean(complete(imp_cat, action = 1)$pk1, na.rm = TRUE)
  pooled_summary <- summary(pooled_cat, conf.int = TRUE)
  
  intercept_est <- pooled_summary$estimate[pooled_summary$term == "(Intercept)"]
  slope_est <- pooled_summary$estimate[pooled_summary$term == "pk1"]
  
  var_int <- pooled_summary$std.error[pooled_summary$term == "(Intercept)"]^2
  var_slope <- pooled_summary$std.error[pooled_summary$term == "pk1"]^2
  cov_int_slope <- 0  
  
  est_val <- intercept_est + slope_est * mean_pk1
  var_val <- var_int + (mean_pk1^2) * var_slope + 2 * mean_pk1 * cov_int_slope
  se_val <- sqrt(var_val)
  
  est_cat <- data.frame(
    estimate = est_val,
    std.error = se_val,
    conf.low = est_val - 1.96 * se_val,
    conf.high = est_val + 1.96 * se_val,
    acu_control_delta = d
  )
  delta_results_cat_placebo <- bind_rows(delta_results_cat_placebo, est_cat)
}

delta_results_cat_placebo$treatment<-"Placebo"
delta_result_cat_placebo_plot <- ggplot(delta_results_cat_placebo, aes(x = estimate, y=acu_control_delta)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) +
  labs(
    title = "Placebo with δ-Adjustment (categorical)",
    x = "Mean headache severity at 12 months",
    y = "Delta"
  ) +
  theme_minimal()+ 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )




#categorical fishoil
inlist <- c("pain_base", "vitdactive", "fishoilactive", "pain_yr4")
pred_cat <- quickpred(vital_wide, minpuc = 0.5, include = inlist)
imp.all.undamped_cat <- vector("list", length(fishoil_delta))

for (i in 1:length(fishoil_delta)) {
  d <- fishoil_delta[i]
  imp_init <- mice(vital_wide, pred = pred_cat, maxit = 1, print = FALSE)
  method_cat <- imp_init$method
  method_cat["pain_base"] <- "pmm" 
  post_cat <- rep("", ncol(vital_wide))
  names(post_cat) <- names(vital_wide)
  post_cat["pain_yr4"]  <- paste0(
    "idx <- which(data[,'vitdactive'] == 0 & data[,'fishoilactive'] == 1 & is.na(data[,'pain_yr4'])); ",
    "imp[[j]]$pain_yr4[idx] <- imp[[j]]$pain_yr4[idx] + ", d, ";"
    )
  imp_cat <- mice(vital_wide, pred = pred_cat, post = post_cat, method=method_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat[[i]] <- imp_cat
}
delta_results_cat_fishoil <- data.frame()

for (i in seq_along(imp.all.undamped_cat)) {
  imp_cat <- imp.all.undamped_cat[[i]]
  d <- fishoil_delta[i]
  fit_cat <- with(imp_cat, lm(pain_yr4 ~ fishoilactive + vitdactive  + pain_base))
  pooled_cat<- pool(fit_cat)
  est_cat <- tidy(pooled_cat, conf.int = TRUE) %>%
    filter(term == "fishoilactive") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(fishoil_delta = d)
  delta_results_cat_fishoil <- bind_rows(delta_results_cat_fishoil, est_cat)
}

delta_results_cat_fishoil$treatment<-"Fish Oil"
delta_results_cat_fishoil_plot <- ggplot(delta_results_cat_fishoil, aes(x = estimate, y = fishoil_delta)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) + 
  labs(
    title = "Treatment effect with δ-Adjustment (categorical)",
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



#categorical vitamin D
inlist <- c("pain_base", "vitdactive", "fishoilactive", "pain_yr4")
pred_cat <- quickpred(vital_wide, minpuc = 0.5, include = inlist)
imp.all.undamped_cat <- vector("list", length(vitd_delta))

for (i in 1:length(vitd_delta)) {
  d <- vitd_delta[i]
  imp_init <- mice(vital_wide, pred = pred_cat, maxit = 1, print = FALSE)
  method_cat <- imp_init$method
  method_cat["pain_base"] <- "pmm" 
  post_cat <- rep("", ncol(vital_wide))
  names(post_cat) <- names(vital_wide)
  post_cat["pain_yr4"]  <- paste0(
    "idx <- which(data[,'vitdactive'] == 1 & data[,'fishoilactive'] == 0 & is.na(data[,'pain_yr4'])); ",
    "imp[[j]]$pain_yr4[idx] <- imp[[j]]$pain_yr4[idx] + ", d, ";"
    )
  imp_cat <- mice(vital_wide, pred = pred_cat, post = post_cat, method=method_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat[[i]] <- imp_cat
}

delta_results_cat_vitd <- data.frame()
for (i in seq_along(imp.all.undamped_cat)) {
  imp_cat <- imp.all.undamped_cat[[i]]
  d <- vitd_delta[i]
  fit_cat <- with(imp_cat, lm(pain_yr4 ~ fishoilactive + vitdactive  + pain_base))
  pooled_cat<- pool(fit_cat)
  est_cat <- tidy(pooled_cat, conf.int = TRUE) %>%
    filter(term == "vitdactive") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(vitd_delta = d)
  
  delta_results_cat_vitd <- bind_rows(delta_results_cat_vitd, est_cat)
}

delta_results_cat_vitd$treatment<-"Vitamin D"
delta_results_cat_vitd_plot <- ggplot(delta_results_cat_vitd, aes(x = estimate, y = vitd_delta)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) + 
  labs(
    title = "Treatment effect of Vit D with δ-Adjustment (categorical)",
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


#vital categorical control 
vital_wide_placebo <- vital_wide %>% filter(fishoilactive == 0, vitdactive == 0)
inlist <- c("pain_base", "vitdactive", "fishoilactive", "pain_yr4")
pred_cat_placebo <- quickpred(vital_wide_placebo, minpuc = 0.5, include = inlist)
imp.all.undamped_cat <- vector("list", length(vital_control_delta))

for (i in 1:length(vital_control_delta)) {
  d <- vital_control_delta[i]
  imp_init <- mice(vital_wide_placebo , pred = pred_cat_placebo, maxit = 1, print = FALSE)
  method_cat <- imp_init$method
  method_cat["pain_base"] <- "pmm" 
  post_cat <- rep("", ncol(vital_wide_placebo))
  names(post_cat) <- names(vital_wide_placebo)
  post_cat["pain_yr4"]  <- paste0(
    "idx <- which(is.na(data[,'pain_yr4'])); ",
    "imp[[j]]$pain_yr4[idx] <- imp[[j]]$pain_yr4[idx] + ", d)
  imp_cat <- mice(vital_wide_placebo, pred = pred_cat_placebo, method = method_cat, post = post_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat[[i]] <- imp_cat
}

delta_results_cat_vital_placebo <- data.frame()
for (i in seq_along(imp.all.undamped_cat)) {
  imp_cat <- imp.all.undamped_cat[[i]]
  d <- vital_control_delta[i]
  fit_cat <- with(imp_cat, lm(pain_yr4 ~ pain_base))
  pooled_cat<- pool(fit_cat)
  pooled_summary <- summary(pooled_cat, conf.int = TRUE)
  intercept_est <- pooled_summary$estimate[pooled_summary$term == "(Intercept)"]
  slope_est <- pooled_summary$estimate[pooled_summary$term == "pain_base"]
  var_int <- pooled_summary$std.error[pooled_summary$term == "(Intercept)"]^2
  var_slope <- pooled_summary$std.error[pooled_summary$term == "pain_base"]^2
  cov_int_slope <- 0  
  mean_pain_base <- mean(complete(imp_cat, action = 1)$pain_base, na.rm = TRUE)
  est_val <- intercept_est + slope_est * mean_pain_base
  var_val <- var_int + (mean_pain_base^2) * var_slope + 2 * mean_pain_base * cov_int_slope
  se_val <- sqrt(var_val)
  est_cat <- data.frame(
    estimate = est_val,
    std.error = se_val,
    conf.low = est_val - 1.96 * se_val,
    conf.high = est_val + 1.96 * se_val,
    vital_control_delta = d
  )
  delta_results_cat_vital_placebo <- bind_rows(delta_results_cat_vital_placebo, est_cat)
}

delta_results_cat_vital_placebo$treatment<-"Control"
delta_results_cat_vital_placebo_plot <- ggplot(delta_results_cat_vital_placebo, aes(x = estimate, y = vital_control_delta)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) + 
  labs(
    title = "Conditional expectation of pain_yr4",
    x = "Outcome",
    y = "Delta"
  ) +
  theme_minimal()+ 
  theme(
    strip.background = element_rect(fill = "lawngreen", color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )







