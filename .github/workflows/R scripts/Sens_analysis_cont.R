# SENSITIVITY ANALYSIS : DELTA ADJUSTMENT 


#continuous acupuncutre
delta <- c(-5,-2,0,2,5)
inlist <- c("group", "pk1")
pred_cont <- quickpred(acu_long_cont, minpuc = 0.5, include = inlist)
imp.default_cont <- mice(acu_long_cont, m = 1, maxit = 1, predictorMatrix = pred_cont, seed = 123, print= FALSE)
post_cont <- imp.default_cont$post
imp.all.undamped_cont <- vector("list", length(delta))


for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste0(
    "idx <- which(data[,'group'] == 1 & is.na(data[,'pk_score'])); ",
    "imp[[j]]$pk_score[idx] <- imp[[j]]$pk_score[idx] + ", d
  )
  post_cont["pk_score"] <- cmd
  imp_cont <- mice(acu_long_cont, pred = pred_cont, post = post_cont, maxit = 10,
                   seed = i * 22, print=FALSE)
  imp.all.undamped_cont[[i]] <- imp_cont
}

delta_results_cont_acu <- data.frame()
for (i in seq_along(imp.all.undamped_cont)) {
  imp_cont <- imp.all.undamped_cont[[i]]
  d <- delta[i]
  fit_cont <- with(imp_cont,  lmer(pk_score ~ group*(time_c) + pk1 + (1|id)))
  pooled_cont <- pool(fit_cont)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "group") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta = d)
  
  delta_results_cont_acu  <- bind_rows(delta_results_cont_acu, est_cont)
}

delta_results_cont_acu$treatment<-"Acupuncture Treatment"
delta_results_cont_acu_plot <- ggplot(delta_results_cont_acu, aes(x = estimate, y = delta)) +
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
delta <- c(-5,-2,0,2,5)
inlist <- c("group", "pk1")
pred_cont <- quickpred(acu_long_cont, minpuc = 0.5, include = inlist)
imp.default_cont <- mice(acu_long_cont, m = 1, maxit = 1, predictorMatrix = pred_cont, seed = 123, print= FALSE)
post_cont <- imp.default_cont$post
imp.all.undamped_cont_placebo <- vector("list", length(delta))


for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste0(
    "idx <- which(data[,'group'] == 0 & is.na(data[,'pk_score'])); ",
    "imp[[j]]$pk_score[idx] <- imp[[j]]$pk_score[idx] + ", d
  )
  post_cont["pk_score"] <- cmd
  imp_cont <- mice(acu_long_cont, pred = pred_cont, post = post_cont, maxit = 10,
                   seed = i * 22, print=FALSE)
  imp.all.undamped_cont_placebo[[i]] <- imp_cont
}

delta_results_cont_acu_placebo <- data.frame()
for (i in seq_along(imp.all.undamped_cont_placebo)) {
  imp_cont <- imp.all.undamped_cont_placebo[[i]]
  d <- delta[i]
  fit_cont <- with(imp_cont, lmer(pk_score ~ group*(time_c) + pk1 + (1|id)))
  pooled_cont <- pool(fit_cont)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "group") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta = d)
  
  delta_results_cont_acu_placebo <- bind_rows(delta_results_cont_acu_placebo, est_cont)
}

delta_results_cont_acu_placebo$group<-"Placebo"
delta_results_cont_acu_placebo_plot <- ggplot(delta_results_cont_acu_placebo, aes(x = estimate, y = delta)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ group) +
  labs(
    title = "Placebo with δ-Adjustment (continuous)",
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
delta <- c(-10,-5,-2,0,2,5,10)
inlist <- c("sex", "ageyr", "bmi")
pred_cont <- quickpred(vital_long, minpuc = 0.5, include = inlist)
imp.default_cont <- mice(vital_long, m = 1, maxit = 1, predictorMatrix = pred_cont, seed = 123, print= FALSE)
post_cont <- imp.default_cont$post
imp.all.undamped_cont <- vector("list", length(delta))

for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste0(
    "idx <- which(is.na(data[,'pain'])); ",
    "imp[[j]]$pain[idx] <- imp[[j]]$pain[idx] + ", d
  )
  post_cont["pain"] <- cmd
  imp_cont <- mice(vital_long, pred = pred_cont, post = post_cont, maxit = 10,
                   seed = i * 22, print=FALSE)
  imp.all.undamped_cont[[i]] <- imp_cont
}
delta_results_cont_fishoil <- data.frame()

for (i in seq_along(imp.all.undamped_cont)) {
  imp_cont <- imp.all.undamped_cont[[i]]
  d <- delta[i]
  fit_cont <- with(imp_cont, lm(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base))
  pooled_cont <- pool(fit_cont)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "fishoilactive") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta = d)
  delta_results_cont_fishoil <- bind_rows(delta_results_cont_fishoil, est_cont)
}

delta_results_cont_fishoil$treatment<-"Fish Oil"
delta_results_cont_fishoil_plot <- ggplot(delta_results_cont_fishoil, aes(x = estimate, y = delta)) +
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
delta <- c(-10,-5,-2,0,2,5,10)
inlist <- c("pain_base","time_contin", "vitdactive", "fishoilactive")
vital_long$placebo<- ifelse(vital_long$fishoilactive == 0 & vital_long$vitdactive == 0, 1, 0)
pred_cont <- quickpred(vital_long, minpuc = 0.5, include = inlist)
imp.default_cont <- mice(vital_long, m = 1, maxit = 1, predictorMatrix = pred_cont, seed = 123, print= FALSE)
post_cont <- imp.default_cont$post
imp.all.undamped_cont <- vector("list", length(delta))


for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste0(
    "idx <- which(is.na(data[,'pain'])); ",
    "imp[[j]]$pain[idx] <- imp[[j]]$pain[idx] + ", d
  )
  post_cont["pain"] <- cmd
  imp_cont <- mice(vital_long, pred = pred_cont, post = post_cont, maxit = 10,
                   seed = i * 22, print=FALSE)
  imp.all.undamped_cont[[i]] <- imp_cont
}
delta_results_cont_placebo <- data.frame()

for (i in seq_along(imp.all.undamped_cont)) {
  imp_cont <- imp.all.undamped_cont[[i]]
  d <- delta[i]
  fit_cont <- with(imp_cont, lm(pain ~ placebo*time_contin +vitdactive*time_contin + fishoilactive*time_contin  + pain_base))
  pooled_cont <- pool(fit_cont)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "placebo") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta = d)
  delta_results_cont_placebo<- bind_rows(delta_results_cont_placebo, est_cont)
}

delta_results_cont_placebo$treatment<-"Placebo"
delta_results_cont_placebo_plot <- ggplot(delta_results_cont_placebo, aes(x = estimate, y = delta)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) + 
  labs(
    title = "Placebo with δ-Adjustment (continuous)",
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

##VITAL VITAMIN D

#continuous
delta <- c(-10,-5,-2,0,2,5,10)
inlist <- c("pain_base","time_contin", "vitdactive", "fishoilactive")
pred_cont <- quickpred(vital_long, minpuc = 0.5, include = inlist)
imp.default_cont <- mice(vital_long, m = 1, maxit = 1, predictorMatrix = pred_cont, seed = 123, print= FALSE)
post_cont <- imp.default_cont$post
imp.all.undamped_cont <- vector("list", length(delta))


for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste0(
    "idx <- which(is.na(data[,'pain'])); ",
    "imp[[j]]$pain[idx] <- imp[[j]]$pain[idx] + ", d
  )
  post_cont["pain"] <- cmd
  imp_cont <- mice(vital_long, pred = pred_cont, post = post_cont, maxit = 10,
                   seed = i * 22, print=FALSE)
  imp.all.undamped_cont[[i]] <- imp_cont
}

delta_results_cont_vitd <- data.frame()
for (i in seq_along(imp.all.undamped_cont)) {
  imp_cont <- imp.all.undamped_cont[[i]]
  d <- delta[i]
  fit_cont <- with(imp_cont, lm(pain ~ fishoilactive*time_contin + vitdactive*time_contin + pain_base))
  pooled_cont <- pool(fit_cont)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "vitdactive") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta = d)
  
  delta_results_cont_vitd <- bind_rows(delta_results_cont_vitd, est_cont)
}

delta_results_cont_vitd$treatment<-"Vitamin D"
delta_results_cont_vitd_plot <- ggplot(delta_results_cont_vitd, aes(x = estimate, y = delta)) +
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


