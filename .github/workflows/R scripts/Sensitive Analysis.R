# SENSITIVITY ANALYSIS : DELTA ADJUSTMENT 

# Acupuncture 

# Acupuncture categorical 
delta <- c(-5,-2,0,2,5)
inlist <- c("sex", "age", "pk1")
pred_cat <- quickpred(acu_wide, minpuc = 0.5, include = inlist)
imp.default_cat <- mice(acu_wide, m = 1, maxit = 1, predictorMatrix = pred_cat, seed = 123, print= FALSE)
post_cat <- imp.default_cat$post
imp.all.undamped_cat <- vector("list", length(delta))

for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste("imp[[j]][,i] <- imp[[j]][,i] +", d)
  post_cat["pk5"] <- cmd
  imp_cat <- mice(acu_wide, pred = pred_cat, post = post_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat[[i]] <- imp_cat
}
delta_results_cat <- data.frame()

for (i in seq_along(imp.all.undamped_cat)) {
  imp_cat <- imp.all.undamped_cat[[i]]
  d <- delta[i]
  fit_cat <- with(imp_cat, lm(pk5 ~ group + pk1))
  pooled_cat <- pool(fit_cat)
  est_cat <- tidy(pooled_cat, conf.int = TRUE) %>%
    filter(term == "group") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta = d)
  delta_results_cat <- bind_rows(delta_results_cat, est_cat)
}

delta_results_cat$group<-"Acupuncture Treatment"
delta_result_cat_plot <- ggplot(delta_results_cat, aes(x = estimate, y = delta)) +
  geom_point(size = 4, color = "#a80050") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ group) +
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

#continuous
delta <- c(-5,-2,0,2,5)
inlist <- c("sex", "age", "pk1")
pred_cont <- quickpred(acu_long, minpuc = 0.5, include = inlist)
imp.default_cont <- mice(acu_long, m = 1, maxit = 1, predictorMatrix = pred_cont, seed = 123, print= FALSE)
post_cont <- imp.default_cont$post
imp.all.undamped_cont <- vector("list", length(delta))


for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste("imp[[j]][,i] <- imp[[j]][,i] +", d)
  post_cont["pk_score"] <- cmd
  imp_cont <- mice(acu_long, pred = pred_cont, post = post_cont, maxit = 10,
                   seed = i * 22, print=FALSE)
  imp.all.undamped_cont[[i]] <- imp_cont
}

delta_results_cont_acu <- data.frame()
for (i in seq_along(imp.all.undamped_cont)) {
  imp_cont <- imp.all.undamped_cont[[i]]
  d <- delta[i]
  fit_cont <- with(imp_cont, lm(pk_score ~ group * time + pk1))
  pooled_cont <- pool(fit_cont)
  est_cont <- tidy(pooled_cont, conf.int = TRUE) %>%
    filter(term == "group") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta = d)
  
  delta_results_cont_acu  <- bind_rows(delta_results_cont_acu, est_cont)
}

delta_results_cont_acu$group<-"Acupuncture Treatment"
delta_results_cont_acu_plot <- ggplot(delta_results_cont_acu, aes(x = estimate, y = delta)) +
  geom_point(size = 4, color = "#a80050") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ group) +
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

#COMBINING THE TWO PLOTS TOGETHER 

delta_results_cat$tme<- "Categorical time"
delta_results_cont_acu$tme <- "Continuous time"
delta_combined_acu <- bind_rows(delta_results_cat, delta_results_cont_acu)
delta_combined_acu$group<-"Acupuncture"

delta_compare_acu <- ggplot(delta_combined_acu, aes(x = estimate, y = delta)) +
  geom_point(aes(color = tme, shape = tme), size = 4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = tme), height = 0.4) +
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



##VITAL FISHOIL

#continuous
delta <- c(-10,-5,-2,0,2,5,10)
inlist <- c("sex", "ageyr", "bmi")
pred_cont <- quickpred(vital_long, minpuc = 0.5, include = inlist)
imp.default_cont <- mice(vital_long, m = 1, maxit = 1, predictorMatrix = pred_cont, seed = 123, print= FALSE)
post_cont <- imp.default_cont$post
imp.all.undamped_cont <- vector("list", length(delta))

for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste("imp[[j]][,i] <- imp[[j]][,i] +", d)
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
  geom_point(size = 4, color = "#a80050") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +   
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

#categorical
pred_cat <- quickpred(vital_wide, minpuc = 0.5, include = inlist)
imp.default_cat <- mice(vital_wide, m = 1, maxit = 1, predictorMatrix = pred_cat, seed = 123, print= FALSE)
post_cat<- imp.default_cat$post
imp.all.undamped_cat <- vector("list", length(delta))

for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste("imp[[j]][,i] <- imp[[j]][,i] +", d)
  post_cat["pain_yr4"] <- cmd
  imp_cat <- mice(vital_wide, pred = pred_cat, post = post_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat[[i]] <- imp_cat
}
delta_results_cat_fishoil <- data.frame()

for (i in seq_along(imp.all.undamped_cat)) {
  imp_cat <- imp.all.undamped_cat[[i]]
  d <- delta[i]
  fit_cat <- with(imp_cat, lm(pain_yr4 ~ fishoilactive + vitdactive  + pain_base))
  pooled_cat<- pool(fit_cat)
  est_cat <- tidy(pooled_cat, conf.int = TRUE) %>%
    filter(term == "fishoilactive") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta = d)
  delta_results_cat_fishoil <- bind_rows(delta_results_cat_fishoil, est_cat)
}

delta_results_cat_fishoil$treatment<-"Fish Oil"
delta_results_cat_fishoil_plot <- ggplot(delta_results_cat_fishoil, aes(x = estimate, y = delta)) +
  geom_point(size = 4, color = "#a80050") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +   
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


# combine fishoil sensitivity analysis plots

delta_results_cat_fishoil$tme<-"Categorical Time"
delta_results_cont_fishoil$tme<- "Continuous Time"
delta_combined_fishoil<-bind_rows(delta_results_cat_fishoil, delta_results_cont_fishoil)
delta_combined_fishoil$treatment<- "Fish Oil"

delta_combined_fishoil_plot <- ggplot(delta_combined_fishoil, aes(x = estimate, y = delta, color = tme, shape = tme)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +   
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
inlist <- c("sex", "ageyr", "bmi")
pred_cont <- quickpred(vital_long, minpuc = 0.5, include = inlist)
imp.default_cont <- mice(vital_long, m = 1, maxit = 1, predictorMatrix = pred_cont, seed = 123, print= FALSE)
post_cont <- imp.default_cont$post
imp.all.undamped_cont <- vector("list", length(delta))


for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste("imp[[j]][,i] <- imp[[j]][,i] +", d)
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
  geom_point(size = 4, color = "#a80050") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +   
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


#categorical

for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste("imp[[j]][,i] <- imp[[j]][,i] +", d)
  post_cat["pain_yr4"] <- cmd
  imp_cat <- mice(vital_wide, pred = pred_cat, post = post_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat[[i]] <- imp_cat
}

delta_results_cat_vitd <- data.frame()
for (i in seq_along(imp.all.undamped_cat)) {
  imp_cat <- imp.all.undamped_cat[[i]]
  d <- delta[i]
  fit_cat <- with(imp_cat, lm(pain_yr4 ~ fishoilactive + vitdactive  + pain_base))
  pooled_cat<- pool(fit_cat)
  est_cat <- tidy(pooled_cat, conf.int = TRUE) %>%
    filter(term == "vitdactive") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta = d)
  
  delta_results_cat_vitd <- bind_rows(delta_results_cat_vitd, est_cat)
}

delta_results_cat_vitd$treatment<-"Vitamin D"
delta_results_cat_vitd_plot <- ggplot(delta_results_cat_vitd, aes(x = estimate, y = delta)) +
  geom_point(size = 4, color = "#a80050") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +   
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


#combine vitamin d plots 
delta_results_cat_vitd$tme<-"Categorical Time"
delta_results_cont_vitd$tme<- "Continuous Time"
delta_combined_vitd<-bind_rows(delta_results_cat_vitd, delta_results_cont_vitd)
delta_combined_vitd$treatment<-"Vitamin D"

delta_combined_vitd_plot <- ggplot(delta_combined_vitd, aes(x = estimate, y = delta, color = tme, shape = tme)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +   
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

delta_combined_vital_plot <- ggplot(delta_combined_vital, aes(x = estimate, y = delta, color = tme, shape = tme)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4) +   
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
