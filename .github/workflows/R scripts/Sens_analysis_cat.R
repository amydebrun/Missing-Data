# Acupuncture 
delta_acu <- c(-5,-2,0,2,5)
# Acupuncture categorical treatment
inlist <- c("group", "pk5", "pk1")
pred_cat <- quickpred(acu_wide, minpuc = 0.5, include = inlist)
imp.default_cat <- mice(acu_wide, m = 1, maxit = 1, predictorMatrix = pred_cat, seed = 123, print= FALSE)
post_cat <- imp.default_cat$post
imp.all.undamped_cat <- vector("list", length(delta_acu))

for (i in 1:length(delta_acu)) {
  d <- delta_acu[i]
  cmd <- paste0(
    "idx <- which(data[,'group'] == 1 & is.na(data[,'pk5'])); ",
    "imp[[j]]$pk5[idx] <- imp[[j]]$pk5[idx] + ",d)
  post_cat["pk5"] <- cmd
  imp_cat <- mice(acu_wide, pred = pred_cat, post = post_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat[[i]] <- imp_cat
}
delta_results_cat <- data.frame()

for (i in seq_along(imp.all.undamped_cat)) {
  imp_cat <- imp.all.undamped_cat[[i]]
  d <- delta_acu[i]
  fit_cat <- with(imp_cat, lm(pk5 ~ group + pk1))
  pooled_cat <- pool(fit_cat)
  est_cat <- tidy(pooled_cat, conf.int = TRUE) %>%
    filter(term == "group") %>% 
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta_acu = d)
    
  delta_results_cat <- bind_rows(delta_results_cat, est_cat)
}

delta_results_cat$treatment<-"Acupuncture"
delta_result_cat_plot <- ggplot(delta_results_cat, aes(x = estimate, y = delta_acu)) +
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
pred_cat_placebo <- quickpred(acu_wide_placebo, minpuc = 0.5, include = inlist)
imp.default_cat <- mice(acu_wide_placebo, m = 1, maxit = 1, predictorMatrix = pred_cat_placebo, seed = 123, print= FALSE)
post_cat <- imp.default_cat$post
imp.all.undamped_cat_placebo <- vector("list", length(delta_acu))

for (i in 1:length(delta_acu)) {
  d <- delta_acu[i]
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
  d <- delta_acu[i]
  fit_cat <- with(imp_cat, lm(pk5 ~ group + pk1))
  pooled_cat <- pool(fit_cat)
  est_cat <- tidy(pooled_cat, conf.int = TRUE) %>%
    filter(term == "(Intercept)") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta_acu = d)
  delta_results_cat_placebo <- bind_rows(delta_results_cat_placebo, est_cat)
}

delta_results_cat_placebo$treatment<-"Placebo"
delta_result_cat_placebo_plot <- ggplot(delta_results_cat_placebo, aes(x = estimate, y = delta_acu)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) +
  labs(
    title = "Placebo with δ-Adjustment (categorical)",
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




#categorical fishoil
inlist <- c("pain_base", "vitdactive", "fishoilactive", "pain_yr4")
pred_cat <- quickpred(vital_wide, minpuc = 0.5, include = inlist)
imp.all.undamped_cat <- vector("list", length(delta_vital))

for (i in 1:length(delta_vital)) {
  d <- delta_vital[i]
  post_cat <- rep("", ncol(vital_wide))
  names(post_cat) <- names(vital_wide)
  post_cat["pain_yr4"]  <- paste0(
    "idx <- which(data[,'vitdactive'] == 0 & data[,'fishoilactive'] == 1 & is.na(data[,'pain_yr4'])); ",
    "imp[[j]]$pain_yr4[idx] <- imp[[j]]$pain_yr4[idx] + ", d)
  imp_cat <- mice(vital_wide, pred = pred_cat, post = post_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat[[i]] <- imp_cat
}
delta_results_cat_fishoil <- data.frame()

for (i in seq_along(imp.all.undamped_cat)) {
  imp_cat <- imp.all.undamped_cat[[i]]
  d <- delta_vital[i]
  fit_cat <- with(imp_cat, lm(pain_yr4 ~ fishoilactive + vitdactive  + pain_base))
  pooled_cat<- pool(fit_cat)
  est_cat <- tidy(pooled_cat, conf.int = TRUE) %>%
    filter(term == "fishoilactive") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta_vital = d)
  delta_results_cat_fishoil <- bind_rows(delta_results_cat_fishoil, est_cat)
}

delta_results_cat_fishoil$treatment<-"Fish Oil"
delta_results_cat_fishoil_plot <- ggplot(delta_results_cat_fishoil, aes(x = estimate, y = delta_vital)) +
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
imp.all.undamped_cat <- vector("list", length(delta_vital))

for (i in 1:length(delta_vital)) {
  d <- delta_vital[i]
  post_cat <- rep("", ncol(vital_wide))
  names(post_cat) <- names(vital_wide)
  post_cat["pain_yr4"]  <- paste0(
    "idx <- which(data[,'vitdactive'] == 1 & data[,'fishoilactive'] == 0 & is.na(data[,'pain_yr4'])); ",
    "imp[[j]]$pain_yr4[idx] <- imp[[j]]$pain_yr4[idx] + ", d)
  imp_cat <- mice(vital_wide, pred = pred_cat, post = post_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat[[i]] <- imp_cat
}

delta_results_cat_vitd <- data.frame()
for (i in seq_along(imp.all.undamped_cat)) {
  imp_cat <- imp.all.undamped_cat[[i]]
  d <- delta_vital[i]
  fit_cat <- with(imp_cat, lm(pain_yr4 ~ fishoilactive + vitdactive  + pain_base))
  pooled_cat<- pool(fit_cat)
  est_cat <- tidy(pooled_cat, conf.int = TRUE) %>%
    filter(term == "vitdactive") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta_vital = d)
  
  delta_results_cat_vitd <- bind_rows(delta_results_cat_vitd, est_cat)
}

delta_results_cat_vitd$treatment<-"Vitamin D"
delta_results_cat_vitd_plot <- ggplot(delta_results_cat_vitd, aes(x = estimate, y = delta_vital)) +
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

inlist <- c("pain_base", "vitdactive", "fishoilactive", "pain_yr4")
pred_cat_placebo <- quickpred(vital_wide_placebo, minpuc = 0.5, include = inlist)
imp.all.undamped_cat <- vector("list", length(delta_vital))

for (i in 1:length(delta_vital)) {
  d <- delta_vital[i]
  post_cat <- rep("", ncol(vital_wide_placebo))
  names(post_cat) <- names(vital_wide_placebo)
  post_cat["pain_yr4"]  <- paste0(
    "idx <- which(is.na(data[,'pain_yr4'])); ",
    "imp[[j]]$pain_yr4[idx] <- imp[[j]]$pain_yr4[idx] + ", d)
  imp_cat <- mice(vital_wide_placebo, pred = pred_cat_placebo, post = post_cat, maxit = 10,
                  seed = i * 22, print=FALSE)
  imp.all.undamped_cat[[i]] <- imp_cat
}

delta_results_cat_vital_placebo <- data.frame()
for (i in seq_along(imp.all.undamped_cat)) {
  imp_cat <- imp.all.undamped_cat[[i]]
  d <- delta_vital[i]
  fit_cat <- with(imp_cat, lm(pain_yr4 ~ pain_base))
  pooled_cat<- pool(fit_cat)
  est_cat <- tidy(pooled_cat, conf.int = TRUE) %>%
    filter(term == "(Intercept)") %>%  
    select(estimate, std.error, conf.low, conf.high, p.value) %>%
    mutate(delta_vital = d)
  
  delta_results_cat_vital_placebo <- bind_rows(delta_results_cat_vital_placebo, est_cat)
}

delta_results_cat_vital_placebo$treatment<-"Control"
delta_results_cat_vital_placebo_plot <- ggplot(delta_results_cat_vital_placebo, aes(x = estimate, y = delta_vital)) +
  geom_point(size = 4, color = "#a80050",position = position_nudge(y = 0.15)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, position = position_nudge(y = 0.15)) +   
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +   
  facet_wrap(~ treatment) + 
  labs(
    title = "Control effect of Vital with δ-Adjustment (categorical)",
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







