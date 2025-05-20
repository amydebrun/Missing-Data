# LOCF function
LOCF <- function(data, columns){
  for (col in columns) {
    for (i in 1:nrow(data)) {  
      if (is.na(data[i, col])) {
        data[i, col] <- data[i, col - 1]  
      }
    }
  }
  return(data)  
}



# MEAN IMPUTATION FUNCTION 
mean_impute <- function(data) {
  data <- data.frame(
    lapply(data, function(x) {
      if (is.numeric(x)) {
        return(ifelse(is.na(x), mean(x, na.rm = TRUE), x))
      } else {
        return(x)
      }
    })
  )
  return(data)
}



# Function to transform wide to long

# Acupuncture
# For category time
to_long_format_acu_cat <- function(data_wide) {
  data_wide %>%
    pivot_longer(
      cols = c('pk2', 'pk5'), 
      names_to = 'pk_time',
      values_to = 'pk_score'
    ) %>%
    pivot_longer(
      cols = c('f2', 'f5'), 
      names_to = 'freq_time',
      values_to = 'freq_score',
      names_repair = 'unique'
    ) %>%
    filter((pk_time == 'pk2' & freq_time == 'f2') | (pk_time == 'pk5' & freq_time == 'f5')) %>%
    mutate(
      time = case_when(
        pk_time == 'pk2' ~ "3m",
        pk_time == 'pk5' ~ "12m"
      )
    ) %>%
    select(-pk_time, -freq_time)
}
# For MICE category time
to_long_format_acu_cat_MICE <- function(data_wide) {
  data_wide %>%
    pivot_longer(
      cols = c('pk2', 'pk5'), 
      names_to = 'pk_time',
      values_to = 'pk_score'
    ) %>%
    pivot_longer(
      cols = c('f2', 'f5'), 
      names_to = 'freq_time',
      values_to = 'freq_score',
      names_repair = 'unique'
    ) %>%
    filter((pk_time == 'pk2' & freq_time == 'f2') | (pk_time == 'pk5' & freq_time == 'f5')) %>%
    mutate(
      time = case_when(
        pk_time == 'pk2' ~ "3m",
        pk_time == 'pk5' ~ "12m"
      )
    ) %>%
    select(-pk_time, -freq_time) %>%
    group_by(.imp) %>%
    mutate(.id = row_number())
}
# For continuous time
to_long_format_acu_cont <- function(data_wide) {
  data_wide %>%
    pivot_longer(
      cols = c('pk2', 'pk5'), 
      names_to = 'pk_time',
      values_to = 'pk_score'
    ) %>%
    pivot_longer(
      cols = c('f2', 'f5'), 
      names_to = 'freq_time',
      values_to = 'freq_score',
      names_repair = 'unique'
    ) %>%
    filter((pk_time == "pk2" & freq_time == "f2") | (pk_time == "pk5" & freq_time == "f5")) %>%
    mutate(
      time = case_when(
        pk_time == "pk2" ~ 3,
        pk_time == "pk5" ~ 12
      )
    ) %>%
    select(-c(pk_time, freq_time))
}
# For MICE continuous time
to_long_format_acu_cont_MICE <- function(data_wide) {
  data_wide %>%
    pivot_longer(
      cols = c('pk2', 'pk5'), 
      names_to = 'pk_time',
      values_to = 'pk_score'
    ) %>%
    pivot_longer(
      cols = c('f2', 'f5'), 
      names_to = 'freq_time',
      values_to = 'freq_score',
      names_repair = 'unique'
    ) %>%
    filter((pk_time == 'pk2' & freq_time == 'f2') | (pk_time == 'pk5' & freq_time == 'f5')) %>%
    mutate(
      time = case_when(
        pk_time == 'pk2' ~ 3,
        pk_time == 'pk5' ~ 12
      )
    ) %>%
    select(-pk_time, -freq_time) %>%
    group_by(.imp) %>%
    mutate(.id = row_number()) 
}

# VITAL transfer to long
to_long_format_vital <- function(data_wide) {
  data_wide %>%
    pivot_longer(cols = matches("_yr[[:digit:]]$"),
                 names_to = c(".value", "time"), 
                 names_sep = "_") %>%
    mutate(time_contin = as.integer(gsub("yr", "", time)),
           time_contin_cent = time_contin - 4)
}
# VITAL transfer to long for MICE
to_long_format_vital_mice <- function(data_wide) {
  vital_mice_data %>%
    pivot_longer(cols = matches("_yr[[:digit:]]$"),
                 names_to = c(".value", "time"), 
                 names_sep = "_") %>%
    group_by(.imp) %>%
    mutate(.id = 1:n()) %>%
    mutate(time_contin = as.integer(gsub("yr", "", time)),
           time_contin_cent = time_contin - 4)
}

#LRT function
lrt_from_glm <- function(model) {
  null_dev <- model$null.deviance
  resid_dev <- model$deviance
  df <- model$df.null - model$df.residual
  
  chisq_stat <- null_dev - resid_dev
  p_value <- pchisq(chisq_stat, df, lower.tail = FALSE)
  
  
  print(round(p_value, digits=3))
}
