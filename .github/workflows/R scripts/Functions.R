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