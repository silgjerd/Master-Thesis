

# Yearmon
ymon <- function(date) {
  return(paste0(year(date), ifelse(nchar(month(date))==1, paste0("0", month(date)), month(date))))
}

# Look
look <- function(data) {
  hist(data)
  #cat(summary(data))
  cat("NAs:", sum(is.na(data)))
}
