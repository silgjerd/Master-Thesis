

# Yearmon
ymon <- function(date) {
  return(paste0(year(date), ifelse(nchar(month(date))==1, paste0("0", month(date)), month(date))))
}

# Mode
getmode <- function(v) {
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  
  cat("MODE: ", uniqv[which.max(tabulate(match(v, uniqv)))],
      "(n=", length(uniqv), ")\n",sep="")
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

# Look
look <- function(data) {
  hist(data)
  #cat(summary(data))
  cat("NAs:", sum(is.na(data)))
}
