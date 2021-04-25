# Yearmon
ymon <- function(date) {
  return(paste0(year(date), ifelse(nchar(month(date))==1, paste0("0", month(date)), month(date))))
}

# Save plot
plotsave <- function(title, width=166, ratio=11/16){ #function to save plot
  height <- floor(width * ratio)
  ggsave(title,
         width = width,
         height = height,
         dpi = 500,
         units = "mm")}

# Mode
getmode <- function(v) {
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  
  cat("MODE: ", uniqv[which.max(tabulate(match(v, uniqv)))],
      "(n=", length(uniqv), ")\n",sep="")
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

# Range normalization train test
rangenorm <- function(df, x_vars, y_vars, split_index, range_min = 0, range_max = 1){
  
  train <- df[split_index, x_vars]
  cmin <- sapply(train, min)
  cmax <- sapply(train, max)
  
  df_norm <- df[, x_vars]
  
  for (i in 1:ncol(df_norm)){
    df_norm[,i] <- range_min + ((range_max - range_min) * (df_norm[,i] - cmin[i]) / (cmax[i] - cmin[i]))
    # df_norm[,i] <- (df_norm[,i] - cmin[i]) / (cmax[i] - cmin[i]) old
    }
  
  df_norm <- df_norm %>% bind_cols(df[, y_vars])
  
  return(df_norm)
}

# Model evaluation function
mevaluate <- function(pred, y_test){
  
  cmse <- mean((pred - y_test)^2)
  ccor <- cor(pred, y_test)[1]
  crsq <- ccor^2
  cscore <- ccor / cmse
  
  eval <- tibble("MSE" = cmse,
                 "COR" = ccor,
                 "RSQ" = crsq,
                 "SCORE" = cscore)
  
  return(eval)
}

# Evaluate rets by trading
tradeevaluate <- function(rets){
  
  # Plot
  plot(cumprod(1 + rets), type = "l")
  
  return(summary(rets))
}



