# FOR DATA TRANSFORMATION
priceTransform <- function(x) {
  if (!grepl("-",x)) return(x)
  values <- strsplit(x, "-")[[1]]
  values <- as.numeric(values)
  avg <- (values[1] + values[2]) / 2
  return(as.character(avg))
}

freqTransform <- function(x) {
  if (!grepl("MHz",x)) return(x)
  x = gsub("MHz","",x)
  x = as.numeric(x)
  x = x * 0.001
  x = as.character(x)
  return(x)
}

# FOR INMPUTATION

# FOR TESTING
plotCompare <- function(df1 , df2, col)
{
  plot(density(df1,na.rm = TRUE), main = paste("Density plot of ", col))
  lines(density(df2,na.rm = TRUE),col = "red", lty = 3)
}

plotTest<- function(df1, df2, col)
{
  par(mfrow = c(2, 2))
  comp <- sapply(col, function(c) {
    plotCompare(df1[[c]], df2[[c]], c)
  })
}

# FOR VISUALIZATION