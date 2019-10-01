getLimitedData <- function(limit = 5000){
  newUrl = paste0(baseUrl,"$limit=",limit)
  data <- fromJSON(newUrl)
  
  data <- cleanData(data)
  
  data <- data[,c("crimeType","latitude","longitude","date","victims")]
  data <- data[!is.na(data$date),]
  return(data)
}
getLimitedData(500)