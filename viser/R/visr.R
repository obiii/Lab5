
#' Base Url to get crime data
#' @export
baseUrl =  paste0("https://data.montgomerycountymd.gov/resource/yc8a-5df8.json?")
'%ni%' <- Negate('%in%')

#' Function to clean the data
#' @param jsondf as input dataframe
cleanData <- function(jsondf){
  
  df <- as.data.frame(jsondf)
  garbCols <- c(colnames(df)[1:12],"end_date","state","start_date","nibrs_code","pra","incident_id","geolocation.type","geolocation.coordinates")
  #df <- df[,-c(1:12)]
  df <- df[,colnames(df)%ni%garbCols]
  
  df$time = format(as.POSIXct(strptime(df$date,"%Y-%m-%dT%H:%M:%S",tz="")) ,format = "%H:%M")
  df$date = format(as.POSIXct(strptime(df$date,"%Y-%m-%dT%H:%M:%S",tz="")) ,format = "%Y-%m-%d")
  
  return(df)
}

#' Get data limted by number of observations
#' @param limit to limit the records
#' @export
getLimitedData <- function(limit = 5000){

 
  
  newUrl = paste0(baseUrl,"$limit=",limit)
  data <- fromJSON(url)
  
  data <- cleanData(data)
  
  return(data)
}



#x<-getLimitedData(limit=10)

