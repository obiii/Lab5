
#' Base Url to get crime data
#' @param baseUrl contains the base URL for API call
#' @export
baseUrl =  paste0("https://data.montgomerycountymd.gov/resource/yc8a-5df8.json?")
'%ni%' <- Negate('%in%')


#' CrimeTypes
#' @param allCrime contains crime categories
#' @export
allCrime <- c("All","JUVENILE", "DAMAGE", "SUDDEN", "LARCENY", "BURGLARY", "SEX", "MISSING", "DRIVING", "DRUGS", "LIQUOR", "MENTAL", "LOST", "FORGERY", "RECOVERED", "PUBLIC", "ASSAULT", "ROBBERY", "TRESPASSING", "FAMILY", "POLICE", "AUTO", "LITTERING", "OBSTRUCT", "SUICIDE", "FRAUD", "IDENTITY", "RAPE", "ATTEMPTED", "OVERDOSE", "WEAPON", "FUGITIVE", "ABDUCT", "ESCAPE", "EXTORT", "COUNTERFEITING", "TRAFFIC", "SOLICITATION", "STOLEN", "COMM", "UNAUTHORIZED", "EXTORTION", "ARSON", "KIDNAP", "LOITERING", "FALSE", 
              "HUMAN", "FIRE", "CARRYING", "THREAT", "PROPERTY", "EMBEZZLE", "INCOME", "HIT", "OBSCENE", "DRUNKENNESS")

#' Function to clean the data
#' @param jsondf as input dataframe
cleanData <- function(jsondf){
  require(tidyr)
  require(dplyr)
  df <- as.data.frame(jsondf)
  garbCols <- c(colnames(df)[1:4],"geolocation.type","geolocation.coordinates","incident_id","nibrs_code","police_district_number","pra","sector","start_date","state","zip_code","end_date")
  df <- df[,colnames(df)%ni%garbCols]
  
  df$time = format(as.POSIXct(strptime(df$date,"%Y-%m-%dT%H:%M:%S",tz="")) ,format = "%H:%M")
  df$date = format(as.POSIXct(strptime(df$date,"%Y-%m-%dT%H:%M:%S",tz="")) ,format = "%Y-%m-%d")
  df <- separate(df,crimename3, c("crimeType", "Crime"))
  
  df$latitude <- as.numeric(df$latitude)
  df$longitude <- as.numeric(df$longitude)
  return(df)
}

#' Get data limted by number of observations
#' @param limit to limit records
#' limit the records
#' @return dataset containing limited rows with limit as \code{limit}
#'
#' @examples
#' getLimitedData(limit = 5000)
#' 
#' @export
getLimitedData <- function(limit = 5000){
  require(jsonlite)
  newUrl = paste0(baseUrl,"$limit=",limit)
  data <- fromJSON(newUrl)
  
  data <- cleanData(data)
  
  data <- data[,c("crimeType","latitude","longitude","date","victims")]
  data <- data[!is.na(data$date),]
  return(data)
}


#' Function to get data by crime type
#' 
#' @param dataset a dataset
#' @param crimeType a crime type
#'
#' @return dataset for a specific crime given by \code{crimeType}
#' 
#' @export
getDataByCrimeType <- function(dataset,crimeType){
  
  crimetype = allCrime[crimeType]
  return(dataset[dataset$crimeType==crimetype, c("crimeType","latitude","longitude","date","victims") ])

}



#' Function to map crime name with Id
#' @param id crime ID associated with crime
#' 
#' @examples
#' getCrimeByID(2)
#'  
#' @export
getCrimeByID <- function(id){
  return(allCrime[id])
}



