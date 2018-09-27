
#' Base Url to get crime data
#' @export
baseUrl =  paste0("https://data.montgomerycountymd.gov/resource/yc8a-5df8.json?")
'%ni%' <- Negate('%in%')

#' Data in hold
#' @export
dataset <- NULL

#' CrimeTypes
#' @export
allCrime <- c("JUVENILE", "DAMAGE", "SUDDEN", "LARCENY", "BURGLARY", "SEX", "MISSING", "DRIVING", "DRUGS", "LIQUOR", "MENTAL", "LOST", "FORGERY", "RECOVERED", "PUBLIC", "ASSAULT", "ROBBERY", "TRESPASSING", "FAMILY", "POLICE", "AUTO", "LITTERING", "OBSTRUCT", "SUICIDE", "FRAUD", "IDENTITY", "RAPE", "ATTEMPTED", "OVERDOSE", "WEAPON", "FUGITIVE", "ALL", "ABDUCT", "ESCAPE", "EXTORT", "COUNTERFEITING", "TRAFFIC", "SOLICITATION", "STOLEN", "COMM", "UNAUTHORIZED", "EXTORTION", "ARSON", "KIDNAP", "LOITERING", "FALSE", 
              "HUMAN", "FIRE", "CARRYING", "THREAT", "PROPERTY", "EMBEZZLE", "ANIMAL", "INCOME", "HIT", "OBSCENE", "DRUNKENNESS")

#' Function to clean the data
#' @param jsondf as input dataframe
cleanData <- function(jsondf){
  
  df <- as.data.frame(jsondf)
  garbCols <- c(colnames(df)[1:4],"geolocation.type","geolocation.coordinates","incident_id","nibrs_code","police_district_number","pra","sector","start_date","state","zip_code","end_date")
  df <- df[,colnames(df)%ni%garbCols]
  
  df$time = format(as.POSIXct(strptime(df$date,"%Y-%m-%dT%H:%M:%S",tz="")) ,format = "%H:%M")
  df$date = format(as.POSIXct(strptime(df$date,"%Y-%m-%dT%H:%M:%S",tz="")) ,format = "%Y-%m-%d")
  df <- df %>% separate(crimename3, c("crimeType", "Crime"))
  return(df)
}

#' Get data limted by number of observations
#' @param limit tos
#' limit the records
#' @export
getLimitedData <- function(limit = 5000){

 
  
  newUrl = paste0(baseUrl,"$limit=",limit)
  data <- fromJSON(newUrl)
  
  data <- cleanData(data)
  
  dataset <- data
}


#' Function to get data by crime type
#' @param crimeType a crime type
#' @export
getDataByCrimeType <- function(crimeType){
  
  if(crimeType == "All"){
    return(dataset[allCrime])
  }else{
    dataset <- dataset[,crimeType]
    return(dataset)
  }
}

#x<-getLimitedData(limit=5000)

