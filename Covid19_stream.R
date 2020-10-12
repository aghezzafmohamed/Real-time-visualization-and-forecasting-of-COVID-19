library(httr)
library(jsonlite)
library(tibble)
library(lubridate)

Scripting_Cases <- function (LocationId) {
  request <- paste0(LocationId)
  text <- content(GET(request), as = "text", encoding = "UTF-8")
  tryCatch(data <- fromJSON(text), error=function(e) NULL)
  df <- as_data_frame(data$features$attributes)
  df$Date <- format(df$Date, scientific=FALSE)
  df$Date <- format(as_datetime(as.numeric(substr(df$Date, 1, nchar(df$Date)-3))), "%d/%m/%Y")
  df = subset(df, select = -c(OBJECTID,GlobalID) )
  #df[is.na(df)] <- 0
  #df[complete.cases(df), ]
  df[]<-lapply(df,replace_na_with_previous)
  
  return(df)
}


replace_na_with_previous<-function (vector) {
  if (is.na(vector[1])) 
    vector[1] <- na.omit(vector)[1]
  for (i in 1:length(vector)) {
    if ((i - 1) > 0) {
      if (is.na(vector[i])) 
        vector[i] <- vector[i - 1]
    }
  }
  return(vector)
}



