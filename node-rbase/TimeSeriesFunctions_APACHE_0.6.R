library(mongolite)
library(prophet)

stringMethods <- c('PROPHET')
directoryToSaveModels <- "/home/ruser/forecastModels"
directoryToSaveForecasts <- "/home/ruser/forecastsCache"
forecastsCacheLength <- 14
conn <- NULL
conn_url <- NULL
conn_database <- NULL

getAvailableMethods <- function() {
  return(stringMethods)
}

mongoDBConnection <- function(host, port, database, username, password) {
  if (username == "") {
    conn_url <<- paste("mongodb://", host, ":", port, sep="")
  } else {
    conn_url <<- paste("mongodb://", username, ":", password, "@", host, ":", port, sep="")
  }
  conn_database <<- database
  conn <<- mongo(url = conn_url, db = conn_database) # CONNECT TO THE MONGODB DATABASE
}

searchElement <- function(name, index, tsfrequency, returnDF) { 
  searchString <- ifelse(grepl("metrics", index, fixed=TRUE), 'metric', 
                  ifelse(grepl("factors", index, fixed=TRUE), 'factor', 
                                                              'strategic_indicator'))
                                                           
  query <- paste('{ "', searchString, '": "', name, '" }', sep="") # SEARCH FOR A NORMALIZED ELEMENT
  fields <- paste('{ "value": true, "evaluationDate": true, "_id": false }')
  limit <- 10000
  sort <- paste('{"evaluationDate": 1}')
  
  collection <- mongo(collection = index, db = conn_database, url = conn_url)
  esearch <- collection$find(query, fields = fields, limit = limit, sort = sort)
  
  valuesEsearch <- as.numeric(esearch$value)
  datesEsearch <- as.character(esearch$evaluationDate)
  datesEsearch <- as.Date(datesEsearch)
  
  if (returnDF == FALSE) { # RETURN THE ASSOCIATED TIME SERIES
    timeseries <- ts(valuesEsearch, frequency = tsfrequency, start = 0)
    return(timeseries)
  } else {
    df <- data.frame("ds" = datesEsearch, "y" = valuesEsearch)
    return(df)
  }
}

save <- function(name, index, method, element, directoryToSave) {
  cleanName <- gsub("[^[:alnum:] ]", "", name)
  dir.create(directoryToSave)
  filename <- paste(cleanName, index, method, sep = '_')
  filename <- paste(directoryToSave, filename, sep = '/')
  saveRDS(element, file = filename)
}

load <- function(name, index, method, directoryToLoadFrom) {
  cleanName <- gsub("[^[:alnum:] ]", "", name)
  filename <- paste(cleanName, index, method, sep = '_')
  filename <- paste(directoryToLoadFrom, filename, sep = '/')
  return(readRDS(filename))
}

checkModelExists <- function(name, index, method) {
  cleanName <- gsub("[^[:alnum:] ]", "", name)
  filename <- paste(cleanName, index, method, sep = '_')
  filename <- paste(directoryToSaveModels, filename, sep = '/')
  return(ifelse(file.exists(filename), TRUE, FALSE))
}

checkForecastCache <- function(name, index, method, horizon) {
  print("CHECKING CACHE...")
  cleanName <- gsub("[^[:alnum:] ]", "", name)
  filename <- paste(cleanName, index, method, sep = '_')
  filename <- paste(directoryToSaveForecasts, filename, sep = '/')
  if (file.exists(filename)) {
    forecasts <- readRDS(filename)
    return(ifelse(length(forecasts$mean)>=horizon, TRUE, FALSE))
  } else {
      return(FALSE)
  }
}

trainProphetModel <- function(name, index) {
  df <- searchElement(name, index, 7, returnDF = TRUE)
  method <- stringMethods[1]
  prophetModel <- prophet(df, daily.seasonality = 'auto', weekly.seasonality = 'auto')
  save(name, index, method, prophetModel, directoryToSaveModels)
  flist <- forecastProphet(prophetModel, forecastsCacheLength) # STORE FORECAST CACHE
  save(name, index, method, flist, directoryToSaveForecasts)
  return(prophetModel)
}

forecastProphet <- function(model, horizon) {
  future <- make_future_dataframe(model, periods = horizon, freq = 'day', include_history = FALSE)
  f <- predict(model, future)
  flist <- list("lower1" = f$yhat_lower, "lower2" = f$yhat_lower, "mean" = f$yhat,
                "upper1" = f$yhat_upper, "upper2" = f$yhat_upper)  
  return(flist)
}

forecastProphetWrapper <- function(name, index, horizon) {
  method <- stringMethods[1]
  model <- NULL
  if(checkModelExists(name, index, method)) {
    if(checkForecastCache(name, index, method, horizon)) { # IF MODEL EXISTS, CHECK THE FORECAST CACHE
      flist <- load(name, index, method, directoryToSaveForecasts) 
      return(lapply(flist, FUN=function(x) x[1:horizon])) # lapply(flist, FUN=function(x) x[1:horizon])
    } else {
        model <- load(name, index, method, directoryToSaveModels)
        flist <- forecastProphet(model, horizon)
        save(name, index, method, flist, directoryToSaveForecasts)
    }
  } else {
    model <- trainProphetModel(name, index)
    flist <- forecastProphet(model, horizon)
    if(horizon > forecastsCacheLength) {
      save(name, index, method, flist, directoryToSaveForecasts)
    }
  }
  return(flist)
}