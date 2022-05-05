library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)

# ------------------------------------
sncf_api <- function(path, token) {
  # Code from https://httr.r-lib.org/articles/api-packages.html
  
  # Add path to base URL
  url <- paste("https://api.sncf.com/v1", path, sep = "/")
  
  # GET request
  resp <- GET(url,
              add_headers("Authorization" = token))
  
  # Check type
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  # Parse JSON
  # parsed <- jsonlite::fromJSON(content(resp, "text"), 
  #                              simplifyVector = FALSE)
  parsed <- fromJSON(rawToChar(resp$content))
  
  # Return error in case
  if (http_error(resp)) {
    stop(
      sprintf(
        "SNCF API request failed [%s]\n%s", 
        status_code(resp),
        parsed$message
      ),
      call. = FALSE
    )
  }
  
  # S3 object
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "sncf_api"
  )
}

get_stations_from_keyword <- function(res){
  # Returns a df with info about all stations in administrative
  # area containing keyword.
  # Typically, returns all train stations in a town.
  # ### Inputs
  # res: output of a query on the API
  # ### Outputs
  # a df of stop_areas, with additional column "city"
  
  if("places" %in% names(res$content)){
    tab <- res$content$places
    stations <- tab[tab$embedded_type == "stop_area",]
    
    # To do later
    # # Get all stations which are effectively in this town (eg pb with Gare de Lyon in Paris)
    # in_town <- sapply(stations$stop_area$administrative_regions, 
    #                   function(i){grepl(pattern = paste0(".*", search, ".*"), 
    #                                     x = i$name, 
    #                                     ignore.case = TRUE)})
    # in_town <- unlist(in_town)
    # stations <- stations[in_town,]
    
    # --- Get start city to filter out circular trips later
    stations$city <- gsub(x = stations$name,
                          pattern = ".*\\((.*)\\)$",
                          replacement = "\\1")
  }else{
    stations <- data.frame()
  }
  
  
  return(stations)
}

clean_journeys <- function(journeys, from){
  # Cleans a df of journeys (output of a query in the journey endpoint)
  # ### Inputs
  # journey: output of type sncf_api fro a query in the journey endpoint
  # from: city corresponding to the station at the start of journey.
  # ### Outputs
  # A cleaned dataframe, which looks more like a dataframe than a JSON object.
  # Has columns :
  #     from_name, from_id, from_coord, from_city,
  #     nb_transfers, arrival_date_time,
  #     to_name, to_id, to_coord, links
  
  res <- journeys %>%
    select(from, to, links, nb_transfers, arrival_date_time)
  
  # Keep only from id and name
  res$from_id <- res$from$stop_area$id
  res$from_name <- res$from$stop_area$name
  from_coord <- res$from$stop_area$coord
  res$from_lat <- from_coord$lat
  res$from_lon <- from_coord$lon
  
  res$from_city <- rep(from, nrow(res))
  
  # Keep only to id 
  res$to_name <- res$to$name
  res$to_id <- res$to$id
  to_coord <- res$to$stop_point$coord
  res$to_lat <- to_coord$lat
  res$to_lon <- to_coord$lon
  
  # Keep only href for link
  links <- sapply(res$links, function(li) li$href)
  res$links <- links
  
  # Remove big from/to tables
  res <- res %>%
    select(-c(from, to))
  
  # Reorder columns 
  res <- res %>% select(from_name, from_id, from_lat, from_lon, from_city,
                        nb_transfers, arrival_date_time,
                        to_name, to_id, to_lat, to_lon, 
                        links)
  return(res)
}
# ----------------------------------------------------------------