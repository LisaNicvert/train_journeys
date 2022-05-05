library(here)

here::i_am("test.R")

source(here("functions.R"))
source(here("constants.R"))
library(tidyr)


# ----------------------------------------
# Get a station name to search
# ----------------------------------------
key <- "Lyon"

res <- tryCatch(sncf_api(paste0("coverage/sncf/places?q=", key),
                         token = token),
                error = function(e) NULL)

# sncf_api(paste0("coverage/sncf/places?q=", key),
#          token = token)
# debug(get_stations_from_keyword)
stations <- get_stations_from_keyword(res)

v <- stations$stop_area$name
# ----------------------------------------
# Search all journeys from station
# ----------------------------------------

# --- Get date to search from
now <- Sys.time()
hnow <- hour(now)

dstart <- now + ddays(1)

hour(dstart) <- 06
minute(dstart) <- 00
second(dstart) <- 00

# if(hnow > 12){
#   # Search from tomorrow at 6am
#   dstart <- now + ddays(1)
# 
#   hour(dstart) <- 06
#   minute(dstart) <- 00
#   second(dstart) <- 00
# }else{
#   dstart <- now
# }
# dstart <- now()

# --- Choose start date
dat <- as.character(dstart, format = fmt)

# --- Choose max duration
max_duration_hours <- 2
max_duration_s <- max_duration_hours*3600

# --- Initialise empty dataframe
all_journeys <- data.frame()

for(i in 1:nrow(stations)){
  stat <- stations[i, ]
  print(stat$name)
  
  station_id <- stat$id
  from_city <- stat$city
  
  # --- Request
  res <- sncf_api(paste0("coverage/sncf/journeys?from=", station_id,
                         "&max_duration=", max_duration_s,
                         "&datetime=", dat,
                         "&datetime_represents=departure"), 
                  token = token)

  
  if("journeys" %in% names(res$content)){
    journeys <- res$content$journeys
    
    # --- Clean result
    journeys <- clean_journeys(journeys, from = from_city)
  }else{
    journeys <- data.frame()
  }
  
  all_journeys <- bind_rows(all_journeys,
                            journeys)
}

# Get arrival town
all_journeys$to_city <- gsub(x = all_journeys$to_name,
                             pattern = ".*\\((.*)\\)$",
                             replacement = "\\1")

# Transform datetime
all_journeys$arrival_date_time <- as.POSIXct(all_journeys$arrival_date_time, 
                                             format=fmt, 
                                             tz=Sys.timezone(location = TRUE))


plot_journeys <- all_journeys %>%
  group_by(from_id, to_id) %>%
  summarise(from_lat = from_lat,
            from_lon = from_lon,
            to_lat = to_lat,
            to_lon = to_lon,
            size = n()) %>%
  ungroup()



from <- plot_journeys %>% select(starts_with("from")) %>%
  unique() %>%
  mutate(col = "from")
colnames(from) <- gsub(pattern = "^from_", replacement = "", 
                       x = colnames(from))

to <- plot_journeys %>% select(starts_with("to")) %>%
  unique() %>%
  mutate(col = "to")
colnames(to) <- gsub(pattern = "^to_", replacement = "", 
                       x = colnames(to))

plot_journeys <- bind_rows(from, to)
as.numeric(plot_journeys$lat)

# Sumarise for results table
res_summ <- all_journeys %>%
  group_by(from_city, to_city) %>%
  summarise(n_trains = n())

# Filter out circular trips
from <- unique(res_summ$from_city)
res_summ <- res_summ %>% filter(!(to_city %in% from))

# Reorder
res_summ <- res_summ %>%
  arrange(desc(n_trains), to_city)


# # --- Prune url (remove base)
# nc <- nchar("https://api.sncf.com/v1/")
# all.journeys$links <- substr(all.journeys$links, nc+1, nchar(all.journeys$links))
# 
# # ----------------------------------------
# # Filter journeys based on duration
# # ----------------------------------------
# 
# # Initialise empty results
# res_all <- data.frame()
# 
# # Search all journeys starting from there with max duration
# max_duration_hours <- 2
# max_duration_s <- max_duration_hours*3600
# 
# # Go through all journeys found
# for(i in 1:nrow(all.journeys)){
# 
#   row <- all.journeys[i,]
#   message("Journey to: ", row$to_id, " (", i, "/", nrow(all.journeys), ")")
#   
#   # --- Request
#   res_i <- sncf_api(row$links,
#                     token = token)
#   
#   # --- Filter request results on TRAVEL DURATION!!
#   df <- res_i$content$journeys %>%
#     mutate(tot_duration = res_i$content$journeys$durations$total)
#   
#   df <- df %>% filter(tot_duration < max_duration_s)
#   
#   if(nrow(df) != 0){
#     # Get stop area for this journey
#     first_to <- df$sections[[1]]$to$stop_point$stop_area$id
#     to_area_id <- first_to[length(first_to)] 
#     
#     first_to <- df$sections[[1]]$to$stop_point$stop_area$name
#     to_area_name <- first_to[length(first_to)] 
#     
#     n_trains <- nrow(df)
#     mean_duration <- mean(df$tot_duration)
#     
#     res_i <- data.frame(from_area_id, from_area_name, 
#                         to_area_id, to_area_name,
#                         mean_duration, n_trains)
#     
#     res_all <- bind_rows(res_all, res_i)
#   }
#   
# }
