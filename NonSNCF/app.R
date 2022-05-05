#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)
library(leaflet)

library(here)

here::i_am("NonSNCF/app.R")

source(here("functions.R"))
source(here("constants.R"))

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("NonSNCF!"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textInput("userinput",
                   "Ville de départ :",
                   value=""),
        selectizeInput(inputId = "city",
                    label = "Ville de départ :", 
                    choices = NULL,
                    selected = NULL,
                    options = list(placeholder = "Choisissez une ville")),
        checkboxInput("all_stations", "Toutes les gares dans cette ville ?",
                      value = TRUE),
        dateInput("dstart",
                   value = Sys.Date(),
                   "Partir le :"),
        timeInput("time", "À :",
                  value=Sys.time(),
                  seconds = FALSE),
        numericInput("max_duration", "Durée max :", 
                     value = 2, min = 1, max = 24, step = 1),
        checkboxInput("direct", "Trajet direct",
                      value = TRUE)
      ),
      mainPanel(
        textOutput("duration"),
        leafletOutput("map"),
        dataTableOutput("journeys_summary")
      )
   )
)

server <- function(input, output, session) {
  # ---
  # Get suggestions
  # ---
  suggestions_request <- reactive({
    # Search a station from user input
    res <- tryCatch(sncf_api(paste0("coverage/sncf/places?q=", input$userinput),
                             token = token),
                    error = function(e) NULL)
    res <- get_stations_from_keyword(res)
    res
  })
  
  suggestions <- reactive({
    # if something found
    res <- suggestions_request()
    if(nrow(res) != 0){
      # Get stations ids
      out <- res$id 
      # get station names and set list names with it
      names(out) <- res$name
      
      # Remove NAs
      out <- out[!is.na(out)]
    }else{
      out <- NULL
    }
  })
  
  observe({
    if(length(suggestions()) == 0){ # If no suggestions found
      if(input$userinput != ""){ # If userinput field not empty
        placeholder = "Ville non reconnue :'("
      }else{
        placeholder = "Choisissez une ville"
      }
      updateSelectizeInput(session, "city",
                           options = list(placeholder = placeholder),
                           server = TRUE)
    }else{
      updateSelectizeInput(session, "city",
                           choices = suggestions(),
                           server = TRUE)
    }
  })

   # ---
   # Convert user input date and time to POSIX
   # ---
   start_datetime <- reactive({
     dstart <- as.POSIXct(input$time)
     date(dstart) <- as.Date(input$dstart)

     dstart
   })

   max_arrival <- reactive({
     # Set max_arrival on same day as start datetime
     arrival_datetime <- start_datetime() + 
       dhours(input$max_duration)
     # res <- as.duration(arrival_datetime - start_datetime())
     arrival_datetime
   })

   output$duration <- renderText({
     # paste0("Le voyage durera au plus ", as.character(max_duration()), ".")
     paste0("Date d'arrivée max : ", as.character(max_arrival()))
   })

   nb_transfers <- reactive({ifelse(input$direct, 0, 10)})
   # ---
   # Get all journeys list
   # ---
   all_journeys <- reactive({

     if(!is.null(input$city)){
       # --- Choose start date
       dat <- as.character(start_datetime(), format = fmt)

       # --- Choose max duration
       max_duration_s <- input$max_duration*3600
       
       # --- Initialise empty dataframe
       all_journeys <- data.frame()

       # Get selected station
       station_id <- input$city
       from_city <- suggestions_request()$city[suggestions_request()$id == station_id]
       
       if(input$all_stations){ # If we want all trains from this city
         # Get all relevant stations
        stations <- suggestions_request()$id[suggestions_request()$city == from_city] 
       }else{ # Only one station
         stations <- station_id
       }
       
       # For all stations
       for(s in stations){
         # --- Request
         res <- sncf_api(paste0("coverage/sncf/journeys?from=", s,
                                "&max_duration=", max_duration_s,
                                "&datetime=", dat,
                                "&max_nb_transfers=", nb_transfers(),
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
       
       if(nrow(all_journeys) != 0){
         # Get arrival town
         all_journeys$to_city <- gsub(x = all_journeys$to_name,
                                      pattern = ".*\\((.*)\\)$",
                                      replacement = "\\1")

         # Transform datetime
         all_journeys$arrival_date_time <- as.POSIXct(all_journeys$arrival_date_time,
                                                      format=fmt,
                                                      tz=Sys.timezone(location = TRUE))
       }

     }else{
       all_journeys <- data.frame()
     }
     all_journeys
   })

   # ---
   # Summarise data for table
   # ---
   journeys_summary <- reactive({

     if(nrow(all_journeys()) != 0){
       # Sumarise for results table
       res_summ <- all_journeys() %>%
         # group_by(from_city, to_city) %>%
         group_by(from_name, to_name, from_city, to_city) %>%
         summarise(n_trains = n())

       # Filter out circular trips
       from <- unique(res_summ$from_city)
       res_summ <- res_summ %>% filter(!(to_city %in% from))

       # Reorder
       res_summ <- res_summ %>%
         arrange(desc(n_trains), to_city)
     }else{
       res_summ <- data.frame()
     }
     res_summ
   })

   # ---
   # Print data table
   # ---
   output$journeys_summary <- renderDataTable({
     shiny::validate(need(nrow(journeys_summary()) != 0, "Pas de trajets trouvés."))
     journeys_summary()
   })
   # 
   # ---
   # Create map
   # ---
   plot_journeys <- reactive({
     if(nrow(all_journeys()) != 0){
       plt <- all_journeys() %>%
         group_by(from_id, from_name, to_id, to_name) %>%
         summarise(from_lat = from_lat,
                   from_lon = from_lon,
                   to_lat = to_lat,
                   to_lon = to_lon,
                   size = n()) %>%
         ungroup()

       from <- plt %>% select(starts_with("from")) %>%
         unique() %>%
         mutate(col = "from")
       colnames(from) <- gsub(pattern = "^from_", replacement = "",
                              x = colnames(from))

       to <- plt %>% select(starts_with("to")) %>%
         unique() %>%
         mutate(col = "to")
       colnames(to) <- gsub(pattern = "^to_", replacement = "",
                            x = colnames(to))

       plt <- bind_rows(from, to)

       plt$lat <- as.numeric(plt$lat)
       plt$lon <- as.numeric(plt$lon)
     }else{
       plt <- data.frame()
     }
     plt
   })

   output$map <- renderLeaflet({
     ll <- leaflet() %>%
       setView(lng = 2.213749, lat = 46.227638, zoom = 5) %>%
       addTiles()
     
     if(nrow(plot_journeys()) != 0){
       ll <- ll %>% addMarkers(data=plot_journeys(),
                         lng = ~lon, lat = ~lat,
                         label = ~name)
     }
     ll
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

