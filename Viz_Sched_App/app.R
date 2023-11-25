library(tidyverse)
library(shiny)
library(bslib)
library(mapdeck)
library(shinycssloaders)
library(DT)
library(shinyjs)

data <- read.table("Sports_Sched.txt",
  sep = "", col.names = c("Home", "Away", "Week"), header = F,
  na.strings = "", stringsAsFactors = F
)

# Create a dataframe with latitude and longitude for cities
cities <- c("CAN", "ICE", "SWE", "RUS", "DEN", "JAP", "ANT", "AUS", "CHI", "ARG", "TAH", "FIJ")
latitude <- c(45.4247, 64.1467, 59.3294, 55.7558, 55.6761, 35.6897, -85, -35.2931, 39.9040, -34.5997, -17.5334, -18.1416)
longitude <- c(-75.6950, -21.9400, 18.0686, 37.6178, 12.5683, 139.6922, 0, 149.1269, 116.4075, -58.3819, -149.5667, 178.4419)
locations <- data.frame(cities, latitude, longitude)

to_plot <- left_join(data, locations, by = c("Home" = "cities")) |>
  left_join(locations, by = c("Away" = "cities")) |>
  rename("lat.home" = latitude.x, "long.home" = longitude.x, lat.away = latitude.y, long.away = longitude.y) |> 
  mutate(game_num=1:nrow(data),
         info=paste0("<b>",Home, " - ", Away, "</b>"))


## set your mapbox token here


ui <- page_sidebar(
  tags$style(
    type = "text/css",
    "
      .loader {
        min-height: 40px !important;
      }
    "
  ),
  useShinyjs(),
  title = "Sport Schedule Viz", 
  theme = bs_theme( bootswatch  = 'lux'),
  fillable_mobile = TRUE,
  fillable = FALSE,
  sidebar =sidebar(
      title = "Map Controls",
      selectInput(
        "Team", "Select Team(s)",
        choices = unique(cities),
        multiple = TRUE,
        selected = unique(to_plot$Home)
      ),
      selectInput(
        "Breakdown", "Home/Away/Both",
        choices = c('Away','Home','All'),
        selected='All'
      ),
      sliderInput('week', 'Match Weeks', min = 1, max = 9,value=c(1,9),step = 1,animate = animationOptions(interval = 3000, loop = TRUE))
      ,
      sliderInput('pitch', 'Map Viewing Pitch', min = 0, max = 90, value = 45, step = 1,ticks = FALSE),
      fileInput('file','Sport Schedule File',width = '100%',accept = c(".txt")),
      actionButton("reset", "Reset", icon = icon("refresh"))
    ),
  card(full_screen = TRUE,
       style = "resize:vertical;",
    card_header("Route Map"),
    uiOutput("distPlot") |> withSpinner()),
  card(full_screen = TRUE,
       style = "resize:vertical;",
    card_header("Schedule"),
    DTOutput("scheduleDT") |> withSpinner())
)



server <- function(input, output) {
  set_token("YourMapBoxToken")
  observeEvent(input$reset, { 
    refresh()
  })
  
  uploaded_data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           txt = read.table(input$file$datapath,
                            sep = "", col.names = c("Home", "Away", "Week"), header = F,
                            na.strings = "", stringsAsFactors = F) |> 
             left_join(locations, by = c("Home" = "cities")) |>
             left_join(locations, by = c("Away" = "cities")) |>
             rename("lat.home" = latitude.x, "long.home" = longitude.x, lat.away = latitude.y, long.away = longitude.y) |> 
             mutate(info=paste0("<b>", Home, " - ", Away, "</b>")),
           validate("Invalid file; Please upload a .txt file")
    )
  })
  
  map_data <- reactive({
    Sys.sleep(1)
    req(to_plot)
    if(input$Breakdown == "All"){
      m1 <- to_plot |> filter(Home %in% input$Team | Away %in% input$Team)
    } else if (input$Breakdown == "Home"){
      m1 <- to_plot |> filter(Home %in% input$Team)
    } else if (input$Breakdown == "Away"){
      m1 <- to_plot |> filter(Away %in% input$Team)
    }
    
    m1 |> filter(Week >= input$week[1] & Week <= input$week[2])
  })
  
  output$scheduleDT <- DT::renderDT({
    if(is.null(input$file)) {to_plot |> select(Home, Away, Week) |> DT::datatable()} 
    else {uploaded_data() |> select(Home, Away, Week) |> DT::datatable()}
  })
    #to_plot |> select(Home, Away, Week) |> DT::datatable()})
  
  output$distPlot <- renderUI({
    req(map_data())
    mapdeck(
      style = mapdeck_style("outdoors"),
      pitch = input$pitch,
      height = "80vh",
      zoom = 50) %>%
      add_animated_arc(
        data = if(is.null(input$file)) {map_data()} else{uploaded_data()},
        origin = c("long.away", "lat.away"),
        destination = c("long.home", "lat.home"),
        layer_id = "arcs",
        stroke_from = "Away",
        stroke_to = "Home",
        stroke_from_opacity = 1000,
        stroke_to_opacity = 1000,
        stroke_width = 3,
        tooltip = "info",
        auto_highlight = TRUE,
        legend = TRUE,
        legend_options = list(
          stroke_from = list(title = "Travel Team"), stroke_to = list(title = "Home Team"),
          css = "max-height: 200px;"
        )
      ) |> 
      add_arc(
        data = if(is.null(input$file)) {map_data()} else{uploaded_data()},
        origin = c("long.away", "lat.away"),
        destination = c("long.home", "lat.home"),
        layer_id = "arcs_static",
        stroke_from = "Away",
        stroke_to = "Home",
        stroke_from_opacity = 100,
        stroke_to_opacity = 100,
        stroke_width = 3,
        tooltip = "info",
        auto_highlight = TRUE
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
