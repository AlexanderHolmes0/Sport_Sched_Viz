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
  mutate(
    game_num = 1:nrow(data),
    info = paste0("<b>", Home, " - ", Away, "</b>")
  )

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
  tags$head(
    HTML(
      '<!-- Quick & Dirty HTML Meta Tags -->
<title>Sports Schedule Viz</title>
<meta name="description" content="Visualize Your Own Team Schedule!">

<!-- Google / Search Engine Tags -->
<meta itemprop="name" content="Sports Schedule Viz">
<meta itemprop="description" content="Visualize Your Own Team Schedule!">
<meta itemprop="image" content="https://github.com/AlexanderHolmes0/Sports_Sched_Viz/blob/main/Sample.png">

<!-- Google / Search Engine Tags -->
<meta name="title" content="Sports Schedule Viz">
<meta name="description" content="Visualize Your Own Team Schedule!">
<meta name="image" content="https://github.com/AlexanderHolmes0/Sports_Sched_Viz/blob/main/Sample.png">

<!-- Facebook Meta Tags -->
<meta property="og:url" content="https://aholmes25.shinyapps.io/Viz_Sched_App/">
<meta property="og:type" content="website">
<meta property="og:title" content="Sports Schedule Viz">
<meta property="og:description" content="Visualize Your Own Team Schedule!">
<meta property="og:image" content="https://github.com/AlexanderHolmes0/Sports_Sched_Viz/blob/main/Sample.png">

<!-- Twitter Meta Tags -->
<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:url" content="https://aholmes25.shinyapps.io/Viz_Sched_App/">
<meta name="twitter:title" content="Sports Schedule Viz">
<meta name="twitter:description" content="Visualize Your Own Team Schedule!">
<meta name="twitter:image" content="https://github.com/AlexanderHolmes0/Sports_Sched_Viz/blob/main/Sample.png">'
    ),
    tags$link(rel = "shortcut icon", href = "https://raw.githubusercontent.com/Tarikul-Islam-Anik/Animated-Fluent-Emojis/master/Emojis/Travel%20and%20places/World%20Map.png")
  ),
  useShinyjs(),
  title = "Sport Schedule Viz",
  theme = bs_theme(bootswatch = "lux"),
  fillable_mobile = TRUE,
  fillable = FALSE,
  sidebar = sidebar(
    title = "Map Controls",
    selectInput(
      "Team", "Select Team(s)",
      choices = unique(cities),
      multiple = TRUE,
      selected = unique(to_plot$Home)
    ),
    selectInput(
      "Breakdown", "Home/Away/Both",
      choices = c("Away", "Home", "All"),
      selected = "All"
    ),
    sliderInput("week", "Match Weeks", min = 1, max = 9, value = c(1, 9), step = 1, animate = animationOptions(interval = 3000, loop = TRUE)),
    sliderInput("pitch", "Map Viewing Pitch", min = 0, max = 90, value = 45, step = 1, ticks = FALSE),
    fileInput("file", "Sport Schedule File", width = "100%", accept = c(".txt")),
    actionButton("reset", "Reset", icon = icon("refresh"))
  ),
  card(
    full_screen = TRUE,
    style = "resize:vertical;",
    card_header("Route Map"),
    uiOutput("distPlot") |> withSpinner()
  ),
  card(
    full_screen = TRUE,
    style = "resize:vertical;",
    card_header("Schedule"),
    DTOutput("scheduleDT") |> withSpinner()
  )
)



server <- function(input, output) {
  set_token("pk.eyJ1IjoiYWxleGFuZGVyaG9sbWVzMCIsImEiOiJjbHBkNTk1dWkwMTJjMmtxejQ2Z3R6aXFuIn0.9tEJPCxyKZEt13dQjR5LqQ")
  observeEvent(input$reset, {
    refresh()
  })

  uploaded_data <- reactive({
    req(input$file)

    ext <- tools::file_ext(input$file$name)
    switch(ext,
      txt = read.table(input$file$datapath,
        sep = "", col.names = c("Home", "Away", "Week"), header = F,
        na.strings = "", stringsAsFactors = F
      ) |>
        left_join(locations, by = c("Home" = "cities")) |>
        left_join(locations, by = c("Away" = "cities")) |>
        rename("lat.home" = latitude.x, "long.home" = longitude.x, lat.away = latitude.y, long.away = longitude.y) |>
        mutate(info = paste0("<b>", Home, " - ", Away, "</b>")),
      validate("Invalid file; Please upload a .txt file")
    )
  })

  map_data <- reactive({
    Sys.sleep(1)
    req(to_plot)
    if (is.null(input$file)) {
      m1 <- to_plot
    } else {
      m1 <- uploaded_data()
    }

    if (input$Breakdown == "All") {
      m1 <- m1 |> filter(Home %in% input$Team | Away %in% input$Team)
    } else if (input$Breakdown == "Home") {
      m1 <- m1 |> filter(Home %in% input$Team)
    } else if (input$Breakdown == "Away") {
      m1 <- m1 |> filter(Away %in% input$Team)
    }

    m1 <- m1 |> filter(Week >= input$week[1] & Week <= input$week[2])
    to_mod1 <- m1
    to_mod2 <- m1
    diff <- abs(to_mod1$long.home - to_mod1$long.away)
    to_mod1$long.home <- ifelse(diff > 180 & to_mod1$long.home > 0, to_mod1$long.home - 360,
      ifelse(diff > 180 & to_mod1$long.home < 0, to_mod1$long.home + 360, to_mod1$long.home)
    )

    to_mod2$long.away <- ifelse(diff > 180 & to_mod2$long.away > 0, to_mod2$long.away - 360,
      ifelse(diff > 180 & to_mod2$long.away < 0, to_mod2$long.away + 360, to_mod2$long.away)
    )
    print(rbind(to_mod1, to_mod2))
    rbind(to_mod1, to_mod2)
  })

  output$scheduleDT <- DT::renderDT({
    if (is.null(input$file)) {
      to_plot |>
        select(Home, Away, Week) |>
        DT::datatable()
    } else {
      uploaded_data() |>
        select(Home, Away, Week) |>
        DT::datatable()
    }
  })
  # to_plot |> select(Home, Away, Week) |> DT::datatable()})

  output$distPlot <- renderUI({
    req(map_data())
    mapdeck(
      style = mapdeck_style("outdoors"),
      pitch = input$pitch,
      height = "80vh",
      zoom = 50
    ) %>%
      add_animated_arc(
        data = map_data(),
        origin = c("long.away", "lat.away"),
        destination = c("long.home", "lat.home"),
        layer_id = "arcs",
        stroke_from = "Away",
        stroke_to = "Home",
        stroke_from_opacity = 1000,
        stroke_to_opacity = 1000,
        stroke_width = 5,
        tooltip = "info",
        auto_highlight = TRUE,
        legend = TRUE,
        legend_options = list(
          stroke_from = list(title = "Travel Team"), stroke_to = list(title = "Home Team"),
          css = "max-height: 200px;"
        )
      ) |>
      add_arc(
        data = map_data(),
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
