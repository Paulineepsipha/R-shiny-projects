############INSTALL REQUIRED PACKAGES###############
# install.packages("shinydashboard")
# install.packages("timevis")
# install.packages("fontawesome")
# install.packages("fresh")
# install.packages("readxl")
# install.packages("gfonts")

#####Loaded Packages:
library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet) # For making maps
library(timevis)
library(fontawesome)
library(fresh)
library(dplyr)
library(readr) 
library(readxl)
library(ggplot2)
library(gfonts)

# Load all data
question1 <- read_csv("question1_new.csv")
question2 <- read_csv("question2.csv")
question3 <- read_csv("question3.csv")


##Custom setting CSS styles:
custom_css <- "
.my-full-width-box {
  width: 100%;
  margin: 0;       
  padding: 0;    
  font-family: 'Roboto Slab', serif;
}

#smallValueBox .small-value-box {
  width: 100%;    /* Adjust the width as needed */
  height: 100px;  /* Adjust the height as needed */
  font-family: 'Roboto Slab', serif;
}
"
# Define UI for application
ui <- fluidPage(
  tags$script(src = "https://kit.fontawesome.com/38ebadab8d.js"),
  tags$head(tags$link(rel = "preconnect", href = "https://fonts.googleapis.com")),
  tags$head(tags$link(rel = "preconnect", href = "https://fonts.gstatic.com")),
  tags$head(tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@700&display=swap")),
  tags$head(
    tags$style(HTML("
      i { padding: 0 1rem; }
    "))
  ),
  # Include the Font Awesome kit script in the HTML header
  tags$head(tags$style(HTML(custom_css))),  
  dashboardPage(title = "TOURDEFRANCE",
                dashboardHeader(
                  title = "CYCLING THROUGH TIME: AN ANALYSIS ON TOUR-DE-FRANCE FROM 1903 – 2022", 
                  titleWidth = "850px"),
                dashboardSidebar(disable = TRUE),
                dashboardBody(
                  fluidRow(
                    box(
                      div(
                        style = "height: 15px; font-size: 35px; font-family: 'Georgia', sans-serif; font-weight: bold; text-align: center; color: #1972A4;",
                        'TOUR-DE-FRANCE'
                      ),
                      div(style = "margin-top: 35px;"),
                      div(
                        style = "height: 15px; font-size: 15px; font-family: 'Georgia', sans-serif; font-weight: bold; text-align: center; color: #1972A4;",
                        'Iconic annual men Bi-cycle race primarily held in France from the year 1903 to till now. It is oldest of the three Grand Tours and considered most prestigious. '
                      ),
                      width = 12,
                      height = 150,
                      solidHeader = TRUE,
                      div(style = "margin-top: 20px;"),
                      div(
                        style = "height: 15px; font-size: 25px; font-family: 'Georgia', sans-serif; font-weight: bold; text-align: center; color: #1972A4;",
                        'Let us delve into the rich history!!!'
                      ),
                      ),
                    box(
                      div(
                        style = "height: 15px; font-size: 25px; font-family: 'Georgia', sans-serif; font-weight: bold; text-align: center;",
                        'Important Timeline Events(Drag & Zoom)'
                      ),
                      width = 12,
                      height = 380,
                      solidHeader = TRUE,
                      div(style = "margin-top: 50px;"),
                      timevisOutput("mjTimeline"),
                      div(
                        style = "height: 15px; font-size: 15px; font-family: 'Georgia', sans-serif; font-weight: bold;",
                        'The above timeline displays the most notable events in the history of Tour-de-France. You can drag and zoom on the timeline to explore these events.'
                      ),
                    ),
                    box(
                      width = 12,
                      height = 130,
                      solidHeader = TRUE,
                      valueBox(
                        value = 109,  
                        subtitle = "Years",
                        icon = icon("calendar"),
                        color = "light-blue",
                        width = 3
                      ),
                      valueBox(
                        value = 15705,  
                        subtitle = "Total Starters",
                        icon = icon("globe"),
                        color = "light-blue",
                        width = 3
                      ),
                      valueBox(
                        value = 9895,  
                        subtitle = "Total Finishers",
                        icon = icon("thumbs-up"),
                        color = "light-blue",
                        width = 3
                      ),
                      valueBox(
                        value = 104,  
                        subtitle = "Total Winners",
                        icon = icon("star"),
                        color = "light-blue",
                        width = 3
                      )
                    ),
                    # Right Column (split into 4 boxes)
                      fluidRow(
                      column(8,
                             box(
                               div(
                                 style = "height: 15px; font-size: 25px; font-family: 'Georgia', sans-serif; font-weight: bold; text-align: center;",
                                 'Country Participated in Tour-De-France'
                               ),
                               width = "200px",  
                               height = "600px",
                               solidHeader = TRUE,
                               div(style = "margin-top: 50px;"),
                               radioButtons("mapType", label = NULL,
                                            choices = c("Winner", "Finisher"), inline = TRUE,
                                            selected = "Winner"),
                               leafletOutput("map"),
                               div(
                                 style = "height: 15px; font-size: 15px; font-family: 'Georgia', sans-serif; font-weight: bold;",
                                 'This above map displays the Participated countries around the world in the Tour-de-France. There were riders from 15 countries who have won in history and riders from 55 countries who have completed the race sofar. You can choose to view winners or finishers on the map.'
                               ),
                             )
                             ),
                        column(4,
                             box(
                               div(
                                 style = "height: 15px; font-size: 25px; font-family: 'Georgia', sans-serif; font-weight: bold; text-align: center;",
                                 'Country dominance'
                               ),
                               width = "200px",  
                               height = "600px",
                               solidHeader = TRUE,
                               div(style = "margin-top: 80px;"),
                               plotlyOutput("top_countries_chart"),
                               div(
                                 style = "height: 15px; font-size: 15px; font-family: 'Georgia', sans-serif; font-weight: bold;",
                                 'The above chart displays the top 10 country dominance as winners and finishers'
                               ),
                             ) 
                        )
                      ),
                           fluidRow(  
                             column(8,  
                                    box(
                                      div(
                                        style = "height: 15px; font-size: 25px; font-family: 'Georgia', sans-serif; font-weight: bold; text-align: center;",
                                        'Evolution of Stage Types over Years'
                                      ),
                                      width = "200px",
                                      height = "660px",
                                      solidHeader = TRUE,
                                      div(style = "margin-top: 50px;"),
                                      sliderInput("yearRange", "Select Year Range", min = 1903, max = 2022, value = c(1903, 2022)),
                                      step = 1, sep = "", pre = "", post = "", dragRange = TRUE,
                                      plotlyOutput("stackedBarChart"),
                                      div(
                                        style = "height: 15px; font-size: 15px; font-family: 'Georgia', sans-serif; font-weight: bold;",
                                        'The above Stackedbar chart displays the evolution of stage types over the years in the Tour-de-France. You can notice the stages were increased over the years. You can select a specific year range to explore.'
                                      ),
                                    )
                             ),
                             column(4,
                                    box(
                                      div(
                                        style = "height: 15px; font-size: 20px; font-family: 'Georgia', sans-serif; font-weight: bold; text-align: center;",
                                        'Distribution of European and Non-European Regions'
                                      ),
                                      width = "200px",
                                      height = "660px",
                                      solidHeader = TRUE,
                                      div(style = "margin-top: 80px;"),
                                      actionButton("rwin", "Winners"),
                                      actionButton("rfin", "Finishers"),
                                      plotlyOutput("donutChart"),
                                      div(
                                        style = "height: 15px; font-size: 15px; font-family: 'Georgia', sans-serif; font-weight: bold;",
                                        "The above donut chart shows the distribution of European and non-European regions among Tour-de-France winners and finishers."
                                      ),
                                      
                                    )
                             )
                           ),
                  fluidRow(
                    box(
                      div(
                        style = "height: 15px; font-size: 25px; font-family: 'Georgia', sans-serif; font-weight: bold; text-align: center;",
                        'Evolution of Speed, Time, Riders over years'
                      ),
                      width = 12,  
                      height = "700px",
                      solidHeader = TRUE,
                      div(style = "margin-top: 80px;"),
                      selectInput(inputId = "select_variable", 
                                  label = "Choose one chart", 
                                  choices = c("Total Starters Vs Total Finishers", "Total distance(km) Vs time taken to win (mins)", "Total distance(km) Vs average speed to win (Kmh)"), 
                                  selected = "Total Starters Vs Total Finishers"),
                      plotlyOutput('myplot'),
                      div(
                        style = "height: 15px; font-size: 15px; font-family: 'Georgia', sans-serif; font-weight: bold;",
                        "The  above three charts provides an overview of the evolution of Tour-de-France statistics over the years. You can choose from different charts to explore the data."
                      ),
                    )
                  )
                )
                )
                )
  )


# Define server logic:
server <- function(input, output) {
  
  ##########################Timeline Box########################################################################################
  # Create Horizontal Timeline box
  tourdefrance_time <- data.frame(
    id = 1:12,
    content = c("First Tourdefrance held(1903)", "Point system start(1905)","Point system end(1912)", "WORLD WAR I Start(1914)","WORLD WAR I End(1918)", "Introduction of the Yellow Jersey(1930)", "WORLD WAR II Start(1939)", "WORLD WAR II End(1945)", "First Foreign Winner(1953)","No Winner(1999 - 2005)", "Centenary Edition(2003)","Happening till now(2023)"),
    start   = c("1903-05-31","1905-05-01","1912-01-01","1914-06-28", "1918-11-11", "1930-07-17" ,"1939-09-01", "1945-09-02", "1953-07-19","1999-05-05","2003-07-01","2023-10-29"),
    end     = c(NA,NA, NA,NA,NA,NA ,NA,NA, NA,"2005-05-05",NA ,NA)
    # className   = c("blue_style","purple_style", "green_style", "yellow_style", "red_style", "orange_style"),
    # style = "font-weight: light"
  )
  
  output$mjTimeline <- renderTimevis({
    timevis(tourdefrance_time,
            showZoom = TRUE,
            options = list(orientation = "top"))
  })
  ##########################Map box########################################################################################
  
  question1$rank <- as.integer(question1$rank)
  countries_code <- read_excel("countries-codes.xlsx")
  merged_data <- merge(question1, countries_code, by.x = "rider_country", by.y = "LABEL EN", all.x = TRUE)
  selected_columns <- c("year", "rank", "rider_name", "rider_country", "eur_non","geo_point_2d")
  merged_data1 <- merged_data[selected_columns]
  merged_data1$rank <- as.integer(merged_data1$rank)
  merged_data2 <- merged_data1[order(merged_data1$year, merged_data1$rank), ]
  
  geo_points <- strsplit(merged_data2$geo_point_2d, ",")
  for (i in seq_along(geo_points)) {
    if (length(geo_points[[i]]) == 2) {
      merged_data2$longitude[i] <- as.numeric(geo_points[[i]][2])
      merged_data2$latitude[i] <- as.numeric(geo_points[[i]][1])
    }
  }
  
  merged_data3 <- merged_data2 %>% filter(rank == 1)
  

  top_winner_countries <- reactive({
    merged_data3 %>%
      group_by(rider_country) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      head(10)
  })

  top_finisher_countries <- reactive({
    merged_data2 %>%
      group_by(rider_country) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      head(10)
  })
  
  #---------------------------------------------------
  
  output$map <- renderLeaflet({
    if (input$mapType == "Winner") {
      leaflet(data = merged_data3, width = "100%", height = "300px") %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 2.3522, lat = 48.8566, zoom = 2) %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = 6,
          color = "#01084f",
          fillColor = "#01084f",
          fillOpacity = 0.7,
          weight = 1,
          opacity = 1,
          stroke = TRUE,
          label = ~paste("Country:", rider_country)
        )
    } else if (input$mapType == "Finisher") {
      all_finishers_summary <- merged_data2 %>%
        group_by(rider_country, longitude, latitude) %>%
        summarize(n = n_distinct(rider_name)) %>%
        ungroup()
      
      leaflet(data = all_finishers_summary, width = "100%", height = "300px") %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 2.3522, lat = 48.8566, zoom = 2) %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = 6,
          color = "#FF5733",
          fillColor = "#FF5733",
          fillOpacity = 0.7,
          weight = 1,
          opacity = 1,
          stroke = TRUE,
          label = ~paste("Country:", rider_country)
        )
    }
  })
  
  #------------------------------------------------------
  output$top_countries_chart <- renderPlotly({
    # Depending on the selected mapType, you can choose the appropriate data frame.
    data <- if (input$mapType == "Winner") {
      top_winner_countries()
    } else {
      top_finisher_countries()
    }
    
    # Create the bar chart
    fig <- plot_ly(data, x = ~count, y = ~reorder(rider_country, count), type = "bar", orientation = 'h', marker = list(color = ~count, colors = "Magma")) %>% 
      layout(
      xaxis = list(title = "Count"),
      yaxis = list(title = "Country"),
      title = "Top 10 Countries",
      showlegend = FALSE
    )
    
    fig
  })

  
  ##########################Do-nut chart box########################################################################################
  merged_data3 <- merged_data2 %>% filter(rank == 1)
  # View(merged_data3)
  
  output$donutChart <- renderPlotly({
    data <- merged_data3 %>%
      group_by(eur_non) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    # Define custom colors
    custom_colors <- c("#1972A4", "#FF5733", "#A5A5A5") # Add more colors as needed
    
    fig <- plot_ly(data, labels = ~eur_non, values = ~count, type = 'pie', hole = 0.6) %>%
      add_trace(marker = list(colors = custom_colors))
    
    fig <- fig %>% layout(legend = list(orientation = "h"))
    fig
  })
  
  # Listen for the "Winners" button click
  observeEvent(input$rwin, {
    data <- merged_data3 %>%
      group_by(eur_non) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    # Define custom colors
    custom_colors <- c("#1972A4", "#a73c5a") 
    
    fig <- plot_ly(data, labels = ~eur_non, values = ~count, type = 'pie', hole = 0.6) %>%
      add_trace(marker = list(colors = custom_colors))
    
    fig <- fig %>% layout(legend = list(orientation = "h"))
    output$donutChart <- renderPlotly(fig)
  })
  
  # Listen for the "Finishers" button click
  observeEvent(input$rfin, {
    data <- merged_data2 %>%
      group_by(eur_non) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    # Define custom colors
    custom_colors <- c("#1972A4", "#A5A5A5")
    
    fig <- plot_ly(data, labels = ~eur_non, values = ~count, type = 'pie', hole = 0.6) %>%
      add_trace(marker = list(colors = custom_colors))
    
    fig <- fig %>% layout(legend = list(orientation = "h"))
    output$donutChart <- renderPlotly(fig)
  })
  
  ##########################Stacked-chart box########################################################################################
  ##Create StackedBarchart
  question3 <- distinct(question3)
  question3 <- question3[order(question3$year),]
  question3$stage_types <- tolower(question3$stage_types)
  distinct_stage_types <- unique(question3$stage_types)
  
  # Group the data by year, count distinct stages, and create a stacked bar chart
  distinct_stages <- question3 %>%
    group_by(year, stage_types) %>%
    summarize(n_distinct_stages = n_distinct(stage_types))
  
  filtered_data <- reactive({
    question3 %>%
      filter(year >= input$yearRange[1] & year <= input$yearRange[2])
  })
  
  output$yearRangeLabel <- renderUI({
    yearRange <- input$yearRange
    yearRangeLabel <- paste(yearRange[1], " - ", yearRange[2])
    tags$p(yearRangeLabel)
  })
  
  # Create the stacked bar chart
  output$stackedBarChart <- renderPlotly({
    ggplot(filtered_data(), aes(x = year, fill = stage_types)) +
      geom_bar(width = 0.7, position = "stack") +
      labs(x = "Year", y = "Stage Types Count", title = "Stages in each year") +
      scale_fill_brewer(palette = "PuBu") +
      theme_classic() +
      theme(legend.position = "right")
  })
  ##########################TimeSeries chart box########################################################################################
  
  output$myplot <- renderPlotly({
    selected_chart <- switch(input$select_variable,
                             "Total Starters Vs Total Finishers" = {
                               ggplot(question2, aes(x = year)) +
                                 geom_line(aes(y = total_starters, color = "Total Starters")) +
                                 geom_line(aes(y = total_finishers, color = "Total Finishers")) +
                                 scale_x_continuous(breaks = seq(1900, 2023, by = 5)) +
                                 scale_color_manual(values = c("Total Starters" = "blue", "Total Finishers" = "#ffd400")) +
                                 labs(x = "Year", y = "Total count", title = "Total Starters Vs Total Finishers Over years") +
                                 theme_classic() +
                                 theme(legend.position = "top")
                             },
                             "Total distance(km) Vs time taken to win (mins)" = {
                               ggplot(question2, aes(x = year)) +
                                 geom_line(aes(y = total_distance_km, color = "Total distance in Km")) +
                                 geom_line(aes(y = winning_time_min, color = "Total time taken to win")) +
                                 scale_x_continuous(breaks = seq(1900, 2023, by = 5)) +
                                 scale_color_manual(values = c("Total distance in Km" = "#01084f", "Total time taken to win" = "#a73c5a")) +
                                 labs(x = "Year", y = "Total distance", title = "Total Distance (Km) vs Time Taken to Win the competition (mins) over years") +
                                 theme_classic() +
                                 theme(legend.position = "top") +
                                 scale_y_continuous(sec.axis = sec_axis(~ . / 100, name = "Winning time (hrs)"))
                             },
                             "Total distance(km) Vs average speed to win (Kmh)" = {
                               ggplot(question2, aes(x = year)) +
                                 geom_line(aes(y = total_distance_km,color = "Total distance in Km")) +
                                 geom_line(aes(y = avg_speed_kmh * 100, color = "Average speed in Kmh")) +
                                 scale_x_continuous(breaks = seq(1903, 2023, by = 5)) +
                                 scale_color_manual(values = c("Total distance in Km" = "#01084f", "Average speed in Kmh" = "#FF5733")) +
                                 labs(x = "Year", y = "Total distance", title = "Total Distance (Km) vs Average speed(Kmh) by winner over years")  +
                                 theme_classic() +
                                 theme(legend.position = "top") +
                                 scale_y_continuous(sec.axis = sec_axis(~ . / 100, name = "Average speed (Kmh)"))  # Add sec_axis for the second y-axis
                             }
    )
    
    if (!is.null(selected_chart)) {
      print(selected_chart)
    }
  })
  
}

shinyApp(ui = ui, server = server)

######################REFERENCES##################################################
##Reference: https://github.com/daattali/timevis
##Reference: https://github.com/rstudio/fontawesome
##Reference: https://cran.r-project.org/web/packages/gfonts/index.html
##Reference: https://stackoverflow.com/questions/52820202/r-fluidrow-column-font-size
##Reference: https://rstudio.github.io/shinydashboard/structure.html
##Reference: https://www.rdocumentation.org/packages/shinydashboard/versions/0.7.2/topics/valueBox
##Reference: https://shiny.posit.co/r/articles/build/action-buttons/
##Reference: https://shiny.posit.co/r/reference/shiny/latest/radiobuttons
##Reference: https://shiny.posit.co/r/reference/shiny/0.12.2/selectinput
##Reference: https://rstudio.github.io/shiny/reference/observeEvent.html