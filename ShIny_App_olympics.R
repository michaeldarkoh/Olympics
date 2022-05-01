#Libraries 
library(shiny)
library(tidyverse)
library(here)
library(DT)
library(shinythemes)
library(htmltools)

#Load data
olympics_data <-readRDS("data.rds")

#Map countries and cities to the correct host city
countries <- maps::world.cities %>% 
  select(city = name,
         country = country.etc) %>% 
  distinct()

olympics_countries <- 
  olympics_data %>% 
  left_join(countries, by = "city") %>%
  mutate(country = case_when(
    city == "Barcelona" ~ "Spain",
    city == "London" ~ "United Kingdom",
    city == "Paris" ~ "France",
    city == "Los Angeles" ~ "California, USA",
    city == "Salt Lake City" ~ "Utah, USA",
    city == "Antwerpen" ~ "Belgium",
    city == "Lake Placid" ~ "New York, USA",
    city == "Sydney" ~ "Australia",
    city == "Atlanta" ~ "Georgia, USA",
    city == "Torino" ~ "Italy",
    city == "Athina" ~ "Greece",
    city == "Squaw Valley" ~ "California, USA",
    city == "Seoul" ~ "South Korea",
    city == "Berlin" ~ "Germany",
    city == "Cortina d'Ampezzo" ~ "Italy",
    city == "Melbourne" ~ "Australia",
    city == "Roma" ~ "Italy",
    city == "Moskva" ~ "Soviet Union",
    city == "Vancouver" ~ "Canada",
    city == "Chamonix" ~ "France",
    city == "St. Louis" ~ "Missouri, USA",
    TRUE ~ country
  )) %>% 
  distinct(city, country, year, season)

#UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "olympics.css")
  ),
  
  # Title of the Application
  titlePanel("Olympics History, Statistics and Results"),
  
  # Sidebar with various radioButtons and selectInputs
  sidebarLayout(
    sidebarPanel(
      img(src = "Olympic-Games-page-banner.png", height = 430, width = "100%"),
      radioButtons(inputId = "season",
                   label = "Select season",
                   choices = c("Summer", "Winter")),
      selectInput(inputId = "year",
                  label = "Select year",
                  choices = sort(unique(olympics_data$year))),
      selectInput(inputId = "sport", label = "Select a sport",
                  choices = sort(unique(olympics_data$sport))),
      selectInput(inputId = "event", label = "Select an event",
                  choices = sort(unique(olympics_data$event)))
    ),
    
    #Plot and table summary of winners
    mainPanel(
      textOutput("intro"),
      plotOutput(outputId = "barchart"),
      dataTableOutput('table')
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  observeEvent(input$season,
               {
                 
                 updateSelectInput(session, "year",
                                   choices = olympics_data %>% filter(season == input$season) %>% 
                                     distinct(year) %>% arrange(year) %>% pull()
                 )
                 updateSelectInput(session, "sport",
                                   choices = olympics_data %>% filter(season == input$season) %>%  
                                     distinct(sport) %>% arrange(sport) %>% pull(sport))
                 updateSelectInput(session, "event",
                                   choices = olympics_data %>% filter(season == input$season) %>%  
                                     distinct(event) %>% arrange(event) %>% pull(event))
               }
  )
  
  observeEvent(input$year,
               {
                 
                 updateSelectInput(session, "sport",
                                   choices = olympics_data %>% filter(year == input$year,
                                                                 season == input$season) %>%  
                                     distinct(sport) %>% arrange(sport) %>% pull(sport))
                 updateSelectInput(session, "event",
                                   choices = olympics_data %>% filter(year == input$year,
                                                                 season == input$season) %>%  
                                     distinct(event) %>% arrange(event) %>% pull(event))
               }
               
  )
  observeEvent(input$sport,
               {
                 updateSelectInput(session, "event",
                                   choices = olympics_data %>% filter(year == input$year,
                                                                 season == input$season,
                                                                 sport == input$sport) %>%  
                                     distinct(event) %>% arrange(event) %>% pull(event))
               }
  )
  #Display Rseults in a Chart
  output$barchart <- renderPlot({
    chart <-
      olympics_data %>%
      filter(!is.na(medal),
             year == input$year, 
             season == input$season) %>%
      mutate(team = gsub('-[0-9]+', '', team)) %>%
      distinct(team, event, medal) %>%
      count(team, medal) %>% 
      mutate(medal = factor(medal, levels = c("Gold", "Silver", "Bronze"))) %>% 
      group_by(team) %>%
      arrange(medal) %>% 
      ungroup() %>% 
      add_count(team, wt = n, name = "num_medals") %>% 
      mutate(rank = dense_rank(desc(num_medals))) 
    
    chart_data <-
      chart %>%
      filter(rank %in% 1:5) 
    
    if (length(unique(chart_data$team)) > 10){
      chart_data <-
        chart %>%
        filter(rank %in% 1:3) 
    }
    
    chart_data %>% 
      ggplot(aes(x = reorder(team, -num_medals), y = n, fill = medal)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      geom_text(aes(label = paste0(n, " ", medal)), position = position_stack(vjust = 0.5), 
                color = "white", size = 4, fontface = "bold") +
      stat_summary(fun = sum, aes(label = paste(..y.., "medals"), group = team), 
                   geom = "text", vjust = -.3, size = 5) +
      labs(title = paste0("Top ", length(unique(chart_data$team)), " Teams in the ", input$year, " ", input$season, 
                          " Olympic Games"),
           x = "Country",
           y = "Total number of Medals Won") +
      scale_fill_manual("legend", values = c("Gold" = "gold", "Silver" = "#C0C0C0", 
                                             "Bronze" = "#cd7f32")) + 
      theme(legend.position = "none") 
    
  })
  
  # reactive data 
  event_table <- reactive({
    olympics_data %>%
      filter(!is.na(medal),
             year == input$year,
             season == input$season,
             event == input$event) %>% 
      mutate(Team = gsub('-[0-9]+', '', team),
             Medal = factor(medal, levels = c("Gold", "Silver", "Bronze"))) %>%
      distinct(name,Team, Medal) %>% 
      arrange(Medal) %>% 
      rename( Name = name)
  })
  
  output$table <- renderDataTable({
    DT::datatable(event_table(), 
                  options = list(dom = 't',
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                   "}")
                  ),
                  caption = htmltools::tags$caption(
                    paste(input$year, input$season, input$event, 
                          "Olympics Medal Winners", sep = " "),
                    style = "color:white"))
  })
  
  olympic_location <- reactive({
    olympics_countries %>% 
      filter(year == input$year,
             season == input$season) %>% 
      distinct(city, country) %>%
      mutate(location = paste0(city, ", ", country)) %>% 
      pull(location)
  })
  
  olympic_teams <- reactive({
    olympics_data %>% 
      filter(year == input$year,
             season == input$season) %>% 
      mutate(team = gsub('-[0-9]+', '', team)) %>% 
      distinct(team) %>% 
      nrow()
  })
  
  output$intro <- renderText({ 
    paste0("The ", input$year, " ", input$season, 
           " Olympic Games were held in ", 
           olympic_location(), ".",
           " A total of ", olympic_teams(), " teams participated in the games."
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)