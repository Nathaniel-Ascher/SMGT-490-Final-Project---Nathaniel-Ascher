library(shiny)
library(dplyr)
library(mgcv)
library(ggplot2)
library(plotly)
library(tidyr)

division_lookup <- tibble(player_team = c("CWS", "KC", "DET", "MIN", "CLE",
  "HOU", "ATH", "TEX", "LAA", "SEA",
  "NYY", "TOR", "BOS", "BAL", "TB",
  "MIL", "CIN", "STL", "PIT", "CHC",
  "LAD", "SF", "SD", "COL", "AZ",
  "PHI", "MIA", "NYM", "WSH", "ATL"),
  division = c(rep("AL Central", 5),
               rep("AL West", 5),
               rep("AL East", 5),
               rep("NL Central", 5),
               rep("NL West", 5),
               rep("NL East", 5)))

all_divisions <- c("AL Central", "AL West", "AL East", "NL Central", "NL West", "NL East")

all_teams <- division_lookup$player_team

tilt_correlation_vs_strikeout_fullseason <- readRDS("tilt_correlation_vs_strikeout_fullseason.rds")

ui <- fluidPage(
  
  titlePanel("Relationship Between Swing Path Tilt and Strikeout Rate - Nathaniel Ascher SMGT 490 Final Project"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "min_swings",
        label = "Minimum Swings",
        min = 200,
        max = max(tilt_correlation_vs_strikeout_fullseason$swings_fullseason, na.rm = TRUE),
        value = 200,
        step = 25),
      
      sliderInput(inputId = "min_pa",
        label = "Minimum Plate Appearances",
        min = 100,
        max = max(tilt_correlation_vs_strikeout_fullseason$plate_appearances_fullseason, na.rm = TRUE),
        value = 100,
        step = 25),
      
      hr(),
      
      h4("Divisions"),
      
      checkboxGroupInput(inputId = "selected_divisions",
        label = "Select Divisions",
        choices = all_divisions,
        selected = all_divisions),
      
      hr(),
      
      h4("Teams"),
      
      checkboxGroupInput(inputId = "selected_teams",
        label = "Select Teams",
        choices = all_teams,
        selected = all_teams)),
    
    mainPanel(plotlyOutput("tilt_strikeout_plot", height = "650px"),
              div(strong("Predicted Swing Path Tilt comes from a GAM that uses pitch group, pitch location, and count as predictors."),
                style = "font-size: 14px; margin-top: 8px; color: #444;"))))

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    tilt_correlation_vs_strikeout_fullseason %>%
      filter(swings_fullseason >= input$min_swings,
        plate_appearances_fullseason >= input$min_pa,
        division %in% input$selected_divisions,
        player_team %in% input$selected_teams)})
  
  output$tilt_strikeout_plot <- renderPlotly({
    
    df <- filtered_data()
    
    validate(need(nrow(df) > 1, "Not enough players match these filters."))
    
    p <- ggplot(
      df,
      aes(x = actual_predicted_tilt_correlation,
        y = strikeout_rate_fullseason,
        text = paste0(
          batter_name_fullseason,
          "<br>Team: ", player_team,
          "<br>Division: ", division,
          "<br>Strikeout Rate: ", round(strikeout_rate_fullseason * 100, 1), "%",
          "<br>Tilt Correlation: ", round(actual_predicted_tilt_correlation, 3)))) +
      geom_point(alpha = 0.75, size = 2.5, color = "blue") +
      labs(x = "Correlation Between Predicted and Observed Swing Path Tilt",
        y = "Strikeout Rate") +
      theme_classic()
    
    ggplotly(p, tooltip = "text") %>%
      layout(title = list(
          text = paste0("<b>Correlation Between Context-Predicted and Observed Swing Path Tilt vs. Strikeout Rate</b>",
            "<br>",
            "<span style='font-size:13px;'>2025 MLB Season | Min. 200 swings, Min. 100 PA | Overall Correlation: r = -0.105</span>"),
          font = list(size = 15)),
        margin = list(t = 120, r = 40, b = 70, l = 70),
        hoverlabel = list(align = "left"))})}

shinyApp(ui = ui, server = server)