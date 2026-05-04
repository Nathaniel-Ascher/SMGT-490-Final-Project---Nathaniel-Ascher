library(shiny)
library(dplyr)
library(mgcv)
library(ggplot2)
library(plotly)
library(tidyr)

savant2025 <- sabRmetrics::download_baseballsavant(
  start_date = "2025-03-27",
  end_date = "2025-09-28")

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

batter_team_lookup <- savant2025 %>%
  mutate(batter_id_fullseason = as.character(batter_id),
         player_team = as.character(home_team)) %>%
  filter(!is.na(batter_id_fullseason),
         !is.na(player_team), inning_topbot == "Bot") %>%
  count(batter_id_fullseason, player_team, name = "team_rows") %>%
  group_by(batter_id_fullseason) %>%
  arrange(desc(team_rows), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(division_lookup, by = "player_team")

swing_desc <- c("swinging_strike", "swinging_strike_blocked", "foul", "foul_tip", "hit_into_play")

swing_context_data <- savant2025 %>%
  mutate(batter_id_fullseason = as.character(batter_id),
         swing_tilt_fullseason = as.numeric(swing_path_tilt),
         balls_fullseason = pmax(pmin(as.integer(balls), 3), 0),
         strikes_fullseason = pmax(pmin(as.integer(strikes), 2), 0),
         plate_x_fullseason = as.numeric(plate_x),
         plate_z_fullseason = as.numeric(plate_z),
         pitch_type_fullseason = as.character(pitch_type),
         pitch_group_fullseason = case_when(
           pitch_type_fullseason %in% c("FF", "SI", "FC") ~ "Fastball",
           pitch_type_fullseason %in% c("CH", "FS", "FO", "SC") ~ "Offspeed",
           pitch_type_fullseason %in% c("CU", "KC", "CS", "SL", "ST", "SV") ~ "Breaking",
           pitch_type_fullseason == "KN" ~ "Knuckleball",
           TRUE ~ NA_character_),
         pitch_group_fullseason = factor(pitch_group_fullseason)) %>%
  filter(description %in% swing_desc,
         !pitch_type_fullseason %in% c("PO", "UN", "FA", "EP"),
         !is.na(batter_id_fullseason),
         !is.na(swing_tilt_fullseason), is.finite(swing_tilt_fullseason),
         !is.na(balls_fullseason),
         !is.na(strikes_fullseason),
         !is.na(plate_x_fullseason), is.finite(plate_x_fullseason),
         !is.na(plate_z_fullseason), is.finite(plate_z_fullseason),
         !is.na(pitch_group_fullseason))

min_model_swings <- 200

player_tilt_prediction_relationship <- swing_context_data %>%
  group_by(batter_id_fullseason) %>%
  group_modify(~{
    batter_swing_data_fullseason <- .x
    
    if (nrow(batter_swing_data_fullseason) < min_model_swings) {
      return(tibble(
        swings_fullseason = nrow(batter_swing_data_fullseason),
        actual_predicted_tilt_correlation = NA_real_))}
    
    swing_context_model_fullseason <- tryCatch(
      gam(swing_tilt_fullseason ~
            pitch_group_fullseason +
            balls_fullseason +
            strikes_fullseason +
            s(plate_x_fullseason, k = 6) +
            s(plate_z_fullseason, k = 6),
          data = batter_swing_data_fullseason,
          method = "REML"),
      error = function(e) NULL)
    
    if (is.null(swing_context_model_fullseason)) {
      return(tibble(
        swings_fullseason = nrow(batter_swing_data_fullseason),
        actual_predicted_tilt_correlation = NA_real_))}
    
    predicted_swing_tilt_fullseason <- predict(
      swing_context_model_fullseason,
      newdata = batter_swing_data_fullseason,
      type = "response")
    
    tibble(swings_fullseason = nrow(batter_swing_data_fullseason),
           actual_predicted_tilt_correlation = cor(
             batter_swing_data_fullseason$swing_tilt_fullseason,
             predicted_swing_tilt_fullseason,
             use = "complete.obs"))}) %>%
  ungroup() %>%
  filter(!is.na(actual_predicted_tilt_correlation))

plate_appearance_endpoints_fullseason <- savant2025 %>%
  mutate(batter_id_fullseason = as.character(batter_id)) %>%
  filter(!is.na(batter_id_fullseason),
         !is.na(game_id),
         !is.na(at_bat_number),
         is.na(events) | events != "truncated_pa") %>%
  arrange(game_id, at_bat_number, pitch_number) %>%
  group_by(game_id, at_bat_number) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(strikeout_indicator_fullseason = if_else(events %in% c("strikeout", "strikeout_double_play"), 1, 0))

player_strikeout_rates_fullseason <- plate_appearance_endpoints_fullseason %>%
  group_by(batter_id_fullseason) %>%
  summarise(plate_appearances_fullseason = n(),
            strikeouts_fullseason = sum(strikeout_indicator_fullseason, na.rm = TRUE),
            strikeout_rate_fullseason = strikeouts_fullseason / plate_appearances_fullseason,
            .groups = "drop") %>%
  left_join(savant2025 %>%
              transmute(batter_id_fullseason = as.character(batter_id),
                        batter_name_fullseason = batter_name) %>%
              distinct(), by = "batter_id_fullseason") %>%
  select(batter_name_fullseason,
         batter_id_fullseason,
         plate_appearances_fullseason,
         strikeouts_fullseason,
         strikeout_rate_fullseason)

tilt_correlation_vs_strikeout_fullseason <- player_tilt_prediction_relationship %>%
  inner_join(player_strikeout_rates_fullseason, by = "batter_id_fullseason") %>%
  left_join(batter_team_lookup, by = "batter_id_fullseason") %>%
  filter(!is.na(batter_name_fullseason),
         !is.na(player_team),
         !is.na(division))

ui <- fluidPage(
  
  titlePanel("Relationship Between Swing Path Tilt and Strikeout Rate"),
  
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