install.packages("devtools")
devtools::install_github(repo = "saberpowers/sabRmetrics")
library(sabRmetrics)
library(dplyr)
library(mgcv)
library(purrr)
library(ggplot2)

setwd("~/Downloads")
savant2024 = read.csv("savant2024.csv")
savant2025 <- sabRmetrics::download_baseballsavant(start_date = "2025-03-27", end_date = "2025-09-28")

colnames(savant2024)

print(unique(savant2025$description))
print(unique(savant2025$home_team))

###Not including bunts:
swing_desc <- c("swinging_strike", "swinging_strike_blocked", "foul", "foul_tip","hit_into_play")

swings <- savant2025 %>%
  mutate(contact_depth = as.numeric(intercept_ball_minus_batter_pos_y_inches),
    swing_tilt = as.numeric(swing_path_tilt), balls = as.integer(balls),
    strikes = as.integer(strikes), plate_x = as.numeric(plate_x),
    plate_z = as.numeric(plate_z), pitch_type = as.character(pitch_type)) %>%
  filter(description %in% swing_desc, !is.na(batter_id), !is.na(contact_depth), is.finite(contact_depth), !is.na(swing_tilt), is.finite(swing_tilt))

###GAM #1: 
timing_variation_gam <- gam(swing_tilt ~ s(contact_depth, k = 20), data = swings, method = "REML")

###Validating the model:
gam.check(timing_variation_gam)

summary(timing_variation_gam)

plot(timing_variation_gam, shade = TRUE)

plot(fitted(timing_variation_gam),
     residuals(timing_variation_gam),
     pch = 16, cex = 0.3)
abline(h = 0, col = "red")

qqnorm(residuals(timing_variation_gam))
qqline(residuals(timing_variation_gam))
hist(residuals(timing_variation_gam), breaks = 50)

timing_variation_gam_plot <- swings %>%
  mutate(fhat = predict(timing_variation_gam, newdata = swings)) %>%
  arrange(contact_depth)

ggplot(timing_variation_gam_plot %>% sample_n(min(20000, nrow(timing_variation_gam_plot))),
       aes(contact_depth, swing_tilt)) +
  geom_point(alpha = 0.05) +
  geom_line(data = timing_variation_gam_plot %>% distinct(contact_depth, .keep_all = TRUE),
            aes(contact_depth, fhat),
            linewidth = 1.2) +
  labs(title = "Timing GAM: Swing Path Tilt vs Contact Depth",
    subtitle = "2025 MLB Season",
    x = "Contact Depth",
    y = "Swing Path Tilt") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

###GAM #2:

table(savant2025$pitch_type)

min_swings <- 200

batter_metrics <- swings %>%
  group_by(batter_id) %>%
  group_modify(~{
    df <- .x %>%
      filter(!pitch_type %in% c("PO", "UN", "FA", "EP")) %>%
      mutate(pitch_group = case_when(
          pitch_type %in% c("FF", "SI", "FC") ~ "Fastball",
          pitch_type %in% c("CH", "FS", "FO", "SC") ~ "Offspeed",
          pitch_type %in% c("CU", "KC", "CS", "SL", "ST", "SV") ~ "Breaking",
          pitch_type == "KN" ~ "Knuckleball",
          TRUE ~ NA_character_),
        pitch_group = factor(pitch_group),
        balls = pmax(pmin(balls, 3), 0),
        strikes = pmax(pmin(strikes, 2), 0)) %>%
      filter(!is.na(swing_tilt),
        !is.na(contact_depth),
        !is.na(plate_x),
        !is.na(plate_z),
        !is.na(pitch_group))
    
    if (nrow(df) < min_swings) {
      return(tibble(
        n_swings = nrow(df),
        edf_timing = NA_real_,
        edf_controls = NA_real_,
        r2_timing = NA_real_,
        r2_controls = NA_real_,
        resid_var_timing = NA_real_,
        resid_var_controls = NA_real_))}
    
    m_timing <- tryCatch(
      gam(swing_tilt ~ s(contact_depth, k = 10),
        data = df,
        method = "REML"),
      error = function(e) NULL)
    
    m_controls <- tryCatch(
      gam(swing_tilt ~
          s(contact_depth, k = 10) +
          pitch_group +
          balls + strikes +
          s(plate_x, k = 6) +
          s(plate_z, k = 6),
        data = df,
        method = "REML"),
      error = function(e) NULL)
    
    tibble(n_swings = nrow(df),
      edf_timing = if (is.null(m_timing)) NA_real_
      else summary(m_timing)$s.table[1, "edf"],
      edf_controls = if (is.null(m_controls)) NA_real_
      else summary(m_controls)$s.table[1, "edf"],
      r2_timing = if (is.null(m_timing)) NA_real_
      else summary(m_timing)$r.sq,
      r2_controls = if (is.null(m_controls)) NA_real_
      else summary(m_controls)$r.sq,
      resid_var_timing = if (is.null(m_timing)) NA_real_
      else var(residuals(m_timing), na.rm = TRUE),
      resid_var_controls = if (is.null(m_controls)) NA_real_
      else var(residuals(m_controls), na.rm = TRUE))
    
  }) %>%
  ungroup() %>%
  filter(!is.na(resid_var_timing),
    !is.na(resid_var_controls)) %>%
  mutate(var_reduction = 1 - (resid_var_controls/resid_var_timing))

summary(batter_metrics$resid_var_timing)
summary(batter_metrics$resid_var_controls)

mean(batter_metrics$resid_var_timing, na.rm = TRUE)
mean(batter_metrics$resid_var_controls, na.rm = TRUE)

mean(batter_metrics$var_reduction, na.rm = TRUE)
summary(batter_metrics$var_reduction)

summary(batter_metrics$edf_timing)
summary(batter_metrics$edf_controls)

summary(batter_metrics$r2_timing)
summary(batter_metrics$r2_controls)

ggplot(batter_metrics, aes(var_reduction)) +
  geom_histogram(bins = 30, fill = "darkblue") +
  labs(title = "Distribution of Residual Variance Reduction After Adding Pitch Context",
       subtitle = "2025 MLB Season, min. 200 Swings",
       x = "Variance Reduction",
       y = "# of Hitters") + theme_classic() + theme(plot.title = element_text(face = "bold"))

ggplot(batter_metrics, aes(r2_timing, r2_controls)) +
  geom_point(alpha = 0.6) + 
  labs(
    title = "Explained Variation in Swing Path Tilt",
    subtitle = "Timing Model (GAM #1) vs. MI/CC Model (GAM #2), 2025 MLB Season, min. 200 Swings",
    x = "Timing Model R²",
    y = "MI/CC Model R²") + theme_classic() + theme(plot.title = element_text(face = "bold"))

###Strikeout Rate:

print(unique(savant2025$events))

min_swings_fullseason <- 200
min_pa_fullseason <- 100

swing_context_data <- savant2025 %>%
  mutate(batter_id_fullseason = as.character(batter_id),
    swing_tilt_fullseason = as.numeric(swing_path_tilt),
    balls_fullseason = pmax(pmin(as.integer(balls), 3), 0),
    strikes_fullseason = pmax(pmin(as.integer(strikes), 2), 0),
    plate_x_fullseason = as.numeric(plate_x),
    plate_z_fullseason = as.numeric(plate_z),
    pitch_type_fullseason = as.character(pitch_type),
    pitch_group_fullseason = case_when(pitch_type_fullseason %in% c("FF", "SI", "FC") ~ "Fastball",
      pitch_type_fullseason %in% c("CH", "FS", "FO", "SC") ~ "Offspeed",
      pitch_type_fullseason %in% c("CU", "KC", "CS", "SL", "ST", "SV") ~ "Breaking",
      pitch_type_fullseason == "KN" ~ "Knuckleball",
      TRUE ~ NA_character_),
    pitch_group_fullseason = factor(pitch_group_fullseason)) %>%
  filter(description %in% c("swinging_strike", "swinging_strike_blocked", "foul", "foul_tip", "hit_into_play"),
    !pitch_type_fullseason %in% c("PO", "UN", "FA", "EP"),
    !is.na(batter_id_fullseason),
    !is.na(swing_tilt_fullseason), is.finite(swing_tilt_fullseason),
    !is.na(balls_fullseason),
    !is.na(strikes_fullseason),
    !is.na(plate_x_fullseason), is.finite(plate_x_fullseason),
    !is.na(plate_z_fullseason), is.finite(plate_z_fullseason),
    !is.na(pitch_group_fullseason))

player_tilt_prediction_relationship <- swing_context_data %>%
  group_by(batter_id_fullseason) %>%
  group_modify(~{
    batter_swing_data_fullseason <- .x
    
    if (nrow(batter_swing_data_fullseason) < min_swings_fullseason) {
      return(tibble(
        swings_fullseason = nrow(batter_swing_data_fullseason),
        actual_predicted_tilt_correlation = NA_real_))}
    
    swing_context_model_fullseason <- tryCatch(
      gam(swing_tilt_fullseason ~
          pitch_group_fullseason +
          balls_fullseason + strikes_fullseason +
          s(plate_x_fullseason, k = 6) +
          s(plate_z_fullseason, k = 6),
        data = batter_swing_data_fullseason,
        method = "REML"),
      error = function(e) NULL)
    
    if (is.null(swing_context_model_fullseason)) {
      return(tibble(swings_fullseason = nrow(batter_swing_data_fullseason),
        actual_predicted_tilt_correlation = NA_real_))}
    
    predicted_swing_tilt_fullseason <- predict(
      swing_context_model_fullseason,
      newdata = batter_swing_data_fullseason,
      type = "response")
    
    tibble(swings_fullseason = nrow(batter_swing_data_fullseason),
      actual_predicted_tilt_correlation = cor(batter_swing_data_fullseason$swing_tilt_fullseason,
        predicted_swing_tilt_fullseason,
        use = "complete.obs"))}) %>% ungroup() %>%
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
  filter(plate_appearances_fullseason >= min_pa_fullseason) %>%
  left_join(savant2025 %>%
      transmute(batter_id_fullseason = as.character(batter_id),
        batter_name_fullseason = batter_name) %>%
      distinct(), by = "batter_id_fullseason") %>%
  select(batter_name_fullseason, batter_id_fullseason, plate_appearances_fullseason, strikeouts_fullseason, strikeout_rate_fullseason)

tilt_correlation_vs_strikeout_fullseason <- player_tilt_prediction_relationship %>%
  inner_join(player_strikeout_rates_fullseason,
    by = "batter_id_fullseason")

overall_relationship_fullseason <- cor(tilt_correlation_vs_strikeout_fullseason$actual_predicted_tilt_correlation,
  tilt_correlation_vs_strikeout_fullseason$strikeout_rate_fullseason, use = "complete.obs")

print(overall_relationship_fullseason)

strikeout_relationship_model_fullseason <- lm(strikeout_rate_fullseason ~ actual_predicted_tilt_correlation,
  data = tilt_correlation_vs_strikeout_fullseason)

summary(strikeout_relationship_model_fullseason)

ggplot(tilt_correlation_vs_strikeout_fullseason,
  aes(x = actual_predicted_tilt_correlation, y = strikeout_rate_fullseason)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Correlation Between Context-Predicted and Observed Swing Path Tilt vs. Strikeout Rate",
    subtitle = paste0("2025 MLB Season | Min. ", min_swings_fullseason, " swings, Min. ",
      min_pa_fullseason, " PA | r = ", round(overall_relationship_fullseason, 3)),
    x = "Correlation Between Predicted and Observed Swing Path Tilt",
    y = "Strikeout Rate") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))


###Using swing tilt to predict batter performance:
install.packages(c("lme4", "lmerTest"))

library(lme4)
library(lmerTest)

woba_model_data <- savant2025 %>% mutate(batter_id = as.factor(batter_id),
    pitcher_id = as.factor(pitcher_id),
    contact_depth = as.numeric(intercept_ball_minus_batter_pos_y_inches),
    swing_tilt = as.numeric(swing_path_tilt),
    balls = as.integer(balls),
    strikes = as.integer(strikes),
    plate_x = as.numeric(plate_x),
    plate_z = as.numeric(plate_z),
    pitch_type = as.character(pitch_type),
    stand = as.factor(bat_side),
    p_throws = as.factor(pitch_hand),
    woba_value = as.numeric(woba_value)) %>%
  filter(description %in% swing_desc,
    !pitch_type %in% c("PO", "UN", "FA", "EP"),
    !is.na(batter_id),
    !is.na(pitcher_id),
    !is.na(contact_depth), is.finite(contact_depth),
    !is.na(swing_tilt), is.finite(swing_tilt),
    !is.na(woba_value), is.finite(woba_value)) %>%
  mutate(pitch_group = case_when(
      pitch_type %in% c("FF", "SI", "FC") ~ "Fastball",
      pitch_type %in% c("CH", "FS", "FO", "SC") ~ "Offspeed",
      pitch_type %in% c("CU", "KC", "CS", "SL", "ST", "SV") ~ "Breaking",
      pitch_type == "KN" ~ "Knuckleball",
      TRUE ~ NA_character_),
    pitch_group = factor(pitch_group),
    balls = pmax(pmin(balls, 3), 0),
    strikes = pmax(pmin(strikes, 2), 0))

table(woba_model_data$woba_value, useNA = "ifany")
summary(woba_model_data$woba_value)
nrow(woba_model_data)

###First model:
woba_simple_model <- lmer(woba_value ~ swing_tilt + (1 | batter_id) + (1 | pitcher_id),
  data = woba_model_data, REML = FALSE)

summary(woba_simple_model)

###Second model:
woba_controls_model <- lmer(woba_value ~ swing_tilt + contact_depth + pitch_group +
    balls + strikes +
    plate_x + plate_z +
    stand + p_throws +
    (1 | batter_id) +
    (1 | pitcher_id), data = woba_model_data %>%
    filter(!is.na(contact_depth),
      is.finite(contact_depth),
      !is.na(plate_x),
      is.finite(plate_x),
      !is.na(plate_z),
      is.finite(plate_z),
      !is.na(pitch_group)), REML = FALSE)

summary(woba_controls_model)

###Third model:
woba_slope_model <- lmer(woba_value ~ swing_tilt + contact_depth + pitch_group +
    balls + strikes +
    plate_x + plate_z +
    stand + p_throws +
    (1 + swing_tilt | batter_id) +
    (1 | pitcher_id),
  data = woba_model_data %>%
    filter(!is.na(contact_depth),
      is.finite(contact_depth),
      !is.na(plate_x),
      is.finite(plate_x),
      !is.na(plate_z),
      is.finite(plate_z),
      !is.na(pitch_group)), REML = FALSE, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

summary(woba_slope_model)

as.data.frame(VarCorr(woba_slope_model))

###Swing Path Tilt vs. Streakiness:
swing_variability_data <- savant2025 %>%
  mutate(game_date = as.Date(game_date),
    batter_id = as.factor(batter_id),
    swing_tilt = as.numeric(swing_path_tilt),
    pitch_type = as.character(pitch_type)) %>%
  filter(description %in% swing_desc,
    !pitch_type %in% c("PO", "UN", "FA", "EP"),
    !is.na(batter_id),
    !is.na(game_date),
    !is.na(swing_tilt),
    is.finite(swing_tilt))

offense_game_data <- savant2025 %>%
  mutate(game_date = as.Date(game_date),
    batter_id = as.factor(batter_id),
    woba_value = as.numeric(woba_value)) %>%
  filter(!is.na(batter_id),
    !is.na(game_date),
    !is.na(woba_value),
    is.finite(woba_value)) %>%
  group_by(batter_id, game_date) %>%
  summarise(game_woba_sum = sum(woba_value, na.rm = TRUE),
    game_pa_with_woba = n(),
    .groups = "drop") %>%
  arrange(batter_id, game_date)

offense_10game_windows <- offense_game_data %>%
  group_by(batter_id) %>%
  arrange(game_date, .by_group = TRUE) %>%
  mutate(game_number = row_number(),
    window_10 = ((game_number - 1) %/% 10) + 1) %>%
  group_by(batter_id, window_10) %>%
  summarise(start_date = min(game_date),
    end_date = max(game_date),
    n_games_window = n(),
    pa_window = sum(game_pa_with_woba, na.rm = TRUE),
    woba_sum_window = sum(game_woba_sum, na.rm = TRUE),
    woba_mean_window = woba_sum_window / pa_window,
    .groups = "drop") %>%
  filter(n_games_window == 10,
    pa_window >= 15)

hitter_tilt_variability <- swing_variability_data %>%
  group_by(batter_id) %>%
  summarise(n_swings = n(),
    mean_swing_tilt = mean(swing_tilt, na.rm = TRUE),
    sd_swing_tilt = sd(swing_tilt, na.rm = TRUE),
    cv_swing_tilt = sd_swing_tilt / abs(mean_swing_tilt),
    .groups = "drop")

hitter_10game_streakiness <- offense_10game_windows %>%
  group_by(batter_id) %>%
  summarise(n_windows_10 = n(),
    mean_window_woba = mean(woba_mean_window, na.rm = TRUE),
    sd_window_woba = sd(woba_mean_window, na.rm = TRUE),
    iqr_window_woba = IQR(woba_mean_window, na.rm = TRUE),
    cv_window_woba = sd_window_woba / abs(mean_window_woba),
    mean_pa_window = mean(pa_window, na.rm = TRUE),
    .groups = "drop")

streak10_data <- hitter_tilt_variability %>%
  inner_join(hitter_10game_streakiness, by = "batter_id") %>%
  filter(n_swings >= 200,
    n_windows_10 >= 3,
    !is.na(sd_swing_tilt), is.finite(sd_swing_tilt),
    !is.na(sd_window_woba), is.finite(sd_window_woba),
    !is.na(cv_swing_tilt), is.finite(cv_swing_tilt),
    !is.na(cv_window_woba), is.finite(cv_window_woba))

nrow(streak10_data)
summary(streak10_data$sd_swing_tilt)
summary(streak10_data$sd_window_woba)
summary(streak10_data$n_windows_10)

streak_simple_model <- lm(sd_window_woba ~ sd_swing_tilt, data = streak10_data)

summary(streak_simple_model)

streak_second_model <- lm(sd_window_woba ~ sd_swing_tilt + mean_window_woba + n_windows_10 + n_swings,
  data = streak10_data)

summary(streak_second_model)

streak_third_model <- lm(cv_window_woba ~ cv_swing_tilt + mean_window_woba + n_windows_10 + n_swings,
  data = streak10_data)

summary(streak_third_model)

ggplot(streak10_data, aes(x = sd_swing_tilt, y = sd_window_woba)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Swing Tilt Variability vs 10-Game Offensive Streakiness",
    subtitle = "10-game non-rolling windows, min. 15 PAs in window, 2025 MLB Season",
    x = "Swing Tilt Variability (SD)",
    y = "10-Game Window Offensive Streakiness (SD of wOBA)") +
  theme_classic() + theme(plot.title = element_text(face = "bold"))


###Web App Code to get RDS:
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

saveRDS(tilt_correlation_vs_strikeout_fullseason, "tilt_correlation_vs_strikeout_fullseason.rds")