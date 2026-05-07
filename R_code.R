# ============================================================
# 1. Promising young players
# 2. Spanish team attack vs defence
# 3. Sevilla possession bands and match outcomes
# ============================================================

library(tidyverse)
library(lubridate)
library(plotly)
library(scales)
library(htmlwidgets)

# ------------------------------------------------------------
# 0. Setup and load data
# ------------------------------------------------------------

# Change this path if needed.
data_dir <- "Data"
# If you run this script from another folder, change data_dir to the folder containing the CSV files.

read_first_existing <- function(paths) {
  existing_paths <- paths[file.exists(paths)]
  
  if (length(existing_paths) == 0) {
    stop(
      "None of these files were found:\n",
      paste(paths, collapse = "\n")
    )
  }
  
  readr::read_csv(existing_paths[1], show_col_types = FALSE)
}

# Helper for visible bubble sizes in Plotly.
# This avoids the problem where highlighted labels appear but the bubble size is not clear.
rescale_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep(0.5, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

matches_raw <- readr::read_csv(file.path(data_dir, "Match.csv"), show_col_types = FALSE)
teams_raw <- readr::read_csv(file.path(data_dir, "Team.csv"), show_col_types = FALSE)
players_raw <- readr::read_csv(file.path(data_dir, "Player.csv"), show_col_types = FALSE)
player_attr_raw <- readr::read_csv(file.path(data_dir, "Player_Attributes.csv"), show_col_types = FALSE)
leagues_raw <- readr::read_csv(file.path(data_dir, "League.csv"), show_col_types = FALSE)

goals_raw <- read_first_existing(c(
  file.path(data_dir, "Match_Goals.csv"),
  file.path(data_dir, "Match_Goal.csv")
))

possession_raw <- read_first_existing(c(
  file.path(data_dir, "Match_Possesion.csv"),
  file.path(data_dir, "Match_Possession.csv")
))

# Shared visual style to keep all plots consistent.
# The same font, background, grid, reference-line style and highlight logic are used
# across all three visualisations.
base_font <- list(family = "Arial", size = 14, color = "#2F2F2F")
plot_bg <- "#FFFFFF"
grid_col <- "#E8E8E8"
ref_line_col <- "#303030"

# Restrained project palette:
# - pale blue/grey = context
# - red/blue/grey = highlighted football cases
# - green/orange/red = semantic result colours in the stacked bar chart
col_other <- "rgba(113, 152, 190, 0.26)"
col_other_line <- "rgba(85, 120, 155, 0.45)"
col_top <- "rgba(210, 50, 55, 0.88)"
col_sevilla <- "#D71920"
col_real <- "#00529F"
col_barca <- "#00529F"
col_other_team <- "rgba(155, 155, 155, 0.48)"

# Reusable Plotly layout settings.
common_layout <- list(
  font = base_font,
  paper_bgcolor = plot_bg,
  plot_bgcolor = plot_bg,
  hoverlabel = list(font = list(family = "Arial", size = 12)),
  showlegend = FALSE
)

axis_style <- function(title) {
  list(
    title = title,
    showgrid = TRUE,
    gridcolor = grid_col,
    zeroline = FALSE,
    linecolor = "#BDBDBD",
    tickfont = list(size = 12),
    titlefont = list(size = 14)
  )
}

# Add team names to match table.
team_names <- teams_raw %>%
  select(team_api_id, team_long_name)

matches_named <- matches_raw %>%
  left_join(
    team_names %>%
      rename(
        home_team_api_id = team_api_id,
        home_team_name = team_long_name
      ),
    by = "home_team_api_id"
  ) %>%
  left_join(
    team_names %>%
      rename(
        away_team_api_id = team_api_id,
        away_team_name = team_long_name
      ),
    by = "away_team_api_id"
  )

# ============================================================
# VISUALISATION 1: Promising young players
# ============================================================
# x-axis      = age in 2016
# y-axis      = potential rating, on the dataset scale 0 to 100
# bubble size = goal_contributions = goals + assists in 2015/2016
# age range   = 20 to 23
# dashed line = potential threshold 85
# highlighted = top 5 players, within age 20-23 and potential >= 85,
#               ranked by goal_contributions


reference_date <- lubridate::ymd("2016-07-01")

latest_player_attr <- player_attr_raw %>%
  mutate(
    player_api_id = as.numeric(player_api_id),
    attr_date = lubridate::parse_date_time(
      date,
      orders = c("ymd HMS", "ymd HM", "ymd")
    )
  ) %>%
  filter(!is.na(attr_date), attr_date <= reference_date) %>%
  arrange(player_api_id, desc(attr_date)) %>%
  group_by(player_api_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    player_api_id,
    attr_date,
    overall_rating,
    potential
  )

valid_goal_events <- goals_raw %>%
  mutate(
    match_id = as.numeric(match_id),
    player1 = as.numeric(player1),
    player2 = as.numeric(player2)
  ) %>%
  left_join(
    matches_raw %>%
      mutate(id = as.numeric(id)) %>%
      select(id, season),
    by = c("match_id" = "id")
  ) %>%
  filter(season == "2015/2016") %>%
  filter(is.na(goal_type) | !goal_type %in% c("o", "npm", "psm"))

player_goals <- valid_goal_events %>%
  filter(!is.na(player1)) %>%
  count(player_api_id = player1, name = "goals")

player_assists <- valid_goal_events %>%
  filter(!is.na(player2)) %>%
  count(player_api_id = player2, name = "assists")

player_contributions <- full_join(
  player_goals,
  player_assists,
  by = "player_api_id"
) %>%
  mutate(
    goals = replace_na(goals, 0L),
    assists = replace_na(assists, 0L),
    goal_contributions = goals + assists
  )

young_players_all <- players_raw %>%
  mutate(
    player_api_id = as.numeric(player_api_id),
    birthday_date = lubridate::parse_date_time(
      birthday,
      orders = c("ymd HMS", "ymd HM", "ymd")
    ),
    age_2016 = as.numeric(
      lubridate::interval(birthday_date, reference_date) / lubridate::years(1)
    )
  ) %>%
  left_join(latest_player_attr, by = "player_api_id") %>%
  left_join(player_contributions, by = "player_api_id") %>%
  mutate(
    goals = replace_na(goals, 0L),
    assists = replace_na(assists, 0L),
    goal_contributions = replace_na(goal_contributions, 0L)
  ) %>%
  filter(
    !is.na(age_2016),
    !is.na(potential),
    age_2016 >= 20,
    age_2016 <= 23
  )

# Top 5 highlighted players:
# age 20-23, potential >= 85, highest goals + assists in 2015/2016.
top5_young_players <- young_players_all %>%
  filter(potential >= 85) %>%
  arrange(desc(goal_contributions), desc(potential), desc(overall_rating)) %>%
  slice_head(n = 5) %>%
  mutate(
    top5_label = player_name,
    # Pixel offsets used in Plotly annotations. This is more reliable than
    # textposition when several highlighted players are close together.
    label_ax = case_when(
      player_name == "Harry Kane" ~ 45,
      player_name == "Ross Barkley" ~ -45,
      player_name == "Paulo Dybala" ~ -55,
      player_name == "Francisco AlcC!cer" ~ 55,
      player_name == "Francisco Alcacer" ~ 55,
      TRUE ~ 0
    ),
    label_ay = case_when(
      player_name == "Harry Kane" ~ -32,
      player_name == "Ross Barkley" ~ -42,
      player_name == "Paulo Dybala" ~ 28,
      player_name == "Francisco AlcC!cer" ~ 34,
      player_name == "Francisco Alcacer" ~ 34,
      TRUE ~ -34
    )
  )

young_players <- young_players_all %>%
  mutate(
    highlight_group = if_else(
      player_api_id %in% top5_young_players$player_api_id,
      "Top 5: potential b	% 85 and highest goals + assists",
      "Other players aged 20b"
      ))