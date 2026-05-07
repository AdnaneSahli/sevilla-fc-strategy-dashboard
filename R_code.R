# Interactive visualisations

library(tidyverse)
library(lubridate)
library(plotly)
library(scales)
library(htmlwidgets)

# Setup

data_dir <- "Data"

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

# Shared style
base_font <- list(family = "Arial", size = 14, color = "#2F2F2F")
plot_bg <- "#FFFFFF"
grid_col <- "#E8E8E8"
ref_line_col <- "#303030"

col_other <- "rgba(113, 152, 190, 0.26)"
col_other_line <- "rgba(85, 120, 155, 0.45)"
col_top <- "rgba(210, 50, 55, 0.88)"
col_sevilla <- "#D71920"
col_real <- "#00529F"
col_barca <- "#00529F"
col_other_team <- "rgba(155, 155, 155, 0.48)"

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

# Team names
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

# Visualisation 1: Promising young players

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

top5_young_players <- young_players_all %>%
  filter(potential >= 85) %>%
  arrange(desc(goal_contributions), desc(potential), desc(overall_rating)) %>%
  slice_head(n = 5) %>%
  mutate(
    top5_label = player_name,
    label_ax = case_when(
      player_name == "Harry Kane" ~ 45,
      player_name == "Ross Barkley" ~ -45,
      player_name == "Paulo Dybala" ~ -55,
      player_name == "Francisco Alcácer" ~ 55,
      player_name == "Francisco Alcacer" ~ 55,
      TRUE ~ 0
    ),
    label_ay = case_when(
      player_name == "Harry Kane" ~ -32,
      player_name == "Ross Barkley" ~ -42,
      player_name == "Paulo Dybala" ~ 28,
      player_name == "Francisco Alcácer" ~ 34,
      player_name == "Francisco Alcacer" ~ 34,
      TRUE ~ -34
    )
  )

young_players <- young_players_all %>%
  mutate(
    highlight_group = if_else(
      player_api_id %in% top5_young_players$player_api_id,
      "Top 5: potential ≥ 85 and highest goals + assists",
      "Other players aged 20–23"
    ),
    highlight_group = factor(
      highlight_group,
      levels = c(
        "Other players aged 20–23",
        "Top 5: potential ≥ 85 and highest goals + assists"
      )
    ),
    bubble_size = 6 + 34 * sqrt(rescale_01(goal_contributions)),
    label_text = if_else(
      player_api_id %in% top5_young_players$player_api_id,
      player_name,
      ""
    ),
    tooltip = paste0(
      "Player: ", player_name,
      "<br>Age in 2016: ", round(age_2016, 1),
      "<br>Potential rating: ", potential, " / 100",
      "<br>Overall rating: ", overall_rating, " / 100",
      "<br>Goals 2015/2016: ", goals,
      "<br>Assists 2015/2016: ", assists,
      "<br>Goal contributions: ", goal_contributions,
      "<br>Group: ", highlight_group
    )
  )

cat("Top 5 highlighted young players:\n")
print(
  top5_young_players %>%
    select(player_name, age_2016, potential, overall_rating, goals, assists, goal_contributions)
)

player_colors <- c(
  "Other players aged 20–23" = col_other,
  "Top 5: potential ≥ 85 and highest goals + assists" = col_top
)

interactive_young_players <- plot_ly()

for (grp in levels(young_players$highlight_group)) {
  dat <- young_players %>% filter(highlight_group == grp)
  interactive_young_players <- interactive_young_players %>%
    add_markers(
      data = dat,
      x = ~age_2016,
      y = ~potential,
      text = ~tooltip,
      hoverinfo = "text",
      name = grp,
      showlegend = FALSE,
      marker = list(
        size = ~bubble_size,
        sizemode = "diameter",
        opacity = ifelse(grepl("Top 5", grp), 0.90, 0.50),
        color = player_colors[[grp]],
        line = list(
          color = ifelse(grepl("Top 5", grp), "#111111", col_other_line),
          width = ifelse(grepl("Top 5", grp), 1.3, 0.5)
        )
      )
    )
}

young_player_label_annotations <- lapply(seq_len(nrow(top5_young_players)), function(i) {
  list(
    x = top5_young_players$age_2016[i],
    y = top5_young_players$potential[i],
    text = top5_young_players$top5_label[i],
    showarrow = TRUE,
    ax = top5_young_players$label_ax[i],
    ay = top5_young_players$label_ay[i],
    arrowhead = 0,
    arrowwidth = 0.8,
    arrowcolor = "rgba(60,60,60,0.65)",
    bgcolor = "rgba(255,255,255,0.70)",
    borderpad = 2,
    font = list(size = 11, color = "#111111")
  )
})

interactive_young_players <- interactive_young_players %>%
  layout(
    title = list(
      text = paste0(
        "Promising young players",
        "<br><sup>",
        "Age 20–23 | Potential rating ≥ 85 reference line | Bubble size = goals + assists in 2015/2016",
        "</sup>"
      ),
      x = 0.5,
      xanchor = "center"
    ),
    font = common_layout$font,
    paper_bgcolor = common_layout$paper_bgcolor,
    plot_bgcolor = common_layout$plot_bgcolor,
    hoverlabel = common_layout$hoverlabel,
    xaxis = c(
      axis_style("Age in 2016 (years)"),
      list(
        range = c(19.95, 23.05),
        tickmode = "array",
        tickvals = c(20, 21, 22, 23),
        ticktext = c("20", "21", "22", "23")
      )
    ),
    yaxis = c(
      axis_style("Potential rating (0–100)"),
      list(
        range = c(
          max(0, floor(min(young_players$potential, na.rm = TRUE) - 2)),
          min(100, ceiling(max(young_players$potential, na.rm = TRUE) + 2))
        )
      )
    ),
    shapes = list(
      list(
        type = "line",
        x0 = 20,
        x1 = 23,
        y0 = 85,
        y1 = 85,
        line = list(dash = "dash", width = 1.8, color = ref_line_col)
      )
    ),
    annotations = c(
      list(
        list(
          x = 21.35,
          y = 85,
          text = "Potential threshold = 85",
          showarrow = FALSE,
          xanchor = "center",
          yanchor = "bottom",
          bgcolor = "rgba(255,255,255,0.70)",
          borderpad = 2,
          font = list(size = 12, color = "#333333")
        )
      ),
      young_player_label_annotations
    ),
    showlegend = FALSE,
    margin = list(t = 105, r = 45, b = 75, l = 85)
  )

interactive_young_players

# Visualisation 2: Spanish team attack vs defence

spain_league_id <- leagues_raw %>%
  filter(name == "Spain LIGA BBVA") %>%
  pull(id)

spanish_team_matches <- bind_rows(
  matches_raw %>%
    filter(league_id == spain_league_id) %>%
    transmute(
      match_id = id,
      season,
      team_api_id = home_team_api_id,
      goals_scored = home_team_goal,
      goals_conceded = away_team_goal,
      points = case_when(
        home_team_goal > away_team_goal ~ 3,
        home_team_goal == away_team_goal ~ 1,
        TRUE ~ 0
      ),
      venue = "Home"
    ),
  matches_raw %>%
    filter(league_id == spain_league_id) %>%
    transmute(
      match_id = id,
      season,
      team_api_id = away_team_api_id,
      goals_scored = away_team_goal,
      goals_conceded = home_team_goal,
      points = case_when(
        away_team_goal > home_team_goal ~ 3,
        away_team_goal == home_team_goal ~ 1,
        TRUE ~ 0
      ),
      venue = "Away"
    )
)

spanish_team_summary <- spanish_team_matches %>%
  group_by(team_api_id) %>%
  summarise(
    matches_played = n(),
    avg_goals_scored = mean(goals_scored, na.rm = TRUE),
    avg_goals_conceded = mean(goals_conceded, na.rm = TRUE),
    total_goals_scored = sum(goals_scored, na.rm = TRUE),
    total_goals_conceded = sum(goals_conceded, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    avg_points_per_game = mean(points, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    teams_raw %>%
      select(team_api_id, team_long_name),
    by = "team_api_id"
  ) %>%
  mutate(
    highlight_group = case_when(
      team_long_name == "Sevilla FC" ~ "Sevilla FC",
      team_long_name == "Real Madrid CF" ~ "Real Madrid CF",
      team_long_name == "FC Barcelona" ~ "FC Barcelona",
      TRUE ~ "Other Spanish teams"
    ),
    highlight_group = factor(
      highlight_group,
      levels = c(
        "Other Spanish teams",
        "Sevilla FC",
        "Real Madrid CF",
        "FC Barcelona"
      )
    ),
    label_text = if_else(
      highlight_group %in% c("Sevilla FC", "Real Madrid CF", "FC Barcelona"),
      team_long_name,
      ""
    ),
    bubble_size = 10 + 26 * sqrt(rescale_01(avg_points_per_game)),
    tooltip = paste0(
      "Team: ", team_long_name,
      "<br>Matches: ", matches_played,
      "<br>Total points: ", total_points,
      "<br>Average points per game: ", round(avg_points_per_game, 2),
      "<br>Avg goals scored per match: ", round(avg_goals_scored, 2),
      "<br>Avg goals conceded per match: ", round(avg_goals_conceded, 2),
      "<br>Total goals scored: ", total_goals_scored,
      "<br>Total goals conceded: ", total_goals_conceded,
      "<br>Bubble size: average points per game"
    )
  )

overall_avg_scored <- mean(spanish_team_summary$avg_goals_scored, na.rm = TRUE)
overall_avg_conceded <- mean(spanish_team_summary$avg_goals_conceded, na.rm = TRUE)

team_colors <- c(
  "Sevilla FC" = col_sevilla,
  "Real Madrid CF" = col_real,
  "FC Barcelona" = col_barca,
  "Other Spanish teams" = col_other_team
)

x_min <- min(spanish_team_summary$avg_goals_scored, na.rm = TRUE)
x_max <- max(spanish_team_summary$avg_goals_scored, na.rm = TRUE)
y_min <- min(spanish_team_summary$avg_goals_conceded, na.rm = TRUE)
y_max <- max(spanish_team_summary$avg_goals_conceded, na.rm = TRUE)
x_mid <- (x_min + x_max) / 2
y_mid <- (y_min + y_max) / 2

interactive_spanish_teams <- plot_ly()

for (grp in levels(spanish_team_summary$highlight_group)) {
  dat <- spanish_team_summary %>%
    filter(highlight_group == grp)
  
  interactive_spanish_teams <- interactive_spanish_teams %>%
    add_markers(
      data = dat,
      x = ~avg_goals_scored,
      y = ~avg_goals_conceded,
      hovertext = ~tooltip,
      hoverinfo = "text",
      name = grp,
      showlegend = FALSE,
      marker = list(
        size = dat$bubble_size,
        sizemode = "diameter",
        opacity = ifelse(grp == "Other Spanish teams", 0.60, 0.90),
        color = team_colors[[as.character(grp)]],
        line = list(
          color = "#333333",
          width = 1
        )
      )
    )
}

highlighted_team_labels <- spanish_team_summary %>%
  filter(highlight_group %in% c("Sevilla FC", "Real Madrid CF", "FC Barcelona"))

team_label_annotations <- lapply(seq_len(nrow(highlighted_team_labels)), function(i) {
  team_nm <- highlighted_team_labels$team_long_name[i]
  list(
    x = highlighted_team_labels$avg_goals_scored[i],
    y = highlighted_team_labels$avg_goals_conceded[i],
    text = team_nm,
    showarrow = TRUE,
    ax = case_when(
      team_nm == "Sevilla FC" ~ -36,
      team_nm == "Real Madrid CF" ~ 54,
      team_nm == "FC Barcelona" ~ 60,
      TRUE ~ 0
    ),
    ay = case_when(
      team_nm == "Sevilla FC" ~ -26,
      team_nm == "Real Madrid CF" ~ -8,
      team_nm == "FC Barcelona" ~ 18,
      TRUE ~ -20
    ),
    arrowhead = 0,
    arrowwidth = 0.8,
    arrowcolor = "rgba(60,60,60,0.65)",
    bgcolor = "rgba(255,255,255,0.72)",
    borderpad = 2,
    font = list(size = 12, color = team_colors[[team_nm]])
  )
})

interactive_spanish_teams <- interactive_spanish_teams %>%
  layout(
    title = list(
      text = paste0(
        "Sevilla-Benchmarking vs LA LIGA(2008/09-2015/16)",
        "<br><sup>",
        "Dashed lines show Spanish-team averages | Bubble size = average points per game",
        "</sup>"
      ),
      x = 0.5,
      xanchor = "center"
    ),
    font = common_layout$font,
    paper_bgcolor = common_layout$paper_bgcolor,
    plot_bgcolor = common_layout$plot_bgcolor,
    hoverlabel = common_layout$hoverlabel,
    xaxis = axis_style("Average goals scored per match (attack)"),
    yaxis = axis_style("Average goals conceded per match (defence)"),
    showlegend = FALSE,
    shapes = list(
      list(
        type = "line",
        x0 = overall_avg_scored,
        x1 = overall_avg_scored,
        y0 = y_min,
        y1 = y_max,
        line = list(dash = "dash", width = 1.8, color = ref_line_col)
      ),
      list(
        type = "line",
        x0 = x_min,
        x1 = x_max,
        y0 = overall_avg_conceded,
        y1 = overall_avg_conceded,
        line = list(dash = "dash", width = 1.8, color = ref_line_col)
      )
    ),
    annotations = c(
      list(
        list(
          x = overall_avg_scored,
          y = y_min,
          text = paste0("Avg scored = ", round(overall_avg_scored, 2)),
          showarrow = FALSE,
          textangle = 0,
          xanchor = "center",
          yanchor = "top",
          yshift = -8,
          font = list(size = 12, color = "#333333")
        ),
        list(
          x = x_min,
          y = overall_avg_conceded,
          text = paste0("Avg conceded = ", round(overall_avg_conceded, 2)),
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "bottom",
          font = list(size = 12, color = "#333333")
        )
      ),
      team_label_annotations
    ),
    margin = list(t = 95, r = 75, b = 90, l = 90)
  )

interactive_spanish_teams

# Visualisation 3: Sevilla results by possession band

# Sevilla matches

sevilla_match <- matches_named %>%
  filter(
    home_team_name == "Sevilla FC" |
      away_team_name == "Sevilla FC"
  ) %>%
  mutate(
    is_home = home_team_name == "Sevilla FC",
    goals_scored = if_else(is_home, home_team_goal, away_team_goal),
    goals_conceded = if_else(is_home, away_team_goal, home_team_goal),
    result = case_when(
      goals_scored > goals_conceded ~ "Win",
      goals_scored == goals_conceded ~ "Draw",
      goals_scored < goals_conceded ~ "Loss",
      TRUE ~ "Unknown"
    ),
    result = factor(result, levels = c("Loss", "Draw", "Win")),
    opponent = if_else(is_home, away_team_name, home_team_name),
    venue = if_else(is_home, "Home", "Away")
  )

# Possession data

possession_final <- possession_raw %>%
  mutate(
    match_id = as.numeric(match_id),
    elapsed = as.numeric(elapsed),
    elapsed_plus = replace_na(as.numeric(elapsed_plus), 0),
    homepos = as.numeric(homepos),
    awaypos = as.numeric(awaypos),
    possession_time = elapsed + elapsed_plus
  ) %>%
  arrange(match_id, elapsed, elapsed_plus) %>%
  group_by(match_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(
    match_id,
    homepos,
    awaypos,
    elapsed,
    elapsed_plus,
    possession_time
  )

# Merge possession with Sevilla matches

sevilla_pos <- sevilla_match %>%
  left_join(
    possession_final,
    by = c("id" = "match_id")
  ) %>%
  mutate(
    possession = if_else(is_home, homepos, awaypos)
  )

pos_df <- sevilla_pos %>%
  filter(!is.na(possession))

cat("Total Sevilla matches:", nrow(sevilla_pos), "\n")
cat("Matches with possession data:", nrow(pos_df), "\n")

# Possession bands

bands <- c("<45%", "45–50%", "50–55%", "55–60%", ">60%")

pos_df <- pos_df %>%
  mutate(
    possession_band = cut(
      possession,
      breaks = c(20, 45, 50, 55, 60, 80),
      labels = bands,
      include.lowest = TRUE,
      right = TRUE
    ),
    possession_band = factor(
      possession_band,
      levels = bands,
      ordered = TRUE
    ),
    points = case_when(
      result == "Win" ~ 3,
      result == "Draw" ~ 1,
      result == "Loss" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(possession_band))

# Result percentages by possession band

result_order <- c("Loss", "Draw", "Win")

counts <- pos_df %>%
  count(possession_band, result, name = "count") %>%
  complete(
    possession_band = factor(bands, levels = bands, ordered = TRUE),
    result = factor(result_order, levels = result_order),
    fill = list(count = 0)
  ) %>%
  group_by(possession_band) %>%
  mutate(
    total = sum(count),
    percentage = if_else(total > 0, count / total * 100, 0)
  ) %>%
  ungroup() %>%
  mutate(
    result = factor(result, levels = result_order),
    tooltip = paste0(
      "Possession band: ", possession_band,
      "<br>Result: ", result,
      "<br>Matches: ", count,
      "<br>Total in band: ", total,
      "<br>Share: ", round(percentage, 1), "%"
    ),
    label_text = if_else(
      percentage >= 8,
      paste0(round(percentage, 0), "%"),
      ""
    )
  )

summary_possession <- pos_df %>%
  group_by(possession_band) %>%
  summarise(
    n_matches = n(),
    wins = sum(result == "Win"),
    draws = sum(result == "Draw"),
    losses = sum(result == "Loss"),
    avg_points = mean(points, na.rm = TRUE),
    win_rate = wins / n_matches * 100,
    draw_rate = draws / n_matches * 100,
    loss_rate = losses / n_matches * 100,
    .groups = "drop"
  ) %>%
  mutate(
    possession_band = factor(
      possession_band,
      levels = bands,
      ordered = TRUE
    ),
    top_label = paste0(
      "n = ", n_matches,
      " | pts = ", round(avg_points, 2)
    )
  ) %>%
  arrange(possession_band)

print(summary_possession)

dir.create("figures", showWarnings = FALSE)

readr::write_csv(
  summary_possession,
  file.path("figures", "possession_win_draw_loss_summary_R.csv")
)

readr::write_csv(
  counts,
  file.path("figures", "possession_win_draw_loss_percentages_R.csv")
)

# Interactive stacked bar chart

result_colors <- c(
  "Loss" = "#D71920",
  "Draw" = "#F28E2B",
  "Win" = "#2CA02C"
)

interactive_possession_bands <- plot_ly()

for (res in result_order) {
  plot_data <- counts %>%
    filter(result == res)
  
  interactive_possession_bands <- interactive_possession_bands %>%
    add_trace(
      data = plot_data,
      x = ~possession_band,
      y = ~percentage,
      type = "bar",
      name = res,
      marker = list(
        color = result_colors[[res]],
        line = list(
          color = "white",
          width = 1
        )
      ),
      text = ~label_text,
      textposition = "inside",
      hoverinfo = "text",
      hovertext = ~tooltip
    )
}

interactive_possession_bands <- interactive_possession_bands %>%
  layout(
    barmode = "stack",
    title = list(
      text = paste0(
        "Sevilla FC results by possession band",
        "<br><sup>",
        "Win = 3 points | Draw = 1 point | Loss = 0 points | Top labels show sample size and average points",
        "</sup>"
      ),
      x = 0.5,
      xanchor = "center"
    ),
    font = common_layout$font,
    paper_bgcolor = common_layout$paper_bgcolor,
    plot_bgcolor = common_layout$plot_bgcolor,
    hoverlabel = common_layout$hoverlabel,
    xaxis = c(
      axis_style("Sevilla possession band"),
      list(
        categoryorder = "array",
        categoryarray = bands
      )
    ),
    yaxis = c(
      axis_style("Share of matches (%)"),
      list(
        range = c(0, 116),
        ticksuffix = "%"
      )
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.18,
      yanchor = "top",
      title = list(text = "Match result")
    ),
    annotations = lapply(seq_len(nrow(summary_possession)), function(i) {
      list(
        x = as.character(summary_possession$possession_band[i]),
        y = 103,
        text = summary_possession$top_label[i],
        showarrow = FALSE,
        xanchor = "center",
        yanchor = "bottom",
        font = list(size = 11, color = "#222222")
      )
    }),
    margin = list(t = 115, r = 70, b = 125, l = 85)
  )

interactive_possession_bands

# Save interactive plots
dir.create("figures", showWarnings = FALSE)
htmlwidgets::saveWidget(interactive_young_players, file.path("figures", "visual_1_promising_young_players.html"), selfcontained = TRUE)
htmlwidgets::saveWidget(interactive_spanish_teams, file.path("figures", "visual_2_spanish_teams_attack_defence.html"), selfcontained = TRUE)
htmlwidgets::saveWidget(interactive_possession_bands, file.path("figures", "visual_3_sevilla_possession_bands.html"), selfcontained = TRUE)
