#In essence, the story is about evaluating the efficiency and characteristics of the EPL betting market. 
#It asks:
#How often do different outcomes occur? 
#How many goals are typical? 
#Do the betting odds accurately reflect the likelihood of outcomes (especially home wins)? 
#And how do factors like market confidence, perceived team difference, and goal totals interact?

#Our goal was to investigate the relationship between pre-match betting odds and actual match results. 
#Specifically, how accurately do the betting markets predict EPL outcomes?

# EPL Odds vs. Reality Analysis Script

# Load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

# 1) Load & clean data
matches <- read_excel("Desktop/R Project DS/matches.xlsx")

epl <- matches %>%
  # Filter for EPL
  filter(grepl("england/premier-league", League, ignore.case = TRUE)) %>%
  #Drop missing odds or winner
  filter(!is.na(Home_team_odds),
         !is.na(Odds_for_draw),
         !is.na(Away_team_odds),
         !is.na(Winner)) %>%
  #score columns to numeric
  mutate(
    Score_home = as.numeric(Score_home),
    Score_away = as.numeric(Score_away)
  ) %>%
  # Derive outcome labels and total goals
  mutate(
    Winner = case_when(
      Winner == 1 ~ "Home Win",
      Winner == 2 ~ "Away Win",
      TRUE        ~ "Draw"
    ),
    Total_goals = Score_home + Score_away
  )

# 2) Compute outcome frequencies
outcome_counts <- epl %>%
  count(Winner) %>%
  mutate(pct = n / sum(n) * 100)

# 3) Compute implied probabilities & baseline prediction
#This gives the market's best estimate of the likelihood of each outcome

epl <- epl %>%
  mutate(
    ip_home = 1 / Home_team_odds,
    ip_draw = 1 / Odds_for_draw,
    ip_away = 1 / Away_team_odds,
    sum_ip  = ip_home + ip_draw + ip_away,
    p_home  = ip_home / sum_ip,
    p_draw  = ip_draw / sum_ip,
    p_away  = ip_away / sum_ip,
    pred    = case_when(
      p_home >= p_draw & p_home >= p_away ~ "Home Win",
      p_draw >= p_home & p_draw >= p_away ~ "Draw",
      TRUE                                ~ "Away Win"
    )
  )

# 4) Compute calibration bins for home-win
calib_home <- epl %>%
  mutate(bin = ntile(p_home, 10)) %>%
  group_by(bin) %>%
  summarize(
    mean_pred = mean(p_home),
    obs_rate  = mean(Winner == "Home Win")
  )

# 5) Compute odds difference for scatter
epl <- epl %>%
  mutate(odds_diff = abs(Home_team_odds - Away_team_odds))

# 6) Define color palette & theme
outcome_cols <- c(
  "Home Win" = "#3D195B",
  "Draw"     = "#4ECDC4",
  "Away Win" = "#FF6B6B"
)
theme_epl <- theme_minimal(base_size = 14) +
  theme(
    plot.title     = element_text(face="bold", hjust=0.5),
    plot.subtitle  = element_text(hjust=0.5),
    axis.title     = element_text(face="bold"),
    legend.position= "bottom",
    legend.title   = element_blank(),
    panel.grid.major = element_line(color="grey90"),
    panel.grid.minor = element_blank()
  )

# 7) Plot 1: Outcome Distribution
ggplot(outcome_counts, aes(Winner, pct, fill=Winner)) +
  geom_col(width=0.7) +
  geom_text(aes(label=sprintf("%.1f%%", pct)), vjust=-0.5) +
  scale_fill_manual(values=outcome_cols) +
  labs(
    title = "EPL Outcome Distribution (2003–2023)",
    y     = "% of Matches", x = NULL
  ) +
  theme_epl


# 9) Plot 3: Density of Implied Home‑Win Probability by Outcome
ggplot(epl, aes(x=p_home, fill=Winner)) +
  geom_density(alpha=0.5) +
  scale_fill_manual(values=outcome_cols) +
  labs(
    title = "Density of Implied Home/Win Probabilities by Outcome",
    x     = "Implied Home/Win Probability", y = "Density"
  ) +
  theme_epl

# 11) Plot 5: Odds Gap vs. Confidence vs. Goals Scatter

ggplot(epl, aes(
  x = p_home,
  y = odds_diff,
  color = Winner,
  size  = Total_goals
)) +
  geom_point(alpha=0.6) +
  scale_color_manual(values=outcome_cols) +
  scale_size_continuous(range=c(1,6)) +
  labs(
    title    = "Market Gap, Confidence & Match Excitement",
    subtitle = "Odds Spread vs. Implied Probability & Goals",
    x        = "Implied Home/Win Probability",
    y        = "Odds Difference (|Home–Away Odds|)",
    size     = "Total Goals"
  ) +
  theme_epl


#Plot 6 

# odds over time
library(dplyr)
library(ggplot2)

# Step 1: Data summary
epl_accuracy <- epl %>%
  filter(!is.na(p_home), !is.na(Winner)) %>%
  group_by(Season) %>%
  summarise(
    expected_home_wins = mean(p_home),
    actual_home_wins = mean(Winner == "Home Win")
  ) %>%
  ungroup()

# Step 2: Make sure season is in correct order (as numeric or factor)
epl_accuracy$Season <- as.character(epl_accuracy$Season)

#plot 

ggplot(epl_accuracy, aes(x = Season, group = 1)) +
  geom_line(aes(y = expected_home_wins, color = "Expected"), size = 1.5) +
  geom_line(aes(y = actual_home_wins,   color = "Actual"  ), size = 1.5) +
  scale_color_manual(values = c("Expected" = "#3D195B", "Actual" = "#4ECDC4")) +
  labs(
    title = "Yearly Odds Accuracy for EPL Home Wins",
    x     = "Season",
    y     = "Home Win Rate",
    color = ""
  ) +
  theme_epl +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  )



