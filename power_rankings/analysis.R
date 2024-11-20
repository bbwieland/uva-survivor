library(tidyverse)
library(gtExtras)
library(gt)

# Pre-processing ----------------------------------------------------------

df <- read_csv("power_rankings/power_rankings.csv")

# Cleaning some of the numbers out
df_clean <- df %>%
  # Cleaning out digits & punctuation & trailing whitespace
  mutate(player = str_replace_all(player, '[\\d|\\.\\(\\)|\\-|\\+]', '') %>% trimws()) %>%
  # Manually re-tagging some misspelled names
  mutate(player = case_when(
    season == 8 & player == "Annalise" ~ "Analise",
    season == 10 & player == "Caity" ~ "Catie",
    season == 10 & player == "Cristina" ~ "Christina",
    season == 9 & player == "Jenna" ~ "Jenna H",
    season == 8 & player == "Johann" ~ "Jojo",
    season == 8 & player == "Swisher" ~ "Swish",
    TRUE ~ player
  )) %>% 
  # Creating "player IDs" with names & seasons for unique identification
  mutate(player_id = paste0(tolower(player) %>% str_replace("\\s", ""), season))

# Validating our IDs: This should equal 2 (2 observations per player)
length(df_clean$player_id) / length(df_clean$player_id %>% unique())

# Pivoting into analysis format -------------------------------------------

player_df <- df_clean %>%
  pivot_wider(id_cols=c(player_id, player, season), names_from=type, values_from=rank) %>%
  mutate(movement = pre - post)

# Biggest climbers & biggest fallers --------------------------------------

player_df %>%
  arrange(-movement) %>%
  head(10) %>%
  select(player, season, pre, post, movement) %>% 
  gt() %>%
  gt_theme_538() %>%
  fmt_number(movement, force_sign = TRUE, decimals = 0) %>%
  tab_header('Top 10 Biggest Power Ranking Gainers') %>%
  tab_footnote('Only data from seasons 8, 9, 10, 14, 15 are included.') %>%
  tab_spanner('Ranking', columns = pre:movement) %>%
  tab_spanner('Player Info', columns = player:season)

player_df %>%
  arrange(movement) %>%
  head(10) %>%
  select(player, season, pre, post, movement) %>% 
  gt() %>%
  gt_theme_538() %>%
  fmt_number(movement, force_sign = TRUE, decimals = 0) %>%
  tab_header('Top 10 Biggest Power Ranking Fallers') %>%
  tab_footnote('Only data from seasons 8, 9, 10, 14, 15 are included.') %>%
  tab_spanner('Ranking', columns = pre:movement) %>%
  tab_spanner('Player Info', columns = player:season)

# Spearman rank correlations by season ------------------------------------

player_df %>%
  group_by(season) %>%
  summarise(spearman_corr = cor(pre, post, method='spearman')) %>%
  gt() %>%
  gt_theme_538(quiet=TRUE) %>%
  fmt_number(spearman_corr) %>%
  cols_label(spearman_corr ~ "Rank Correlation") %>%
  tab_source_note('if people ranked players totally at random, this number would be 0.00')

# Scatterplot -------------------------------------------------------------

player_df %>%
  group_by(pre) %>%
  summarise(post_rk = mean(post)) %>%
  filter(pre <= 18) %>%
  ggplot(aes(x = pre, y = post_rk)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1,18)) +
  scale_y_continuous(breaks = seq(1, 16)) +
  theme_bw() +
  labs(x = "Preseason Ranking", y = "Average Postseason Ranking", title = "UVA Surivor Preseason Rankings are Dogwater", subtitle = "at least for seasons 8, 9, 10, 14, 15")
