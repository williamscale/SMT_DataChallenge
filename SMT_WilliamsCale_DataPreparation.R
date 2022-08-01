# Clear workspace and set seed.
rm(list = ls())
set.seed(55)

setwd('D:/SMT_StudentDataChallenge')

load('./Data/data_raw.RData')
# Or load raw csv files here.

library(dplyr)

# DATA FILTERING ----------------------------------------------------------

# Assign ID to each play.
game_events$PLAY.ID <- paste(game_events$game_str, game_events$play_id, sep = '.')
game_info$PLAY.ID <- paste(game_info$game_str, game_info$play_per_game, sep = '.')
ball_pos$PLAY.ID <- paste(ball_pos$game_str, ball_pos$play_id, sep = '.')
player_pos$PLAY.ID <- paste(player_pos$game_str, player_pos$play_id, sep = '.')

## EXTRACT PLAY.ID TO KEEP ------------------------------------------------

# Extract unique plays from each dataset.
game_events.PLAY.ID <- game_events %>%
  select(PLAY.ID) %>%
  unique() %>%
  pull(PLAY.ID)

game_info.PLAY.ID <- game_info %>%
  # Need to exclude plays that appear multiple times.
  # I think input error, there were some that maybe were stolen bases.
  # And others that top & bottom of inning had identical PLAY.IDs.
  # Only 19 cases of this.
  group_by(PLAY.ID) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(PLAY.ID) %>%
  unique() %>%
  pull(PLAY.ID)

ball_pos.PLAY.ID <- ball_pos %>%
  select(PLAY.ID) %>%
  unique() %>%
  pull(PLAY.ID)

player_pos.PLAY.ID <- player_pos %>%
  select(PLAY.ID) %>%
  unique() %>%
  pull(PLAY.ID)

# Generate vector of plays that exist in all datasets.
keep.PLAY.ID <- Reduce(intersect, list(game_events.PLAY.ID,
                                       game_info.PLAY.ID,
                                       ball_pos.PLAY.ID,
                                       player_pos.PLAY.ID))

events <- game_events %>%
  filter(PLAY.ID %in% keep.PLAY.ID)

info <- game_info %>%
  filter(PLAY.ID %in% keep.PLAY.ID)

ball <- ball_pos %>%
  filter(PLAY.ID %in% keep.PLAY.ID)

player <- player_pos %>%
  filter(PLAY.ID %in% keep.PLAY.ID)

# Remove unneeded files.
rm(game_events, game_info, ball_pos, player_pos, game_events.PLAY.ID,
   game_info.PLAY.ID, ball_pos.PLAY.ID, player_pos.PLAY.ID)

save.image(file = 'data.RData')
