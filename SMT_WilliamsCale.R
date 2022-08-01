# Clear workspace and set seed.
rm(list = ls())
set.seed(55)

setwd('D:/SMT_StudentDataChallenge')

load('./data.RData')

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(sportyR)
library(ggforce)
library(cowplot)
library(ggridges)

# INTRODUCTION ------------------------------------------------------------

length(unique(ball$PLAY.ID))
length(unique(events$PLAY.ID))
length(unique(info$PLAY.ID))
length(unique(player$PLAY.ID))

# EXTRACT PLAYS WITH THROW TO 1B ------------------------------------------

p.3 <- events %>%
  group_by(PLAY.ID) %>%
  # Filter by batted ball by batter,
  filter(any(player_position == 10 & event_code == 4) &
           # Caught ball by 1B,
           any(player_position == 3 & event_code == 2) &
           # and thrown ball by any player except 1B.
           any(event_code == 3 & player_position != 3))

throwers <- c()

# For each event...
for (i in 1:nrow(p.3)) {
  
  # If 1B catches ball...
  if ((p.3[i, 'player_position'] == 3) & (p.3[i, 'event_code'] == 2)) {
    
    # Capture previous event as throwing player.
    throwers <- c(throwers, p.3[(i - 1), 'player_position'])
    
  } else {
    
    throwers <- c(throwers, NA)
    
  }
  
}

p.3$thrower <- throwers

# Inspect plays with multiple throws to 1B.
inspect <- p.3 %>%
  group_by(PLAY.ID) %>%
  summarize(non.na = sum(!is.na(thrower))) %>%
  filter(non.na != 1)

inspect.detailed <- p.3 %>%
  filter(PLAY.ID %in% inspect$PLAY.ID)

# Manually inspected.
# Reasons for exclusion:
# 1) Throw/catch logged twice consecutively with different timestamps.
# 2) Catcher throwing to 1st.
# The remaining were balls hit to 1B, throw to SS/2B, and back to 1B.
p.remove <- c('1901_18_TeamLH_TeamA3.32', '1903_23_TeamNA_TeamA1.196')

p.3 <- p.3 %>%
  # Remove inspected plays.
  filter(!(PLAY.ID %in% p.remove)) %>%
  # Change NA to 0.
  mutate(thrower = ifelse(is.na(thrower), 0, as.numeric(thrower))) %>%
  # Remove throws from non-infielders.
  filter(any(thrower %in% c(4, 5, 6))) %>%
  # Convert bounces to 0.
  mutate(thrower = ifelse(thrower == 255, 0, thrower))
length(unique(p.3$PLAY.ID))

# Extract indices of throw/catch timestamps.
catch.idx <- which(p.3[, 'thrower'] != 0)
throw.idx <- catch.idx - 1

p.3.summary <- data.frame(p.3[catch.idx, 'PLAY.ID'],
                          p.3[throw.idx, 'timestamp'],
                          p.3[catch.idx, 'timestamp'],
                          p.3[catch.idx, 'thrower']) %>%
  rename(timestamp.throw = timestamp,
         timestamp.catch = timestamp.1)

# EXTRACT BALL LOCATION WHEN THROWN / CAUGHT ------------------------------

ball.throw <- p.3.summary %>%
  left_join(ball, by = c('PLAY.ID', 'timestamp.throw' = 'timestamp'))

sum(is.na(ball.throw$ball_position_x))
ball.throw %>%
  filter(is.na(ball_position_x))

ball.throw <- ball.throw %>%
  filter(!is.na(ball_position_x)) %>%
  rename(ball_position_x.throw = ball_position_x,
         ball_position_y.throw = ball_position_y,
         ball_position_z.throw = ball_position_z)
sum(is.na(ball.throw))

p.3.ball <- ball.throw %>%
  left_join(ball, by = c('PLAY.ID', 'timestamp.catch' = 'timestamp')) %>%
  rename(ball_position_x.catch = ball_position_x,
         ball_position_y.catch = ball_position_y,
         ball_position_z.catch = ball_position_z) %>%
  select(-c(game_str.x, play_id.x, game_str.y, play_id.y))

sum(is.na(p.3.ball)) 
length(unique(p.3.ball$PLAY.ID))

# IDENTIFY / REMOVE CUTOFF THROWS -----------------------------------------

# Define center of first base.
b1 <- c(sqrt((90 - 15/12) ^ 2 / 2), sqrt((90) ^ 2 / 2), 0)

p.3.ball <- p.3.ball %>%
  mutate(dist.b1.catch = sqrt((ball_position_x.catch - b1[1]) ^ 2 +
                                (ball_position_y.catch - b1[2]) ^ 2))

# Plot where balls are thrown from.
geom_baseball('mlb') +
  geom_point(data = p.3.ball,
             aes(x = ball_position_x.throw,
                 y = ball_position_y.throw,
                 color = as.factor(thrower)),
             size = 2) +
  scale_color_manual(values = c('#f0f921', '#f89540', '#dc322f')) +
  labs(color = 'Thrower') +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.position = c(0.94, 0.1)) +
  guides(color = guide_legend(override.aes = list(size = 8)))

ggsave(filename = file.path('Visuals', 'p1.png'), width = 14, height = 11)

# Plot where balls are caught.
geom_baseball('mlb') +
  geom_point(data = p.3.ball,
             aes(x = ball_position_x.catch,
                 y = ball_position_y.catch),
             size = 2,
             color = 'black') +
  # Just adding this point so sportyR matches area of previous plot.
  geom_point(data = p.3.ball %>%
               slice_max(ball_position_y.throw),
             aes(x = ball_position_x.throw,
                 y = ball_position_y.throw),
             size = 0)

ggsave(filename = file.path('Visuals', 'p2.png'), width = 14, height = 11)


## CUTOFF EXAMPLES --------------------------------------------------------

# Example 1.
cutoff.ex1 <- p.3.ball %>%
  filter(PLAY.ID == '1903_27_TeamNK_TeamB.186')

cutoff.p1 <- geom_baseball('mlb') +
  geom_path(data = ball %>%
              filter(PLAY.ID == cutoff.ex1$PLAY.ID &
                       timestamp >= cutoff.ex1$timestamp.throw &
                       timestamp <= cutoff.ex1$timestamp.catch),
            aes(x = ball_position_x,
                y = ball_position_y),
            size = 1,
            color = 'white',
            linetype = 'dashed') +
  geom_point(data = cutoff.ex1,
             aes(x = ball_position_x.throw,
                 y = ball_position_y.throw),
             size = 4,
             color = '#f0f921') +
  geom_point(data = cutoff.ex1,
             aes(x = ball_position_x.catch,
                 y = ball_position_y.catch),
             size = 4,
             color = 'black')

cutoff.p1

ggsave(filename = file.path('Visuals', 'p3.png'), width = 14, height = 9)

cutoff.p1.detailed <- geom_baseball('mlb') +
  geom_path(data = ball %>%
              filter(PLAY.ID == cutoff.ex1$PLAY.ID) %>%
              mutate(timestamp.d = (timestamp - timestamp[1]) / 1000),
            aes(x = ball_position_x,
                y = ball_position_y,
                color = timestamp.d),
            size = 2,
            linetype = 'solid') +
  geom_point(data = cutoff.ex1,
             aes(x = ball_position_x.throw,
                 y = ball_position_y.throw),
             size = 4,
             color = '#f0f921') +
  geom_point(data = cutoff.ex1,
             aes(x = ball_position_x.catch,
                 y = ball_position_y.catch),
             size = 4,
             color = 'black') +
  labs(color = 'Time Since \nPitch Release') +
  scale_color_continuous(type = 'viridis') +
  theme(legend.title = element_text(size = 20),
        legend.text = element_blank(),
        legend.position = c(0.8, 0.1),
        legend.background = element_blank(),
        legend.direction = 'horizontal')

cutoff.p1.detailed

ggsave(filename = file.path('Visuals', 'p4.png'), width = 14, height = 14)

events %>%
  filter(PLAY.ID == '1903_27_TeamNK_TeamB.186')
info %>%
  filter(PLAY.ID == '1903_27_TeamNK_TeamB.186')


cutoff.ex2 <- p.3.ball %>%
  filter(PLAY.ID == '1902_02_TeamMG_TeamA3.161')

cutoff.p2 <- geom_baseball('mlb') +
  geom_path(data = ball %>%
              filter(PLAY.ID == cutoff.ex2$PLAY.ID &
                       timestamp >= cutoff.ex2$timestamp.throw &
                       timestamp <= cutoff.ex2$timestamp.catch),
            aes(x = ball_position_x,
                y = ball_position_y),
            size = 1,
            color = 'white',
            linetype = 'dashed') +
  geom_point(data = cutoff.ex2,
             aes(x = ball_position_x.throw,
                 y = ball_position_y.throw),
             size = 4,
             color = '#dc322f') +
  geom_point(data = cutoff.ex2,
             aes(x = ball_position_x.catch,
                 y = ball_position_y.catch),
             size = 4,
             color = 'black')

cutoff.p2

ggsave(filename = file.path('Visuals', 'p5.png'), width = 14, height = 8)

cutoff.p2.detailed <- geom_baseball('mlb') +
  geom_path(data = ball %>%
              filter(PLAY.ID == cutoff.ex2$PLAY.ID) %>%
              mutate(timestamp.d = (timestamp - timestamp[1]) / 1000),
            aes(x = ball_position_x,
                y = ball_position_y,
                color = timestamp.d),
            size = 2,
            linetype = 'solid') +
  geom_point(data = cutoff.ex2,
             aes(x = ball_position_x.throw,
                 y = ball_position_y.throw),
             size = 4,
             color = '#dc322f') +
  geom_point(data = cutoff.ex2,
             aes(x = ball_position_x.catch,
                 y = ball_position_y.catch),
             size = 4,
             color = 'black') +
  labs(color = 'Time Since \nPitch Release') +
  scale_color_continuous(type = 'viridis') +
  theme(legend.title = element_text(size = 20),
        legend.text = element_blank(),
        legend.position = c(0.8, 0.1),
        legend.background = element_blank(),
        legend.direction = 'horizontal') +
  geom_point(data = player %>%
               filter(PLAY.ID == cutoff.ex2$PLAY.ID & player_position == 6) %>%
               mutate(timestamp.d = (timestamp - timestamp[1]) / 1000),
             aes(x = field_x,
                 y = field_y,
                 color = timestamp.d))

cutoff.p2.detailed

ggsave(filename = file.path('Visuals', 'p6.png'), width = 14, height = 8)

events %>%
  filter(PLAY.ID == '1902_02_TeamMG_TeamA3.161')
info %>%
  filter(PLAY.ID == '1902_02_TeamMG_TeamA3.161')

# Remove plays where 1B catches ball > 6 feet from first base.
p.3.ball <- p.3.ball %>%
  filter(dist.b1.catch <= 6)

# Plot where balls are thrown from.
geom_baseball('mlb') +
  geom_point(data = p.3.ball,
             aes(x = ball_position_x.throw,
                 y = ball_position_y.throw,
                 color = as.factor(thrower)),
             size = 2) +
  scale_color_manual(values = c('#f0f921', '#f89540', '#dc322f')) +
  labs(color = 'Thrower') +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.position = c(0.94, 0.1)) +
  guides(color = guide_legend(override.aes = list(size = 8)))

ggsave(filename = file.path('Visuals', 'p7.png'), width = 14, height = 8)

# Plot where balls are caught.
geom_baseball('mlb') +
  geom_point(data = p.3.ball,
             aes(x = ball_position_x.catch,
                 y = ball_position_y.catch),
             size = 2,
             color = 'black') +
  # Just adding this point so sportyR matches area of previous plot.
  geom_point(data = p.3.ball %>%
               slice_max(ball_position_y.throw),
             aes(x = ball_position_x.throw,
                 y = ball_position_y.throw),
             size = 0)

ggsave(filename = file.path('Visuals', 'p8.png'), width = 14, height = 8)

# THROWER ANALYSIS: SPEED -------------------------------------------------

thrower.4 <- info %>%
  filter(PLAY.ID %in% (p.3.ball %>%
                         filter(thrower == 4) %>%
                         pull(PLAY.ID))) %>%
  select(PLAY.ID, second_base) %>%
  rename(thrower.id = second_base)

thrower.5 <- info %>%
  filter(PLAY.ID %in% (p.3.ball %>%
                         filter(thrower == 5) %>%
                         pull(PLAY.ID))) %>%
  select(PLAY.ID, third_base) %>%
  rename(thrower.id = third_base)

thrower.6 <- info %>%
  filter(PLAY.ID %in% (p.3.ball %>%
                         filter(thrower == 6) %>%
                         pull(PLAY.ID))) %>%
  select(PLAY.ID, shortstop) %>%
  rename(thrower.id = shortstop)

thrower.ID <- rbind(thrower.4, thrower.5, thrower.6)
length(unique(thrower.ID$thrower.id))

assertthat::are_equal(nrow(p.3.ball),
                      sum(nrow(thrower.4), nrow(thrower.5), nrow(thrower.6)))

p.3.ball <- p.3.ball %>%
  left_join(thrower.ID, by = 'PLAY.ID')
sum(is.na(p.3.ball))

p.3.ball <- p.3.ball %>%
  mutate(dist.throw = sqrt((ball_position_x.throw - ball_position_x.catch) ^ 2 +
                             (ball_position_y.throw - ball_position_y.catch) ^ 2 +
                             (ball_position_z.throw - ball_position_z.catch) ^ 2),
         speed = (dist.throw / (timestamp.catch - timestamp.throw)) * (1000 * 3600 / 5280))

## SPEED: ALL PLAYS -------------------------------------------------------

speed.plot <- ggplot(data = p.3.ball,
                     aes(x = speed,
                         group = as.factor(thrower),
                         color = as.factor(thrower))) +
  geom_histogram(aes(y = ..density..,
                     fill = as.factor(thrower)),
                 alpha = 0.5,
                 position = 'identity',
                 size = 1) +
  geom_density(size = 2) +
  scale_color_manual(values = c('#f0f921', '#f89540', '#dc322f'),
                     labels = c('2B', '3B', 'SS')) +
  scale_fill_manual(values = c('#f0f921', '#f89540', '#dc322f'),
                    guide = 'none') +
  labs(color = 'Thrower') +
  xlab('Speed [mph]') +
  ylab('Density') +
  theme_solarized_2(light = FALSE) +
  facet_wrap(~as.factor(thrower),
             ncol = 1,
             scales = 'fixed') +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        strip.text = element_text(size = 20)) +
  guides(color = guide_legend(override.aes = list(size = 8)))

speed.plot
ggsave(filename = file.path('Visuals', 'p9.png'), width = 14, height = 10)

speed.bw.plot <- ggplot(data = p.3.ball,
                        aes(x = as.factor(thrower),
                            y = speed,
                            fill = as.factor(thrower))) +
  geom_boxplot(color = '#839496',
               size  = 1) +
  scale_fill_manual(values = c('#f0f921', '#f89540', '#dc322f'),
                    labels = c('2B', '3B', 'SS')) +
  labs(fill = 'Thrower') +
  xlab('Thrower') +
  ylab('Speed [mph]') +
  theme_solarized_2(light = FALSE) +
  theme(panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

speed.bw.plot
ggsave(filename = file.path('Visuals', 'p10.png'), width = 14, height = 10)

### ANOVA -------------------------------------------------------------------

aov.speed <- aov(speed ~ as.factor(thrower),
                 data = p.3.ball)
summary(aov.speed)
model.tables(x = aov.speed,
             type = 'means')

## DISTANCE: ALL PLAYS ----------------------------------------------------

dist.plot <- ggplot(data = p.3.ball,
                     aes(x = dist.throw,
                         group = as.factor(thrower),
                         color = as.factor(thrower))) +
  geom_histogram(aes(y = ..density..,
                     fill = as.factor(thrower)),
                 alpha = 0.5,
                 position = 'identity',
                 size = 1) +
  geom_density(size = 2) +
  scale_color_manual(values = c('#f0f921', '#f89540', '#dc322f'),
                     labels = c('2B', '3B', 'SS')) +
  scale_fill_manual(values = c('#f0f921', '#f89540', '#dc322f'),
                    guide = 'none') +
  labs(color = 'Thrower') +
  xlab('Distance [ft]') +
  ylab('Density') +
  theme_solarized_2(light = FALSE) +
  facet_wrap(~as.factor(thrower),
             ncol = 1,
             scales = 'fixed') +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        strip.text = element_text(size = 20)) +
  guides(color = guide_legend(override.aes = list(size = 8)))

dist.plot
ggsave(filename = file.path('Visuals', 'p11.png'), width = 14, height = 10)

dist.bw.plot <- ggplot(data = p.3.ball,
                        aes(x = as.factor(thrower),
                            y = dist.throw,
                            fill = as.factor(thrower))) +
  geom_boxplot(color = '#839496',
               size  = 1) +
  scale_fill_manual(values = c('#f0f921', '#f89540', '#dc322f'),
                    labels = c('2B', '3B', 'SS')) +
  labs(fill = 'Thrower') +
  xlab('Thrower') +
  ylab('Distance [ft]') +
  theme_solarized_2(light = FALSE) +
  theme(panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

dist.bw.plot
ggsave(filename = file.path('Visuals', 'p12.png'), width = 14, height = 10)

## CLASSIFY CLOSE PLAYS ---------------------------------------------------

player.close <- player %>%
  filter(PLAY.ID %in% p.3.ball$PLAY.ID &
           player_position == 10) %>%
  mutate(dist.b1 = sqrt((field_x - b1[1]) ^ 2 + (field_y - b1[2]) ^ 2)) %>%
  left_join(p.3.ball, by = 'PLAY.ID') %>%
  select(-c(9, 11:18))

assertthat::are_equal(length(unique(player.close$PLAY.ID)),
                      length(unique(p.3.ball$PLAY.ID)))

player.close <- player.close %>%
  group_by(PLAY.ID) %>%
  filter(dist.b1 <= 5 &
           abs(timestamp - timestamp.catch) <= 1000)

p.3.ball <- p.3.ball %>%
  mutate(close.flag = case_when(PLAY.ID %in% player.close$PLAY.ID ~ 1,
                                TRUE ~ 0))

sum(p.3.ball$close.flag)
sum(p.3.ball$close.flag) / nrow(p.3.ball)
assertthat::are_equal(length(unique(player.close$PLAY.ID)),
                      sum(p.3.ball$close.flag))

speed.bw.plot <- ggplot(data = p.3.ball,
                        aes(x = as.factor(thrower),
                            y = speed,
                            # fill = as.factor(thrower),
                            fill = as.factor(close.flag))) +
  geom_boxplot(color = '#839496',
               size  = 1) +
  scale_fill_manual(values = c('#b58900', '#cb4b16'),
                    labels = c('Non-close', 'Close')) +
  labs(fill = 'Play') +
  xlab('Thrower') +
  ylab('Speed [mph]') +
  theme_solarized_2(light = FALSE) +
  theme(panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

speed.bw.plot
ggsave(filename = file.path('Visuals', 'p13.png'), width = 14, height = 8)

summary(aov(speed ~ close.flag, data = p.3.ball[p.3.ball$thrower == 4, ]))
summary(aov(speed ~ close.flag, data = p.3.ball[p.3.ball$thrower == 5, ]))
summary(aov(speed ~ close.flag, data = p.3.ball[p.3.ball$thrower == 6, ]))

p.3.ball <- p.3.ball %>%
  filter(thrower == 5 |
         thrower == 6 |
         thrower == 4 & close.flag == 1)

nrow(p.3.ball[p.3.ball$thrower == 4, ])
nrow(p.3.ball[p.3.ball$thrower == 5, ])
nrow(p.3.ball[p.3.ball$thrower == 6, ])

## SPEED: CLOSE PLAYS -----------------------------------------------------

t.4 <- p.3.ball %>%
  filter(thrower == 4)
t.5 <- p.3.ball %>%
  filter(thrower == 5)
t.6 <- p.3.ball %>%
  filter(thrower == 6)

length(unique(t.4$thrower.id))
length(unique(t.5$thrower.id))
length(unique(t.6$thrower.id))

common.4.summary <- t.4 %>%
  group_by(thrower.id) %>%
  summarize(n = n(), mu.s = mean(speed), mu.d = mean(dist.throw)) %>%
  arrange(desc(n))
common.5.summary <- t.5 %>%
  group_by(thrower.id) %>%
  summarize(n = n(), mu.s = mean(speed), mu.d = mean(dist.throw)) %>%
  arrange(desc(n))
common.6.summary <- t.6 %>%
  group_by(thrower.id) %>%
  summarize(n = n(), mu.s = mean(speed), mu.d = mean(dist.throw)) %>%
  arrange(desc(n))

common.4.n <- t.4 %>%
  count(thrower.id, sort = TRUE) %>%
  slice_max(n, n = 4, with_ties = TRUE)
common.5.n <- t.5 %>%
  count(thrower.id, sort = TRUE) %>%
  slice_max(n, n = 4, with_ties = TRUE)
common.6.n <- t.6 %>%
  count(thrower.id, sort = TRUE) %>%
  slice_max(n, n = 4, with_ties = TRUE)

# COORDINATE SYSTEM -------------------------------------------------------

geom_baseball('mlb') +
  geom_segment(aes(x = 0, y = 0, xend = 40, yend = 0),
               size = 1.5,
               arrow = arrow(length = unit(0.1, 'inch')),
               lineend = 'butt',
               linejoin = 'mitre') +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 40),
               size = 1.5,
               arrow = arrow(length = unit(0.1, 'inch')),
               lineend = 'butt',
               linejoin = 'mitre') +
  annotate(geom = 'text',
           label = 'x',
           x = 38, y = 10,
           size = 8) +
  annotate(geom = 'text',
           label = 'y',
           x = 8, y = 40,
           size = 8)

ggsave(filename = file.path('Visuals', 'p14.png'), width = 14, height = 8)

geom_baseball('mlb') +
  geom_segment(aes(x = sqrt(90 ^ 2 / 2), y = sqrt(90 ^ 2 / 2), xend = sqrt((90 + 40) ^ 2 / 2), yend = sqrt((90 + 40) ^ 2 / 2)),
               size = 1.5,
               arrow = arrow(length = unit(0.1, 'inch')),
               lineend = 'butt',
               linejoin = 'mitre') +
  geom_segment(aes(x = sqrt(90 ^ 2 / 2), y = sqrt(90 ^ 2 / 2), xend = sqrt((90 - 40) ^ 2 / 2), yend = sqrt((90 + 40) ^ 2 / 2)),
               size = 1.5,
               arrow = arrow(length = unit(0.1, 'inch')),
               lineend = 'butt',
               linejoin = 'mitre') +
  annotate(geom = 'text',
           label = 'x',
           x = 95, y = 90,
           size = 8) +
  annotate(geom = 'text',
           label = 'y',
           x = 30, y = 90,
           size = 8)

ggsave(filename = file.path('Visuals', 'p15.png'), width = 14, height = 8)


# DEFINE VECTORS FROM FIRST BASE TO THROW LOCATION ------------------------

p.3.ball <- p.3.ball %>%
  mutate(ball_position_xT.throw = ball_position_x.throw * cos(pi / 4) +
           ball_position_y.throw * sin(pi / 4) - 90,
         ball_position_yT.throw = - ball_position_x.throw * sin(pi / 4) +
           ball_position_y.throw * cos(pi / 4),
         ball_position_xT.catch = ball_position_x.catch * cos(pi / 4) +
           ball_position_y.catch * sin(pi / 4) - 90,
         ball_position_yT.catch = - ball_position_x.catch * sin(pi / 4) +
           ball_position_y.catch * cos(pi / 4))

# From perspective of first base, not first baseman.
ggplot(data = p.3.ball,
       aes(x = ball_position_xT.catch,
           y = ball_position_z.catch,
           color = as.factor(thrower))) +
  geom_point() +
  geom_vline(xintercept = mean(p.3.ball[p.3.ball$thrower == 4, 'ball_position_xT.catch']),
             color = '#f0f921',
             size = 1) +
  geom_vline(xintercept = mean(p.3.ball[p.3.ball$thrower == 5, 'ball_position_xT.catch']),
             color = '#f89540',
             size = 1) +
  geom_vline(xintercept = mean(p.3.ball[p.3.ball$thrower == 6, 'ball_position_xT.catch']),
             color = '#dc322f',
             size = 1) +
  scale_color_manual(values = c('#f0f921', '#f89540', '#dc322f')) +
  labs(color = 'Thrower') +
  xlab('Catch x Location (CS2)') +
  ylab('Catch z Location') +
  theme_solarized_2(light = FALSE) +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)) +
  guides(color = guide_legend(override.aes = list(size = 6)))

ggsave(filename = file.path('Visuals', 'p16.png'), width = 14, height = 10)


ggplot(data = p.3.ball,
       aes(x = as.factor(thrower),
           y = ball_position_xT.catch,
           fill = as.factor(thrower))) +
  geom_boxplot(color = '#839496',
               size  = 1) +
  scale_fill_manual(values = c('#f0f921', '#f89540', '#dc322f')) +
  labs(fill = 'Thrower') +
  xlab('Thrower') +
  ylab('Catch x Location (CS2)') +
  theme_solarized_2(light = FALSE) +
  theme(panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

ggsave(filename = file.path('Visuals', 'p17.png'), width = 14, height = 10)

aov.catch <- aov(ball_position_xT.catch ~ thrower,
                 data = p.3.ball)
summary(aov.catch)
model.tables(x = aov.catch,
             type = 'means')

# Because p = 0, null hypothesis of equal means is rejected.

p.3.ball <- p.3.ball %>%
  mutate(theta.throw = pi / 2 - atan2(ball_position_yT.throw, ball_position_xT.throw),
         ball_position_x.body = ball_position_xT.catch * cos(theta.throw) -
           ball_position_yT.catch * sin(theta.throw))

ggplot(data = p.3.ball,
                   aes(x = ball_position_x.body,
                       y = ball_position_z.catch,
                       color = as.factor(thrower))) +
  geom_point() +
  geom_vline(xintercept = mean(p.3.ball[p.3.ball$thrower == 4, 'ball_position_x.body']),
             color = '#f0f921',
             size = 1) +
  geom_vline(xintercept = mean(p.3.ball[p.3.ball$thrower == 5, 'ball_position_x.body']),
             color = '#f89540',
             size = 1) +
  geom_vline(xintercept = mean(p.3.ball[p.3.ball$thrower == 6, 'ball_position_x.body']),
             color = '#dc322f',
             size = 1) +
  scale_color_manual(values = c('#f0f921', '#f89540', '#dc322f')) +
  labs(color = 'Thrower') +
  xlab('Catch x Location (Body CS)') +
  ylab('Catch z Location') +
  theme_solarized_2(light = FALSE) +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)) +
  guides(color = guide_legend(override.aes = list(size = 6)))

ggsave(filename = file.path('Visuals', 'p18.png'), width = 14, height = 10)


ggplot(data = p.3.ball,
       aes(x = as.factor(thrower),
           y = ball_position_x.body,
           fill = as.factor(thrower))) +
  geom_boxplot(color = '#839496',
               size  = 1) +
  scale_fill_manual(values = c('#f0f921', '#f89540', '#dc322f')) +
  labs(fill = 'Thrower') +
  xlab('Thrower') +
  ylab('Catch x Location (Body CS)') +
  theme_solarized_2(light = FALSE) +
  theme(panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

ggsave(filename = file.path('Visuals', 'p19.png'), width = 14, height = 10)


aov.catch2 <- aov(ball_position_x.body ~ thrower,
                 data = p.3.ball)

summary(aov.catch2)
model.tables(x = aov.catch2,
             type = 'means')


# THROWER ANALYSIS: ACCURACY ----------------------------------------------

t.4 <- t.4 %>%
  left_join(select(p.3.ball, PLAY.ID, ball_position_x.body), by = 'PLAY.ID')
t.5 <- t.5 %>%
  left_join(select(p.3.ball, PLAY.ID, ball_position_x.body), by = 'PLAY.ID')
t.6 <- t.6 %>%
  left_join(select(p.3.ball, PLAY.ID, ball_position_x.body), by = 'PLAY.ID')

mu.x.4 <- t.4 %>%
  group_by(thrower.id) %>%
  summarize(mu.x = mean(ball_position_x.body))
mu.x.5 <- t.5 %>%
  group_by(thrower.id) %>%
  summarize(mu.x = mean(ball_position_x.body))
mu.x.6 <- t.6 %>%
  group_by(thrower.id) %>%
  summarize(mu.x = mean(ball_position_x.body))

common.4.summary <- common.4.summary %>%
  left_join(mu.x.4, by = 'thrower.id')
common.5.summary <- common.5.summary %>%
  left_join(mu.x.5, by = 'thrower.id')
common.6.summary <- common.6.summary %>%
  left_join(mu.x.6, by = 'thrower.id')


# SUMMARY -----------------------------------------------------------------

## THROWER INTERSECTION ---------------------------------------------------

intersect(common.4.summary$thrower.id, common.5.summary$thrower.id)
intersect(common.4.summary$thrower.id, common.6.summary$thrower.id)
intersect(common.5.summary$thrower.id, common.6.summary$thrower.id)

common.summary <- p.3.ball %>%
  group_by(thrower.id) %>%
  summarize(n = n(),
            mu.s = mean(speed),
            mu.d = mean(dist.throw),
            mu.x = mean(ball_position_x.body)) %>%
  arrange(desc(n))

common <- p.3.ball %>%
  filter(thrower.id %in% common.summary$thrower.id[1:5])

ridgeline.speed <- ggplot(data = common,
                          aes(x = speed,
                              y = fct_rev(fct_infreq(as.factor(thrower.id),
                                                     ordered = FALSE)),
                              fill = as.factor(thrower.id))) +
  geom_density_ridges2(scale = 1.5,
                       color = 'black',
                       size = 2) +
  xlab('Throw Speed') +
  ylab('Density by Player ID') +
  scale_fill_manual(values = c('#b58900', '#d33682', '#6c71c4', '#2aa198', '#cb4b16')) +
  theme_solarized_2(light = FALSE) +
  theme(legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

ridgeline.speed

ggsave(filename = file.path('Visuals', 'p20.png'), width = 14, height = 10)

ridgeline.x <- ggplot(data = common,
                          aes(x = ball_position_x.body,
                              y = fct_rev(fct_infreq(as.factor(thrower.id),
                                                     ordered = FALSE)),
                              fill = as.factor(thrower.id))) +
  geom_density_ridges2(scale = 1.5,
                       color = 'black',
                       size = 2) +
  xlab('Catch x Location (Body CS)') +
  ylab('Density by Player ID') +
  scale_fill_manual(values = c('#b58900', '#d33682', '#6c71c4', '#2aa198', '#cb4b16')) +
  theme_solarized_2(light = FALSE) +
  theme(legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

ridgeline.x

ggsave(filename = file.path('Visuals', 'p21.png'), width = 14, height = 10)

