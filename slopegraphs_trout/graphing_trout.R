#### Bill Petti
#### Research Notebook
#### Slopegraphs and Mike Trout
#### Originally coded October 2016

# load required packages

if(!require(baseballr)) {
  install_github("BillPetti/baseballr")
  require(baseballr)
} # functions for baseball analysis

require(tidyverse) # for data manipulation 
require(rvest) # for data scraping
require(ggrepel) # for plot labeling 
require(dplyr) # for data manipulation
require(magrittr) # for data manipulation

# load my custom ggplot theme and mlb team color palette

source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

source("https://gist.githubusercontent.com/BillPetti/b8fd46e24163fbd63e678a7b5689202f/raw/59f84aeceb51272105b6e59f512f0d4c020db654/mlb_team_colors.R")

### cummulative performance since 2012

trout_cum_last5yrs <- fg_bat_leaders(2012, 2016, qual = "y", ind = 0, league = "all")

# reduce the variables we are working with

trout_cum_red <- trout_cum_last5yrs %>% 
  select(Name, Team, wRC_plus, RE24, WAR)

# melt the data, so every row is a player, team, and individual variable, and that variable's value

trout_cum_red_melt <- trout_cum_red %>% 
  gather(variable, value, -Name, - Team)

# rename wRC_plus variable

trout_cum_red_melt$variable <- with(trout_cum_red_melt, ifelse(variable == "wRC_plus", "wRC+", variable))

# rank hitters based on each metric

trout_cum_red_melt <- trout_cum_red_melt %>% 
  group_by(variable) %>% 
  arrange(variable, desc(value)) %>% 
  mutate(rank = row_number(variable)) %>% 
  ungroup() 

# filter by top 10 players based on golf score

trout_golfscore <- trout_cum_red_melt %>% 
  select(-value) %>% 
  spread(key = variable, value = rank) %>%
  mutate(golf_score = rowSums(.[, c(3:5)], na.rm = TRUE)) %>%
  arrange(golf_score) %>% 
  slice(1:10) %>%
  mutate(rank = seq(1:10))

# choose a team for players with multiple teams over the span

trout_cum_red_melt_2 <- trout_cum_red_melt %>%
  filter(Name %in% trout_golfscore$Name)

plyrs_multiple_tms <- filter(trout_cum_red_melt_2, Name %in% c("Josh Donaldson", "Robinson Cano"))

# code Donaldson as a Blue Jay and Cano as a Mariner

plyrs_multiple_tms$Team <- c("Blue Jays", "Mariners")

plyrs_multiple <- trout_cum_red_melt_2 %>% 
  filter(!Name %in% plyrs_multiple_tms$Name)

# merge the Donaldson and Cano cases back into the melted dataset

trout_cum_red_melt_2 <- rbind(plyrs_multiple, plyrs_multiple_tms) %>%
  mutate(rank = factor(rank, levels = rev(seq(1:30))))

# plot the data as a slope graph

ggplot(trout_cum_red_melt_2, aes(factor(variable), rank, 
                            group = Name, 
                            label = Name, 
                            color = Team)) +
  geom_line(size = .75) +
  geom_point(size = 4) + 
  geom_text(data = subset(trout_cum_red_melt_2, variable == "wRC+"), aes(label = paste0("    ", Name)), fontface = "bold", hjust = 0) + 
  geom_text(data = subset(trout_cum_red_melt_2, variable == "RE24"), aes(label = paste0(Name, "    ")), fontface = "bold", hjust = 1) + 
  ggtitle("\n   Comparing the Rank of MLB Players on Key Metrics: 2012-16\n") + 
  labs(subtitle = "    Ordered Highest to Lowest\n", caption = "\n\nCreated by @BillPetti\nData acquired using the baseballr package\nData courtesy of FanGraphs.com") +
  scale_colour_manual(values = mlb_team_colors) +
  theme_bp_grey() + 
  theme(legend.position = "none", 
        axis.text.y=element_blank(), 
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())

# save plot to working directory

ggsave("trout_aggregate_slopegraph_variable.png", scale = 1, height = 8.5, width = 14, units = "in")

### Comparing players across the three metrics, by season

# pull data from FanGraphs for individual seasons for qualified hitters for 2012-2016

trout_last5yrs <- fg_bat_leaders(2012, 2016, league = "all", ind = 1)

# reduce the variables we are working with

trout_red <- trout_last5yrs %>% 
  select(Season, Name, Team, wRC_plus, RE24, WAR)

# melt the data, so every row is a player, season, individual variable, and that variable's value

trout_red_melt <- trout_red %>% 
  gather(variable, value, -Season, -Name, -Team)

# rename variables

trout_red_melt$variable <- with(trout_red_melt, ifelse(variable == "wRC_plus", "wRC+", variable))

# rank hitters based on year and metric

trout_red_melt <- trout_red_melt %>% 
  group_by(Season, variable) %>% 
  arrange(Season, variable, desc(value)) %>% 
  mutate(rank = row_number(variable)) %>% 
  ungroup() 

## slopegraph faceted by metric

trout_golfscore_year <- trout_red_melt %>% select(-value) %>% spread(key = variable, value = rank) 

trout_golfscore_year$golf_score <- rowSums(trout_golfscore_year[, c(4:6)], na.rm = TRUE)

trout_golfscore_year <- trout_golfscore_year %>% group_by(Season) %>% arrange(golf_score) %>% mutate(golf_score_rank = row_number())

trout_slope <- trout_golfscore_year %>% filter(golf_score_rank <= 10)

trout_slope_melt <- trout_slope %>% gather(variable, value, -Season, -Name, -Team) %>% filter(variable != "golf_score_rank") %>% filter(variable != "golf_score")

trout_slope_top10 <- trout_slope_melt %>% ungroup()

trout_slope_top10$value <- factor(trout_slope_top10$value, levels = rev(seq(1:34)))

trout_slope_years_alone <- trout_slope_top10 %>% filter(Name == "Mike Trout")
trout_slope_top10_noTrout <- trout_slope_top10 %>% filter(Name != "Mike Trout")

ggplot(trout_slope_top10_noTrout, aes(factor(variable), value, 
                                        group = Name,
                                        color = Team,
                                        label = Name)) +
  geom_point(size = 3, alpha = .75) +
  geom_point(data = trout_slope_years_alone, aes(factor(variable), value), size = 3) +
  geom_line(size = .5, alpha = .25) +
  geom_line(data = trout_slope_years_alone, aes(factor(variable), value), linetype = "dashed", size = .5) +
  geom_text(data = subset(trout_slope_top10_noTrout, variable == "wRC+"), aes(label = paste0("    ", Name)), fontface = "bold", hjust = 0, size = 3.5) +
  geom_text(data = subset(trout_slope_years_alone, variable == "wRC+"), aes(label = paste0("    ", Name)), fontface = "bold", hjust = 0, size = 3.5) +
  geom_text(data = subset(trout_slope_top10_noTrout, variable == "RE24"), aes(label = paste0("    ", Name)), fontface = "bold", hjust = 1.1, size = 3.5) +
  geom_text(data = subset(trout_slope_years_alone, variable == "RE24"), aes(label = paste0("    ", Name)), fontface = "bold", hjust = 1.1, size = 3.5) +
  ggtitle("\n   Comparing the Rank of MLB Players Across Key Metrics: 2012-16\n") + 
  labs(subtitle = "Ordered Highest to Lowest\n", caption = "\n\nCreated by @BillPetti\nData acquired using the baseballr package\nData courtesy of FanGraphs.com") +
  scale_colour_manual(values = mlb_team_colors) +
  facet_wrap(~Season, nrow = 2) +
  ylab("\nRank\n") +
  theme_bp_grey() +
  scale_x_discrete(expand = c(1.2, 1.2)) +
  theme(legend.position = "none",
        strip.text.x = element_text(face = "bold", size = 14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank(), 
        plot.subtitle = element_text(face = "bold", size = 12, hjust = .02))

# save plot to working directory

ggsave("trout_slopegraph_by_season.png", scale = 1.1, height = 8.5, width = 14, units = "in")

ggplot(trout_slope_top10_noTrout, aes(factor(Season), value, 
                                      group = Name,
                                      color = Team,
                                      label = Name)) +
  geom_point(size = 3, alpha = .75) +
  geom_point(data = trout_slope_years_alone, aes(factor(Season), value), size = 3) +
  geom_line(size = .5, alpha = .25) +
  geom_line(data = trout_slope_years_alone, aes(factor(Season), value), linetype = "dashed", size = .5) +
  geom_text(data = subset(trout_slope_top10_noTrout, Season == 2016), aes(label = paste0("    ", Name)), fontface = "bold", hjust = 0, size = 3.5) +
  geom_text(data = subset(trout_slope_years_alone, Season == 2016), aes(label = paste0("    ", Name)), fontface = "bold", hjust = 0, size = 3.5) +
  geom_text(data = subset(trout_slope_top10_noTrout, Season == 2012), aes(label = paste0(Name, "    ")), fontface = "bold", hjust = 1, size = 3.5) + 
  geom_text(data = subset(trout_slope_years_alone, Season == 2012), aes(label = paste0(Name, "    ")), fontface = "bold", hjust = 1, size = 3.5) +
  ggtitle("\n   Comparing the Rank of MLB Players Across Key Metrics: 2012-16\n") + 
  labs(subtitle = "Ordered Highest to Lowest\n", caption = "\n\nCreated by @BillPetti\nData acquired using the baseballr package\nData courtesy of FanGraphs.com") +
  scale_colour_manual(values = mlb_team_colors) +
  facet_wrap(~variable) +
  ylab("\nRank\n") +
  theme_bp_grey() +
  scale_x_discrete(expand = c(1.2, 1.2)) +
  theme(legend.position = "none",
        strip.text.x = element_text(face = "bold", size = 14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size = 9.5), 
        axis.ticks=element_blank(),
        axis.line=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank(), 
        plot.subtitle = element_text(face = "bold", size = 12, hjust = .02))

# save plot to working directory

ggsave("trout_slopegraph_by_variable.png", scale = 1, height = 8.5, width = 14, units = "in")

