## This script looks to harvest schedule data from : http://passion-soccer.com/calendrier-division-3/ 
## and produce a visualization showing the differences in schedule assignation amongst teams

# 0) Libraries
library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)

# 1) Import data from website
df <- html("http://passion-soccer.com/calendrier-division-3/")

# 2) Look for specific nodes
df_team <- df %>% 
  html_nodes(".commands") %>% 
  html_text()

df_date <- df %>% 
  html_nodes(".date") %>% 
  html_text()

df_time <- df %>% 
  html_nodes(".time") %>% 
  html_text()

df_stadium <- df %>% 
  html_nodes(".stadium") %>% 
  html_text()

# 3) Clean the resulting strings
## Team
df_team <- gsub(pattern = "\n                                            ",
     replacement = "",
     x = df_team)

df_team <- substr(x = df_team,
       start = 1,
       stop = str_locate(string = df_team,
                         pattern = "\n                                         ")[, 1] - 1)

df_team <- gsub(pattern = '[[:digit:]]+', 
     replacement = '', 
     x = df_team)

df_team <- gsub(pattern = ':', 
     replacement = '-', 
     x = df_team)

df_team <- data.frame(teams = df_team) # Transform to data frame

df_team <- df_team %>%         # Split data frame in two columns
  tidyr::separate(col = teams, 
                  into = c("home_team", "away_team"),
                  sep = " - ") %>% 
  mutate(home_team = trimws(home_team),
         away_team = trimws(away_team))

## Date
df_date <- data.frame(date = df_date) %>% 
  mutate(date = trimws(x = date,
                       which = "both")) %>% 
  tidyr::separate(col = date,
                  into = c("jour", "mois"),
                  sep = "                        ") %>% 
  mutate(date = dmy(paste(jour, mois, 2020, sep = " "))) %>% 
  mutate(date = if_else(month(date) >= 10,
                        date - years(1),
                        date))

## Time
df_time <- data.frame(time = trimws(x = df_time,
                         which = "both")) %>% 
  tidyr::separate(col = time,
                  into = c("heure", "minute"),
                  sep = ":") %>% 
  mutate(time = fct_reorder(paste(heure, minute, sep = ":"),
                            heure)) %>% 
  mutate(heure = as.numeric(heure) + 12,
         minute = as.numeric(minute))


## Stadium
df_stadium <- data.frame(terrain = df_stadium)

# 4) Consolidate results
df <- cbind(df_team,
            df_date,
            df_time,
            df_stadium)

# 5) Plot resulting data frame
g1 <- df %>% 
  select(home_team, time) %>% 
  rename(team = home_team) %>% 
  rbind(df %>% 
          select(away_team, time) %>% 
          rename(team = away_team)) %>%  # Reformat the data frame to a longer format
  group_by(team) %>%
  group_by(team, time) %>%               # Calculate the frequencies per group
  summarise(time_n = n()) %>% 
  filter(sum(time_n) >= 20) %>%          # Filter only teams with a full schedule
  group_by(time) %>% 
  mutate(mean_n = mean(time_n)) %>%      # Calculate the mean frequency per group
  ggplot(aes(x = time, 
             y = time_n,
             fill = time)) +
  geom_bar(stat = "identity",            # Main bar chart
           color = "black",
           alpha = 0.8) +
  geom_text(aes(label = time_n),         # Labels for frequencies
            nudge_y = 1,
            size = 8) +
  geom_errorbar(aes(ymin = mean_n,       # Horizontal lines for mean values
                    ymax = mean_n),
                size = 2,
                alpha = 0.5,
                linetype = "dotted") +
  geom_text(aes(label = mean_n,          # Text for mean values
                y = mean_n + 0.4),
            nudge_y = 0,
            size = 3) +
  facet_wrap(~team) +                    # Per team facets
  scale_fill_brewer(palette = "Dark2") + # Why not
  theme_bw() +
  scale_y_continuous(breaks = 1:10) +    # Better scaling option
  ggtitle(label = "Distributions fréquentielles des assignations hebdomadaires Passion-Soccer D3 - Valeurs moyennes incluses") +
  xlab("Heure de la partie") +
  ylab("Fréquence de la partie") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20,
                                  face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16,
                                  face = "bold"))        # Fine tuning chart

## Output plot
ggsave(file=".\\plot.svg", plot=g1, width=12*1.5, height=8*1.5)
