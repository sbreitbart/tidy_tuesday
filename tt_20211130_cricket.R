# Get the Data--------------

# Read in with tidytuesdayR package 
# Install from CRAN via:
install.packages("tidytuesdayR")
library(tidytuesdayR)
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-11-30')
# tuesdata <- tidytuesdayR::tt_load(2021, week = 49)

matches <- tuesdata$matches




# Explore data-----------

colnames(matches)
str(matches)
# What would be interesting to plot? We have individual matches, team names, 
# each team's score per match, whether they were home/away, series,
# the winner, best player per match, date...


## Questions I could ask and then plot the answers to:
### 1. Who are the best players for the nail-biter matches (those where the margin
###    is less than 10?)



# Make figures--------------

## 1. Best players under pressure
### Methodology: each player is a circle, size determined by # of games,
### color determined by country, and on x axis is the margin

### Filter dataset with best players under pressure
high_pressure <- matches %>%
  filter(margin <= 5) %>%
  dplyr::group_by(player_of_match) %>%
  dplyr::summarise(., num_games = n(),
                   country = first(player_of_match_team))



# Libraries
library(ggplot2)
library(dplyr)


# bubble plot
ggplot(high_pressure,
       aes(x = num_games,
           y = country,
           color = country)) +
  geom_point(alpha=0.7) +
  geom_label_repel(data = subset(high_pressure, num_games > 4),
                   aes(num_games, country, label = player_of_match),
                   size = 3) +
  theme(legend.position = "none") +
  ggtitle("Cricket Players who are Cool under Pressure",
          subtitle = "'Players of the Match' who have won games for their teams with less than a 6 run margin") +
  xlab("Number of Games Won") +
  ylab("Player of Match's Country")



# NEXT STEPS:
# use ridgeline plot instead: https://www.r-graph-gallery.com/294-basic-ridgeline-plot.html
