# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-12-07')
# tuesdata <- tidytuesdayR::tt_load(2021, week = 50)

spiders <- tuesdata$spiders




# load libraries

library(magrittr)
library(dplyr)
library(ggplot2)

# Explore the data
spiders %>%
  dplyr::count(family, genus, sort = T)


# Plot the data

# IDEAS:
# - make a time-lapse map plot that shows frequency of spider
#   discoveries over time throughout the world?


# there's no lat/long cols, so I'll join this with a df
lat_longs <- map_data("world") %>%
  dplyr::select(long, lat, region) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(region = dplyr::first(region),
                   lat = dplyr::first(lat),
                   long = dplyr::first(long))

# join with cols of spider df I care about
spider_map_data <- dplyr::left_join(x = (spiders %>%
                                       dplyr::select(3,4,5,8,10) %>%
                                       dplyr::rename(., region = distribution)),
                                     y = lat_longs,
                                     by = "region") %>%
  na.omit() %>%
  # summarize: # species found in each country by year
  dplyr::group_by(year, region) %>%
  dplyr::summarise(num_species = dplyr::n(),
                   lat = dplyr::first(lat),
                   long = dplyr::first(long)) %>%
  dplyr::arrange(year, region)

spider_map_data_decades <- spider_map_data %>%
  mutate(decade = floor(year/10)*10) %>% 
  group_by(decade) %>% 
  select(-year) %>%
  group_by(decade, region) %>%
  summarize(num_species = sum(num_species),
            lat = first(lat),
            long = first(long))

library(ggplot2)
library(maps)
library(ggthemes)
# install.packages("gifski)
# install.packages("gganimate")
library(gganimate)
library(gifski)

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)

map <- world +
  geom_point(aes(x = long, y = lat, size = num_species),
             data = spider_map_data_decades, 
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 10), 
                        breaks = c(1, 5, 25, 100, 500, 1000, 1300)) +
  gganimate::transition_time(decade) +
  labs(title = 'Decade: {frame_time}', size = 'Spider Species Discovered')


animate(map, renderer = gifski_renderer())
