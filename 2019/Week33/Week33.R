# Required Packages
require(tidyverse)
require(magrittr)
require(RCurl)
require(DataExplorer)
require(gganimate)
require(ggthemes)

# Gettin the Data
malaria <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-13/malaria_deaths_age.csv")

death_sum <- malaria[,-which(names(malaria) %in% c("X1","year"))] %>% 
  filter(!str_detect(entity, pattern = c("Sub-Saharan|Middle East|World|Low SDI
                                          |SDI|Asia"))) %>%
  group_by( entity, code, age_group) %>% 
  summarise_all(funs(sum)) %>% 
  arrange(desc(deaths))

# Plots

# 1. Top 10 countries with Most Deaths caused by from 1990 till 2017
  top_5 <- malaria %>% 
    filter(entity %in% head(death_sum$entity,5)) %>% 
    mutate(entity = str_replace(entity, "Democratic Republic of Congo", "Congo")) %>% 
    select(entity, year, deaths) %>% 
    group_by(entity, year) %>% 
    summarise(total_deaths = sum(deaths)) %>% 
    ggplot(aes(year, total_deaths)) +
    geom_line(aes(colour = entity), size = 2) +
    theme_fivethirtyeight() +
    geom_segment(aes(xend = 2016, yend = total_deaths, colour = entity), linetype = 2) +
    geom_point() +
    geom_text(aes(x = 2018, label = entity, colour = entity, size = 2)) +
    scale_color_viridis_d() +
    scale_y_continuous(breaks = seq(0, 350000, by = 50000),
                       labels = c("0", "50", "100", "150",
                                  "200", "250", "300", "350")) +
    scale_x_continuous(limits = c(1990, 2025),
                       breaks = seq(1990, 2020, by = 5),
                       labels = seq(1990, 2020, by = 5)) +
    labs(title = "The Five Countries with most Malaria Deaths", subtitle = 'From 1990 till 2016', caption = "Tidytuesday | #rstats | week 33", y = 'Deaths (thousands)') +
    theme(legend.position = "none", axis.title = element_text(), axis.title.x = element_blank()) +
    transition_reveal(id = entity, year) 
  
  # Save plot  
  anim_save("malaria_deaths.gif", animation = last_animation())
    
  