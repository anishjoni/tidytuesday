# Required Packages
require(tidyverse)
require(magrittr)
require(RCurl)
require(DataExplorer)
require(gganimate)

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
  top_10 <- malaria %>% 
    #filter(entity %in% head(death_sum$entity,10)) %>% 
    mutate(year = as.double(year)) %>% 
    filter(entity == "India") %>% 
    ggplot(aes(year, deaths)) +
    geom_line(aes(colour = entity), size = 2) +
    #geom_segment(aes(xend = 2017, yend = deaths, colour = entity), linetype = 2) +
    geom_point() +
    scale_color_viridis_d() +
    scale_y_continuous(breaks = seq(0, 350000, by = 50000),
                       labels = c("0", "50", "100", "150",
                                  "200", "250", "300", "350")) +
    scale_x_continuous(limits = c(1990, 2025),
                       breaks = seq(1990, 2015, by = 5),
                       labels = seq(1990, 2015, by = 5)) +
    labs(title = 'Year:{frame}', x = 'Years', y = 'Deaths') +
    theme_minimal() +
    theme(legend.position = "none") 
  
  +
    transition_reveal(id = entity, year) 
    
  test_data <-
    data.frame(
      var0 = 100 + c(0, cumsum(runif(49, -20, 20))),
      var1 = 150 + c(0, cumsum(runif(49, -10, 10))),
      date = seq(1891, 1990, by = 1)
    )
  
test_data <- tbl_df(test_data)


ggplot(test_data, aes(date)) + 
  geom_line(aes(y = var0, colour = "var0")) + 
  geom_line(aes(y = var1, colour = "var1"))
