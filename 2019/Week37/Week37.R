#######################
#  Required packages  #
#######################
library(tidyverse)
library(janitor)
library(magrittr)


# Data --------------------------------------------------------------------
set.seed(20181209)

#df <- read_csv("https://data.cityofnewyork.us/api/views/43nn-pn8j/rows.csv")
setwd("~/GitHub/Tidy Tuesday/Week37")
NYC_df <- read_csv("nyc_restaurants.csv")

# Cleaning names with janitor, sampling 300,000 records, and dropping some variables
# NYC_df %<>%
#   janitor::clean_names() %>%
#   select(-phone, -grade_date, -record_date, -building, -street) %>%
#   sample_n(size = 300000)

# save the .csv
# write_csv(NYC_df, "nyc_restaurants.csv")

# Extracting Geo data for regions in NY state
data("zipcode")

zipcode_df <- zipcode %>% 
  tbl_df() %>% 
  filter(state == "NY") %>% 
  mutate(zip = as.integer(zip))

# EDA ---------------------------------------------------------------------

NYC_df
summary(NYC_df)
NYC_df[is.na(NYC_df$grade),12] <- 0 # NAs to 0

# Plots -------------------------------------------------------------------

NYC_df %>% 
  select(zipcode, cuisine_description, score, grade) %>% 
  inner_join(zipcode_df, by = c("zipcode" = "zip")) %>% 
  ggplot() +
  geom_polygon(aes(fill = score,  x=longitude, y=latitude))



