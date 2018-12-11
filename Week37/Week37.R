#######################
#  Required packages  #
#######################
library(tidyverse)
library(janitor)
library(zipcode)
library(magrittr)


# Data --------------------------------------------------------------------
set.seed(20181209)

df <- read_csv("https://data.cityofnewyork.us/api/views/43nn-pn8j/rows.csv")

# Cleaning names with janitor, sampling 300,000 records, and dropping some variables
NYC_df <- df %>% 
  janitor::clean_names() %>%
  select(-phone, -grade_date, -record_date, -building, -street) %>% 
  sample_n(size = 300000)

# save the .csv
write_csv(NYC_df, "nyc_restaurants.csv")

# Extracting Geo data for regions in NY state
zipcode %<>% 
  filter(state == "NY")

zipcode_df <- tbl_df(zipcode)

zipcode_df %<>% 
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



