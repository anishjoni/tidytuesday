###################
# Required Packages
###################
require(dplyr)
require(RCurl)
require(ggplot2)
require(tidyr)
require(forcats)
require(stringr)
require(plotly)
require(webshot)

##################
# Data Processing
##################
options(scipen=999)
data <- read.csv(text = getURL("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week4_australian_salary.csv"), header = T)

# Top 10 Occupations(Male and Female combined)
top_male_occupations <- head(filter(data) %>%
                        group_by(gender) %>% 
                        arrange(desc(average_taxable_income)),10)["occupation"]

top_female_occupations <- head(filter(data, gender == "Female") %>% 
                          group_by(gender) %>% 
                          arrange(desc(average_taxable_income)),10)["occupation"]

common_top_occupations <- union(top_male_occupations$occupation, top_female_occupations$occupation)

top_14 <- data %>% 
  filter(occupation %in% common_top_occupations) %>% 
  select(occupation, gender, average_taxable_income) %>% 
  spread(key = gender, value = average_taxable_income)


# Final data
final_df <- filter(data, occupation %in% common_top_occupations) # Filtering only to 14 different occupations among top 10 male and female occuaptions
final_df$occupation <- str_replace(final_df$occupation, "Plastic and reconstructive surgeon", "P & R Surgeon")
final_df <- separate(final_df, "occupation", "occupation", sep = ";") 
final_df$average_taxable_income <- as.numeric(as.character(final_df$average_taxable_income))

# Data for dumbell chart
top_14$occupation <- iconv(top_14$occupation, "", "ASCII", "byte")
top_14$occupation <- str_replace(top_14$occupation, " \026", "")
top_14$occupation <- str_replace(top_14$occupation, "Plastic and reconstructive surgeon", "P & R Surgeon")
top_14 <- separate(top_14, "occupation", "occupation", sep = ";") 

#######
# Plots
#######

#1. Grouped bar graph of Top 10 high paying occupations in Australia Male/Female
bars <- ggplot(final_df, aes(fct_reorder(occupation, average_taxable_income, .desc = TRUE), y = average_taxable_income, fill = gender)) + 
  geom_bar(stat = "identity", position = "dodge" ) +
  xlab("Occupation") +
  ylab("Avg. Taxable Income ($)") + 
  ggtitle("Top 10 high paying occupations in Australia", "For both Men and Women combined") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("week4_plot1.jpg", plot = bars)

#2. Dumbell chart of income differences
x <- list(
  title = "Avg. Taxable Income ($)",
  showgrid = FALSE
)
y <- list(
  title = "",
  showgrid = FALSE
)

p <- plot_ly(top_14, color = I("gray85")) %>%
  add_segments(x = ~Female, xend = ~Male, y = ~occupation, yend = ~occupation, showlegend = FALSE) %>% 
  add_markers(x = ~Female, y = ~occupation, name = "Female", color = I("pink")) %>%
  add_markers(x = ~Male, y = ~occupation, name = "Male", color = I("blue")) %>%
  layout(
    title = "Gender Income disparity in Top 10 high paying occupations in Australia",
    xaxis = x, yaxis = y,
    margin = list(l = 165)
  )
p
