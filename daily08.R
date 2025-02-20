# Load required libraries
library(tidyverse)

# Load COVID data
url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid <- read_csv(url)

df_region <- data.frame(
  state_abb = state.abb,
  state_name = state.name,
  region = state.region
)

covid_with_region <- covid %>%
  left_join(df_region, by = c("state" = "state_name")) %>%
  filter(!is.na(region))  

covid_summary <- covid_with_region %>%
  arrange(region, date) %>%
  group_by(region, date) %>%
  summarise(
    daily_cases = sum(cases, na.rm = TRUE),
    daily_deaths = sum(deaths, na.rm = TRUE)
  ) %>%
  mutate(
    cumulative_cases = cumsum(daily_cases),
    cumulative_deaths = cumsum(daily_deaths)
  ) %>%
  ungroup()

covid_long <- covid_summary %>%
  pivot_longer(cols = c(cumulative_cases, cumulative_deaths),
               names_to = "variable",
               values_to = "value")

p <- ggplot(covid_long, aes(x = date, y = value, color = variable)) +
  geom_line(na.rm = TRUE) +
  facet_grid(variable ~ region, scales = "free_y") +
  labs(
    title = "Cumulative Cases and Deaths: Region",
    subtitle = "COVID-19 Data: NY-Times",
    x = "Date",
    y = "Daily Cumulative Count"
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +  
  scale_y_continuous(labels = scales::comma) + 
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5) 
  )

print(p)

ggsave("img/covid_region_plot.png", plot = p, width = 8, height = 6)
