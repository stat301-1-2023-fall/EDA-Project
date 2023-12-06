library(tidyverse)
library(knitr)
library(maps)
library(purrr)
install.packages("kableExtra")
library(kableExtra)
eda_data <- read_csv("data/eda_data.csv")

#How has the frequency of terrorist attacks changed from 1970 to 2017?
plot1 <- eda_data |>
  ggplot(aes(x = year))+
  geom_histogram(fill = "skyblue")+
  labs(title = "Distribution of Terroist Attacks by Year",
       x = "Year", y = "Count")
ggsave("figures/plot1.png", plot1)

#Are there more attacks if a country has a higher or a lower Polity score?
plot2<- eda_data |>
  filter(polity > -11) |>
  mutate(polity = as.factor(polity)) |>
  ggplot(aes(x = polity))+
  geom_bar(fill = "darkblue") +
  labs(title = "Distribution of Terroist Attacks by Polity Score",
       x = "Polity Score", y = "Count")
ggsave("figures/plot2.png", plot2)

#Where do most terrorist attacks occur?
table1 <- eda_data |>
  mutate(region = region_txt)|>
  group_by(region) |>
  summarise(attack_count = n()) |>
  kable("html", booktabs = TRUE) 
writeLines(table1, file.path("figures", "table1.html"))

world_map <- map_data("world")
unique_countries_eda_data <- unique(eda_data$country_name)
unique_countries_world_map <- unique(world_map$region)
countries_not_in_world_map <- setdiff(unique_countries_eda_data, unique_countries_world_map)
country_mapping <- data.frame(
  country_name = c("United States", "United Kingdom", "Czechoslovakia", "South Vietnam", "Slovak Republic"),
  map_country_name = c("USA", "UK", "Czech Republic", "Vietnam", "Slovakia"))
eda_data_corrected <- merge(eda_data, country_mapping, by = "country_name", all.x = TRUE)
eda_data_corrected$map_country_name <- ifelse(is.na(eda_data_corrected$map_country_name),
                                              eda_data_corrected$country_name, 
                                              eda_data_corrected$map_country_name)
df_frequency <- eda_data_corrected |>
  count(map_country_name)
df_mapped <- merge(world_map, df_frequency, by.x = "region", by.y = "map_country_name", all.x = TRUE)
world_map <- map_data("world")

plot3<- ggplot(df_mapped, aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon()+
  labs(fill = "Number of Attacks", title = "Frequency of Terrorist Attacks by Country")
ggsave("figures/plot3.png", plot3)

#polity score by region

eda_data |>
  mutate(polity = as.integer(polity))
  group_by(region_txt)|>
  g


