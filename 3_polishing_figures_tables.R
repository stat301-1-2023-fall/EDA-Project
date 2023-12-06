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

#Basic overview of the distribution of successful and unsuccessful attacks
plot4<- eda_data |>
  mutate(success = as.factor(success))|>
  mutate(success = ifelse(success == 0, "unsuccessful", "successful"))|>
  ggplot(aes(x = success))+
  geom_bar(fill = "skyblue")+
  labs(title = "Count of Successful and Unsuccesful Terrorist Attacks", x = "")
ggsave("figures/plot4.png", plot4)





#weaponry
#Successes and Failures of Terroist Attacks Over Time
seg_data <- eda_data |>
  mutate(segment = cut(year, breaks = seq(1969, 2020, by = 5)))
summary_seg <- seg_data |>
  group_by(segment) |>
  summarize(successes = sum(success == 1),
            failures = sum(success == 0))
long_seg <- summary_seg |>
  pivot_longer(cols = c(successes, failures), names_to = "outcome", values_to = "count")

ggplot(long_seg, aes(x = segment, y = count, color = outcome, group = outcome)) +
  geom_line()+
  labs(title = "Successes and Failures of Terroist Attacks Over Time", 
       x = "Time Period")+
  coord_flip()
# greater disparty. govt is no tkeeping up w groups. serious. suggests tactics ned to evolve. rig
#right noe reactionary. should focus on preventon tactics


