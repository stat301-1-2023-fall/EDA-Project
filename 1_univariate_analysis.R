#How has the frequency of terrorist attacks changed from 1970 to 2017?
#Looking at the histogram, the distribution of terroist attacks over time is skewed to the right, which means terroist attack have become more common as time passes. It is interesting to note that terroist attacks seem to have started decreasing after 1990 but increase drastically after 2001. 

eda_data |>
  ggplot(aes(x = year))+
  geom_histogram(fill = "skyblue")+
  labs(title = "Distribution of Terroist Attacks by Year",
       x = "Year", y = "Count")

#Are there more attacks if a country has a higher or a lower Polity score?
#This bar chart shows that most terroist attacks do not happen in countries with a very low or very high Polity score. Countries with Polity score of 6 have the most terrorist attacks in total. Since very developed countries tend to have a high polity score and very underdeveloped countries have a low polity score, these countries in the middle may be the ones that have several groups fighting for leadership and that don't have extremely advanced technology to defend themselves agaisnt attackers.

eda_data |>
  filter(polity > -11) |>
  mutate(polity = as.factor(polity)) |>
  ggplot(aes(x = polity))+
  geom_bar(fill = "darkblue") +
  labs(title = "Distribution of Terroist Attacks by Polity Score",
       x = "Polity Score", y = "Count")

#Where do most terrorist attacks occur?

eda_data |>
  group_by(region_txt) |>
  summarise(n_attacks = n()) |>
  kable("html", booktabs = TRUE) 

#To visizualize the frequncy of terrorist attacks by country
#| echo: false
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

ggplot(df_mapped, aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon()+
  labs(fill = "Number of Attacks", title = "Frequency of Terrorist Attacks by Country")

#Basic overview of the distribution of successful and unsuccessful attacks
table(eda_data$success) |>
  kable("html", booktabs = TRUE) 
eda_data |>
  ggplot(aes(x = success))+
  geom_bar(fill = "skyblue")+
  labs(title = "Count of Successful and Unsuccesful Terrorist Attacks")
#There are many more unsuccessful attacks than there are succesful ones.
#Comparing the mean values of durable and Polity score for successful and unsuccessful attacks. This will give an idea of whether there is a noticeable difference in political stability factors based on the success of the attacks
