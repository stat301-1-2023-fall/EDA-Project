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

#This will give an idea of whether there is a noticeable difference in political stability factors based on the success of the attacks
table2<- eda_data |>
  filter(polity > -11) |>
  mutate(outcome = ifelse(success == 0, "unsuccessful", "successful")) |>
  group_by(outcome) |>
  summarise(
    avg_durable = mean(durable, na.rm = TRUE),
    avg_polity = mean(polity, na.rm = TRUE)
  ) |>
  kable("html", booktabs = TRUE) 
writeLines(table2, file.path("figures", "table2.html"))

#Durability across successful and unsuccessful attacks
plot5 <- ggplot(eda_data, aes(x = as.factor(success), y = durable, fill = as.factor(success))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "hotpink")) +
  coord_flip()+
  labs(fill = "Outcome", title = "Regime Durability by Success of Attacks", x = "Outcome where 0 is unsucessful and 1 is successful", y = "Regime Durability")
ggsave("figures/plot5.png", plot5)

#Polity across successful and unsuccessful attacks
plot6<- eda_data |>
  filter(polity > -11) |>
  ggplot(aes(x = as.factor(success), y = polity, fill = as.factor(success))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "hotpink")) +
  coord_flip()+
  labs(fill = "Outcome", title = "Polity Score by Success of Attacks", x = "Outcome where 0 is unsucessful and 1 is successful", y = "Polity Score")
ggsave("figures/plot6.png", plot6)

#How do polity score and durable score together influence the success of an attack?
eda_data_filtered <- eda_data %>% 
  filter(polity > -11) |>
  filter(!is.na(success), !is.na(polity), !is.na(durable))
model <- glm(success ~ polity + durable, data = eda_data_filtered, family = "binomial")
eda_data_filtered$predicted_success <- predict(model, type = "response")
eda_data_filtered$success_group <- cut(eda_data_filtered$predicted_success, 
                                       breaks = seq(0.75, 0.95, by = 0.02), 
                                       include.lowest = TRUE)
eda_data_filtered<- eda_data_filtered |>
  mutate(success_group = as.factor(success_group))|>
  mutate(success_probability = success_group)

plot7 <- ggplot(eda_data_filtered, aes(x = polity, y = durable, color =success_probability)) +
  geom_point(alpha = 0.5) +
  labs(title = "Predicted Probability of Success of a Terroist Attack by Political Stability", x = "Polity Score", y = "Durable Score")
ggsave("figures/plot7.png", plot7)

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

plot8 <- ggplot(long_seg, aes(x = segment, y = count, color = outcome, group = outcome)) +
  geom_line()+
  labs(title = "Successes and Failures of Terroist Attacks Over Time", 
       x = "Time Period")+
  scale_color_manual(values = c("successes" = "skyblue", "failures" = "darkblue"))+
  coord_flip()
ggsave("figures/plot8.png", plot8)


#Do the average amount of people killed and wounded vary among which weapon an attacker chose?
table3<- eda_data |>
  mutate(killed_wounded = nkill + nwound, weapon = weaptype1_txt) |>
  group_by(weapon) |>
  summarise(avg_kill = mean(nkill, na.rm = TRUE),
            avg_wound = mean(nwound, na.rm = TRUE),
            avg_casualties = mean(killed_wounded, na.rm = TRUE)) |>
  arrange(desc(avg_casualties)) |>
  kable("html", booktabs = TRUE) 
writeLines(table3, file.path("figures", "table3.html"))

#Does the type of weapon a terrorist chooses influence how likely they are to be succesful? 
plot9 <- eda_data |>
  mutate(success = as.factor(success)) |>
  mutate(weaptype1_txt = ifelse(weaptype1_txt == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)", "Vehicle", weaptype1_txt)) |>
  filter(!(weaptype1_txt == "Unknown")) |>
  filter(!(weaptype1_txt == "Other")) |>
  ggplot(aes(x = weaptype1_txt, fill = success))+
  geom_bar()+
  facet_wrap(~weaptype1_txt, scales = "free") +
  labs(title = "Success Rate of Terroist Attacks by Weapon Type",
       x = "Weapon Type", y = "Count")+
  scale_fill_manual(values = c("0" = "darkblue", "1" = "skyblue"),
                    labels = c("Unsuccessful", "Sucessful"))
ggsave("figures/plot9.png", plot9)

#hostage attacks over time
plot10<- eda_data |>
  group_by(year)|>
  summarize(hostage = sum(ishostkid, na.rm = TRUE)) |>
  ggplot(aes(x = year, y = hostage))+
  geom_point()+
  labs(title = "Distribution of the Number of Terror Attacks Involving Hostages", 
       x = "Year", y = "Number of Attacks Involving Hostages")
ggsave("figures/plot10.png", plot10)

#hostages table
table4 <- eda_data |>
  filter(!is.na(ishostkid))|>
  mutate(ishostkid = ifelse(ishostkid == 0, "no hostages", "hostages")) |>
  mutate(hostage = as.factor(ishostkid))|>
  group_by(hostage) |>
  summarize(success_rate = mean(success, na.rm = TRUE)) |>
  kable("html", booktabs = TRUE) 
writeLines(table4, file.path("figures", "table4.html"))




