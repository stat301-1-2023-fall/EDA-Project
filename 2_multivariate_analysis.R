#Visualizing the distribution of durable and Polity score across successful and unsuccessful attacks
ggplot(eda_data, aes(x = as.factor(success), y = durable)) +
  geom_boxplot() +
  coord_flip()+
  labs(title = "Regime Durability by Success of Attacks", x = "Success", y = "Regime Durability")
eda_data |>
  filter(polity > -11) |>
  ggplot(aes(x = as.factor(success), y = polity)) +
  geom_boxplot() +
  coord_flip()+
  labs(title = "Polity Score by Success of Attacks", x = "Success", y = "Polity Score")
#For attacks that are successful, the average durability of the country is lower for attacks that are unsuccesful.A country is more durable the longer it has been since a regime change which mean more durable countries are likely to have better infrastructure and communication through the country. This means they are better prepared to predict attacks, handle attacks, and send out first responders when needed. Successful attacks occur on average in a country with a lower Polity score. Since countries that have a higher Polity score are more democratic, the policies and authority in these countries tend to focus on protecting the public and civilians more than in autocratic governments.

#How do polity score and durable score together influence the success of an attack?
eda_data_filtered <- eda_data %>% 
  filter(polity > -11) |>
  filter(!is.na(success), !is.na(polity), !is.na(durable))

model <- glm(success ~ polity + durable, data = eda_data_filtered, family = "binomial")

eda_data_filtered$predicted_success <- predict(model, type = "response")

eda_data_filtered$success_group <- cut(eda_data_filtered$predicted_success, 
                                       breaks = seq(0.75, 0.95, by = 0.02), 
                                       include.lowest = TRUE)

ggplot(eda_data_filtered, aes(x = polity, y = durable, color = as.factor(success_group))) +
  geom_point(alpha = 0.5) +
  labs(title = "Predicted Probability of Success of a Terroist Attack by Polity Score and Durable Score", x = "Polity Score", y = "Durable Score")

## Exploring Severity of Terroist Attacks by Weapon Type
#Severity can be defined as rate of success and number of people killed and wounded by an attack

#Does the type of weapon a terrorist chooses influence how likely they are to be succesful? 
eda_data |>
  mutate(success = as.factor(success)) |>
  ggplot(aes(x = weaptype1_txt, fill = success))+
  geom_bar()+
  facet_wrap(~weaptype1_txt, scales = "free") +
  labs(title = "Success Rate of Terroist Attacks by Weapon Type",
       x = "Weapon Type", y = "Count")+
  scale_fill_manual(values = c("0" = "darkblue", "1" = "skyblue"),
                    labels = c("Unsuccessful", "Sucessful"))
#It appears that an attacker's weapon of choice could have a very big impact on whether or not they will be successful. The weapons with the highest rate of success ppear to be attacks using. incendiaries. sabotage equipment, and melee. It is interesting to note that fake weapons has a higer success average than using biological weapons and radiological weapons. This may be due to the difficulty of creating an effective biological/ radiological weapon- advanced science knowledge would be required and resources, such as dangerous viruse and radioactive material, are hard to obtain. 
#From this, we can also tell that the most common weapon choice among terrorist attacks are explosives and the least common are radiological. It is interesting to note that explosives have a very high rate of success while radiological weapons have an extremely low rate of success.

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










  