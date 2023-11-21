#loading packages
library(tidyverse)
library(naniar)
library(haven)

#importing data
terrorism_data <- read_csv("data/raw/globalterrorismdb_0718dist.csv")
polity_data <- read_sav("data/raw/p5v2018.sav")

#calculate percentage of data that is missing
missing_percentages_terrorism <- colSums(is.na(terrorism_data)) / nrow(terrorism_data) * 100
missing_percentages_polity <- colSums(is.na(polity_data)) / nrow(polity_data) * 100

#dropping columns that consists of 20% or more missing values
terrorism_drop <- names(missing_percentages_terrorism[missing_percentages_terrorism > 50])
polity_drop <- names(missing_percentages_polity[missing_percentages_polity > 50])

terrorism_data <- terrorism_data |>
  select(-all_of(terrorism_drop))

polity_data <- polity_data |>
  select(-all_of(polity_drop))

#terrorism dataset

#adding a new date column 
terrorism_data <- terrorism_data |>
  mutate(date = as.Date(paste(iyear, imonth, iday, sep = "-"))) |>
  select(date, everything())
#dropping any entries that have a year prior to 1776 (earliest year in the polity dataset)
terrorism_data <- terrorism_data |>
  filter(iyear > 1775)
#dropping any events that had an unknown country of occurence
terrorism_data <- terrorism_data |>
  filter(!is.na(country_txt))
#changing the column names to match
terrorism_data <- terrorism_data |>
  mutate(year = iyear,
         month = imonth,
         day = iday) |>
  select(date, year, month, day, everything()) |>
  select(-iyear, -imonth, -iday)


#cleaning the polity dataset 
#dropping any entries after 2017 (last year of data in the terrorism set)
polity_data <- polity_data |>
  filter(year < 2018)

glimpse(polity_data)
glimpse(terrorism_data)

#joining the datasets by year and name of country 
terrorism_data <- terrorism_data |>
  mutate(country_name = country_txt)
polity_data <- polity_data |>
  mutate(country_name = country)

combined_data <- terrorism_data |>
  inner_join(polity_data, join_by(year, country_name), relationship =
               "many-to-many") 

vis_miss(combined_data, warn_large_data = FALSE)

#creating a package with my combined dataset with variable descriptions 
install.packages("roxygen2")
library(roxygen2)
install.packages("usethis")
usethis::create_package(data)
save(my_dataset, file = "data/combined_data.RData")

