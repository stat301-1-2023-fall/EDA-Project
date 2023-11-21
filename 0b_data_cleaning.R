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

#dropping columns that consists of 50% or more missing values
terrorism_drop <- names(missing_percentages_terrorism[missing_percentages_terrorism > 50])
polity_drop <- names(missing_percentages_polity[missing_percentages_polity > 50])

terrorism_data <- terrorism_data |>
  select(-all_of(terrorism_drop))

polity_data <- polity_data |>
  select(-all_of(polity_drop))
  
#adding a new date column 
terrorism_data <- terroism_data |>
  mutate(date = as.Date(paste(iyear, imonth, iday, sep = "-"))) |>
  select(date, everything())

#dropping any entries that have a year prior to 1776 (earliest year in the polity dataset)
terrorism_data <- terroism_data |>
  filter(iyear > 1775)
#dropping any entries after 2017 (last year of data in the terrorism set)
polity_data <- polity_data |>
  filter(year < 2018)

#changing the column names to match
terroism_data <- terroism_data |>
  mutate(year = iyear,
         month = imonth,
         day = iday) |>
  select(date, year, month, day, everything())


#incorporating the region variable into the polity dataset
