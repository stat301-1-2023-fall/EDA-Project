#loading packages
library(tidyverse)
library(naniar)
library(haven)
install.packages("readr")
library(readr)

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

#joining the datasets by year and name of country 
terrorism_data <- terrorism_data |>
  mutate(country_name = country_txt)
polity_data <- polity_data |>
  mutate(country_name = country)

combined_data <- terrorism_data |>
  inner_join(polity_data, join_by(year, country_name), relationship =
               "many-to-many") 

#removing repeated and unneeded variables
combined_data <- combined_data |>
  select(-country_txt, - country.y,
         -eventid, -cyear, -country.x, -ccode, -scode, , -latitude, -longitude, -vicinity,
         -dbsource,  -scite1, -specificity, -weapsubtype1_txt,-weapdetail, -summary, 
         -nkillter, -nkillus, -nwoundus, -nwoundte, -claimed, -corp1,
         -nperps,-nperpcap, -INT_MISC, -INT_ANY, -INT_IDEO, -INT_LOG, -p5, -flag, -polity2)

#creating a dataset that is text based
combined_data <- combined_data |>
  select(-date, -region, -attacktype1, -targtype1, -targsubtype1, -natlty1, -weaptype1, -weapsubtype1)

#changing all unknown values to NA
combined_data$ishostkid[combined_data$ishostkid == -9] <- NA
combined_data$day[combined_data$day == 0] <- NA
combined_data$month[combined_data$month == 0] <- NA
combined_data$doubtterr[combined_data$doubtterr == -9] <- NA
combined_data$property[combined_data$property == -9] <- NA


#looking at and cleaning missingness
vis_miss(combined_data, warn_large_data = FALSE)

missing_summary <- miss_var_summary(combined_data)
missing_summary

#checking complexity
num_cat <- sum(sapply(combined_data, is.factor) | sapply(combined_data, is.character))
num_num <- sum(sapply(combined_data, is.numeric))
print(num_cat)
print(num_num)

combined_data

#writing dataet into the data file 
write_csv(combined_data, "data/eda_data.csv")


