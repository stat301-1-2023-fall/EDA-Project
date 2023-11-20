#loading packages
library(tidyverse)
library(naniar)
library(haven)

#importing data
terrorism_data <- read_csv("data/raw/globalterrorismdb_0718dist.csv")
polity_data <- read_sav("data/raw/p5v2018.sav")

glimpse(terrorism_data)
glimpse(polity_data)

summary(terrorism_data)
summary(polity_data)


