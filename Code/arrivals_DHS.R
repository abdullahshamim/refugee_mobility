rm(list = ls())
setwd("C:/Users/Abdullah/Desktop/Grad_school/Research")

library(tidyverse)
library(mgsub)

`%notin%` <- Negate(`%in%`)

# Format data
arrivals_0003 <- read.csv("./Data/DHS_refugee_arrivals/arrivals_1990-2003.csv", header = F, nrows = 108, skip = 5)
names(arrivals_0003) <- c("country", 1990:2003)
arrivals_0003 <- arrivals_0003 %>%  select("country", "2000":"2003")
arrivals_0003 <- 
  subset(arrivals_0003, country %notin% c("Europe", "Asia 1", "Africa", "North America", "Central America 6", "South America"))

arrivals_0413 <- read.csv("./Data/DHS_refugee_arrivals/arrivals_2004-2013.csv", header = F, nrows = 65, skip = 15)
names(arrivals_0413) <- c("country", 2004:2013)
arrivals_0413 <- arrivals_0413 %>% select("country", "2004":"2013")

arrivals_1416 <- read.csv("./Data/DHS_refugee_arrivals/arrivals_2014-2016.csv", header = F, skip = 1)
names(arrivals_1416) <- c("country", 2014:2016)

arrivals_1719 <- read.csv("./Data/DHS_refugee_arrivals/arrivals_2017-2019.csv", header = F, skip = 1)
names(arrivals_1719) <- c("country", 2017:2019)

# Install package 'mgsub' to delete empty spaces and superscripts
arrivals_0003$country <- mgsub(arrivals_0003$country, c(" ", "1", "2", "3", "4", "5"), c("", "", "", "", "", ""))
arrivals_0413$country <- mgsub(arrivals_0413$country, c(" ", "1", "2", "3", "4", "5"), c("", "", "", "", "", ""))
arrivals_1416$country <- mgsub(arrivals_1416$country, c(" ", "1", "2", "3", "4", "5"), c("", "", "", "", "", ""))
arrivals_1719$country <- mgsub(arrivals_1719$country, c(" ", "1", "2", "3", "4", "5"), c("", "", "", "", "", ""))

# Clean data; replace missing values with zero
arrivals <- arrivals_0003 %>% full_join(arrivals_0413) %>% full_join(arrivals_1416) %>% full_join(arrivals_1719)
arrivals[arrivals == "-" | arrivals == "D" | is.na(arrivals) | arrivals == "X"]  <- 0
arrivals[41,2] <- 0

# Remove countries with zero arrivals
for (i in 2:21) {
  arrivals[, i] <- as.numeric(gsub(",", "", arrivals[, i]))
}

arrivals$sum <- rowSums(as.data.frame(subset(arrivals, select = -country)))
arrivals <- filter(arrivals, sum > 0) %>% select(!sum)

# Merge Serbia-Montenegro and SerbiaandMontenegro
arrivals[87, 2:21] <- colSums(subset(arrivals, country == "Serbia-Montenegro" | country == "SerbiaandMontenegro", select = -country))
arrivals <- subset(arrivals, country != "Serbia-Montenegro")

# Convert to long data
arrivals <- arrivals %>% pivot_longer(!country, names_to = "year", values_to = "count")
arrivals$year <- as.numeric(arrivals$year)

# Save arrivals data
saveRDS(arrivals, file = "./Refugee_migration/Data/cleaned/DHS_arrivals_cleaned.Rdata")
