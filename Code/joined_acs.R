rm(list = ls())
setwd("C:/Users/Abdullah/Desktop/Grad_School/Research/Refugee_migration/Data")

library(ipumsr)
library(tidyverse)


###########################################################################################

# load acs immigrants 10 percent sample
ddi <- read_ipums_ddi("./raw/ACS/usa_00005.dat/usa_00005.xml")
data <- read_ipums_micro(ddi)

codes <- read_csv("./raw/ACS/country_codes/country_codes.csv", col_names = F) %>% select("codes" = "X1", "region" = "X2")
d_codes <- read_csv("./raw/ACS/country_codes/detailed_country_codes.csv", col_names = F) %>% select("d_codes" = "X1", "country" = "X2")
d_codes$d_codes <- as.integer(d_codes$d_codes)

data <- data %>% left_join(d_codes, by = c("BPLD" = "d_codes")) %>% left_join(codes, by = c("BPL" = "codes"))
saveRDS(data, file = "./cleaned/acs_immigrants_10percent.Rdata")

############################################################################################

# Now load acs immigrants full sample
ddi_f <- read_ipums_ddi("./raw/ACS/usa_00007.dat/usa_00007.xml")
data_f <- read_ipums_micro(ddi_f)

codes <- read_csv("./raw/ACS/country_codes/country_codes.csv", col_names = F) %>% select("codes" = "X1", "region" = "X2")
d_codes <- read_csv("./raw/ACS/country_codes/detailed_country_codes.csv", col_names = F) %>% select("d_codes" = "X1", "country" = "X2")
d_codes$d_codes <- as.integer(d_codes$d_codes)

data_f <- data_f %>% left_join(d_codes, by = c("BPLD" = "d_codes")) %>% left_join(codes, by = c("BPL" = "codes"))
saveRDS(data_f, file = "./cleaned/acs_immigrants_full.Rdata")

     