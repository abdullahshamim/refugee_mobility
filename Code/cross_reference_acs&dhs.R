rm(list = ls())

setwd("C:/Users/Abdullah/Desktop/Grad_School/Research/Refugee_migration/Data/cleaned")

library(tidyverse)
library(stargazer)

###########################################################
# 
# # Cross-reference country-year pairs (one year)
# 
# imm <- readRDS("acs_immigrants_10percent.Rdata") %>% filter(YRIMMIG == 2015)
# rfg_arrvls <- readRDS("DHS_arrivals_cleaned.Rdata") %>% select("country", "2015")
# 
# imm_sum <- imm %>% group_by(country) %>% summarize(wgtdsum = sum(PERWT))


###########################################################

# # cross reference country-year pairs (10 percent sample; all years)
# 
# imm <- readRDS("acs_immigrants_10percent.Rdata") 
# rfg_arrvls <- readRDS("DHS_arrivals_cleaned.Rdata") %>% select(!sum)
# 
# imm_sum <- imm %>% filter(YRIMMIG > 1999) %>% 
#   group_by(country, YRIMMIG) %>% summarize(wgtdsum = sum(PERWT))
# 
# rfg_arrvls <- rfg_arrvls %>% pivot_longer(!country, names_to = "year", values_to = "count")
# rfg_arrvls$year <- as.numeric(rfg_arrvls$year)
# 
# immgref <- left_join(rfg_arrvls, imm_sum, by = c("country" = "country", "year" = "YRIMMIG")) %>%
#   mutate(ratio = count/wgtdsum) %>%
#   filter(ratio > .7)

############################################################

# cross reference country-year pairs (full sample; all years)

# Load data (ACS and DHS Yearbook of Immigration Statistics)
imm_f <- readRDS("acs_immigrants_full.Rdata") 
rfg_arrvls <- readRDS("DHS_arrivals_cleaned.Rdata")

# Rename countries for conformity b/w datasets
imm_f$country[imm_f$country == "Byelorussia"] <- "Belarus"
imm_f$country[imm_f$country == "Bosnia"] <- "Bosnia-Herzegovina"
imm_f$country[imm_f$country == "Republic of Georgia"] <- "Georgia"
imm_f$country[imm_f$country == "Moldovia"] <- "Moldova"
imm_f$country[imm_f$country == 'Other USSR/"Russia"'] <- "Russia"
imm_f$country[imm_f$country == "Burma (Myanmar)"] <- "Burma"
imm_f$country[imm_f$country == "Cambodia (Kampuchea)"] <- "Cambodia"
imm_f$country[imm_f$country == "China"] <- "China,People'sRepublic"
imm_f$country[imm_f$country == "Congo"] <- "Congo,DemocraticRepublic"
imm_f$country[imm_f$country == "Ivory Coast"] <- "Coted'Ivoire"
imm_f$country[imm_f$country == "Egypt/United Arab Rep."] <- "Egypt"
imm_f$country[imm_f$country == "Gambia"] <- "Gambia,The"
imm_f$country[imm_f$country == "Sierra Leone"] <- "SierraLeone"
imm_f$country[imm_f$country == "Serbia"] <- "SerbiaandMontenegro"
imm_f$country[imm_f$country == "South Sudan"] <- "SouthSudan"
imm_f$country[imm_f$country == "El Salvador"] <- "ElSalvador"

# total immigrants from each country
imm_f1 <- filter(imm_f, YRIMMIG > 1999)

imm_f_sum <- imm_f1 %>% 
  group_by(country, YRIMMIG, YEAR) %>% 
  summarize(totimmg = sum(PERWT)) %>%
  filter(YRIMMIG < YEAR) %>% 
  summarize(yravg = mean(totimmg))

# What proportion immigrants are refugees?
immgref <- left_join(rfg_arrvls, imm_f_sum, by = c("country" = "country", "year" = "YRIMMIG")) %>%
  mutate(ratio = count/yravg)

ggplot(data = immgref, aes(x = yravg, y = count)) +
  geom_point() +
  geom_abline(intercept=0, slope=0.7) +
  geom_abline(intercept=0, slope=0.3) +
  scale_x_continuous(breaks = seq(0, 50000, 10000), 
                     limits = c(0, 50000)) +
  scale_y_continuous(limits = c(0, 50000))

# Define refugee cutoff         
refgs <- immgref %>% filter(ratio > 0.7)
immgs <- immgref %>% filter(ratio < 0.3)


# Migrated after 1999
imm_f1 <- filter(imm_f, YRIMMIG > 1999)

# Refugees dummy
imm_f1$refgstts <- match(paste(imm_f1$country, imm_f1$YRIMMIG),
                         paste(refgs$country, refgs$year),
                         nomatch = NA)
imm_f1$refgstts[!is.na(imm_f1$refgstts)] <- 1
imm_f1$refgstts[is.na(imm_f1$refgstts)] <- 0

# Immigrant dummy
imm_f1$immgstts <- match(paste(imm_f1$country, imm_f1$YRIMMIG),
                         paste(immgs$country, immgs$year),
                         nomatch = NA)
imm_f1$immgstts[!is.na(imm_f1$immgstts)] <- -1
imm_f1$immgstts[is.na(imm_f1$immgstts)] <- 0

# Refugee-immigrant joint categorical variable
imm_f1$immgstts <- imm_f1$refgstts + imm_f1$immgstts
#imm_f1 <- select(imm_f1, !immgstts)

# Create within-state and between-state migration dummies
imm_f1$mvrswnst <- ifelse(imm_f1$MIGRATE1 == 2, 1, 0)
imm_f1$mvrsbnst <- ifelse(imm_f1$MIGRATE1 == 3, 1, 0)


# Run the first refugee status on migration regressions
mvwnreg <- lm(mvrswnst ~ factor(refgstts), data = imm_f1)
mvbnreg <- lm(mvrsbnst ~ factor(refgstts), data = imm_f1)

stargazer(mvwnreg, mvbnreg, type = 'text')

# Subset the data frame to include only refugee producing countries
imm_f2 <- filter(imm_f1, !is.na(match(imm_f1$country, refgs$country, nomatch = NA)))

# Mobility rates of refugees vs immigrants by country
wnstsumm <- imm_f2 %>% # Within-state
  group_by(country, refgstts) %>% 
  summarize(mbltywnst = mean(mvrswnst)) %>% 
  arrange(refgstts, mbltywnst)

bnstsumm <- imm_f2 %>% # Between-state
  group_by(country, refgstts) %>% 
  summarize(mbltybnst = mean(mvrsbnst)) %>% 
  arrange(refgstts, mbltybnst)

# Graph refgstts on mobility rates;
ggplot(data = wnstsumm, aes(x = factor(country, level = wnstsumm$country[1:(length(bnstsumm$country)/2)]),
                            y = mbltywnst, color = as.factor(refgstts))) +
  geom_point() +
  scale_color_discrete(labels = c("Immigrants", "Refugees")) +
  guides(x = guide_axis(angle = 90)) +
  ggtitle("Within-state Mobility Rates") +
  labs(title = "Within-state Mobility Rates\n", x = "Mobility", y = "Country of Birth", color = "\n")

ggplot(data = bnstsumm, aes(x = factor(country, level = bnstsumm$country[1:(length(bnstsumm$country)/2)]),
                            y = mbltybnst, color = as.factor(refgstts))) +
  geom_point() +
  scale_color_discrete(labels = c("Immigrants", "Refugees")) +
  guides(x = guide_axis(angle = 90)) +
  labs(title = "Between-state Mobility Rates\n", x = "Mobility", y = "Country of Birth", color = "\n")

# Country of birth fixed effects
mvwnreg_cntry <- lm(mvrswnst ~ factor(refgstts) + factor(SEX) + factor(country), data = imm_f2)
mvbnreg_cntry <- lm(mvrsbnst ~ factor(refgstts) + factor(SEX) + factor(country), data = imm_f2)

mvwnreg1 <- lm(mvrswnst ~ factor(refgstts) + EDUC + INCTOT + factor(SEX) + factor(country), data = imm_f2)
mvbnreg1 <- lm(mvrsbnst ~ factor(refgstts) + EDUC + INCTOT + factor(SEX) + factor(country), data = imm_f2)

stargazer(mvwnreg_cntry, mvbnreg_cntry, mvwnreg1, mvbnreg1, type = 'text', omit = "country")
#stargazer(mvwnreg1, mvbnreg1, type = 'text')


# probit
mvwnprob <- glm(mvrswnst ~ factor(refgstts) + EDUC + INCTOT + factor(country),
                data = imm_f2, family = binomial(link = "probit"))
mvbnprob <- glm(mvrsbnst ~ factor(refgstts) + EDUC + INCTOT + factor(country),
                data = imm_f2, family = binomial(link = "probit"))

stargazer(mvwnprob, mvbnprob, type = 'text', omit = "country")
#stargazer(mvwnprob, mvbnprob, type = 'text')

# Interaction term
mvwnreg2 <- lm(mvrswnst ~ factor(refgstts) + EDUC + factor(refgstts) * EDUC + INCTOT + factor(country), data = imm_f2)
mvbnreg2 <- lm(mvrsbnst ~ factor(refgstts) + EDUC + factor(refgstts) * EDUC + INCTOT + factor(country), data = imm_f2)

stargazer(mvwnreg2, mvbnreg2, type = 'text', omit = "country")

# Interaction with immgstts
mvwnreg3 <- lm(mvrswnst ~ factor(immgstts) + EDUC + factor(immgstts) * EDUC + INCTOT + factor(country), data = imm_f2)
mvbnreg3 <- lm(mvrsbnst ~ factor(immgstts) + EDUC + factor(immgstts) * EDUC + INCTOT + factor(country), data = imm_f2)

stargazer(mvwnreg3, mvbnreg3, type = 'text', omit = "country")

# probit
mvwnprob2 <- glm(mvrswnst ~ factor(refgstts) + EDUC + factor(refgstts) * EDUC + INCTOT + factor(country),
                   data = imm_f2, family = binomial(link = "probit"))
mvbnprob2 <- glm(mvrsbnst ~ factor(refgstts) + EDUC + factor(refgstts) * EDUC + INCTOT + factor(country),
                   data = imm_f2, family = binomial(link = "probit"))

stargazer(mvwnprob2, mvbnprob2, type = 'text', omit = "country")

####################################################################################

# Reg tables, baseline, w/ control, w/contrl + interaction

stargazer(mvwnreg, mvwnreg1, mvwnreg2, type = 'text', omit = "country")
stargazer(mvbnreg, mvbnreg1, mvbnreg2, type = 'text', omit = "country")

stargazer(mvwnprob, mvwnprob2, mvwnreg2, type = 'text', omit = "country")
stargazer(mvbnprob, mvbnprob2, mvwnreg2, type = 'text', omit = "country")

# Next steps:
# Discretize educ term: low educ, medium, and high

imm_f2$EDUCLVLS <- NA
imm_f2$EDUCLVLS <- as.character(imm_f2$EDUCLVLS)
imm_f2[imm_f2$EDUC >=0 & imm_f2$EDUC <=4, "EDUCLVLS"] <- "Low"
imm_f2[imm_f2$EDUC >=5 & imm_f2$EDUC <=8, "EDUCLVLS"] <- "Medium"
imm_f2[imm_f2$EDUC >=9, "EDUCLVLS"] <- "High"

mvbnprob3 <- glm(mvrsbnst ~ factor(refgstts) + factor(EDUCLVLS) + factor(refgstts) * factor(EDUCLVLS)
                 + INCTOT + factor(country),
                 data = imm_f2, family = binomial(link = "probit"))

mvbnreg4 <- lm(mvrsbnst ~ factor(refgstts) + factor(EDUCLVLS) + factor(refgstts) * factor(EDUCLVLS)
               + INCTOT + factor(country), data = imm_f2)

stargazer(mvbnprob3, mvbnreg4, omit = "country", type = 'text')

# Limit the data to only new refgs
# Create years since immigration variable

imm_f2$YRSAFTRMIG <- imm_f2$YEAR - imm_f2$YRIMMIG
imm_f2$YRSAFTRMIG[imm_f2$YRSAFTRMIG<=5] <- 1
imm_f2$YRSAFTRMIG[imm_f2$YRSAFTRMIG>5 & imm_f2$YRSAFTRMIG<=10] <- 2
imm_f2$YRSAFTRMIG[imm_f2$YRSAFTRMIG>10 & imm_f2$YRSAFTRMIG<=15] <- 3
imm_f2$YRSAFTRMIG[imm_f2$YRSAFTRMIG>15 & imm_f2$YRSAFTRMIG<=20] <- 4
imm_f2$YRSAFTRMIG[imm_f2$YRSAFTRMIG>20 & imm_f2$YRSAFTRMIG<=25] <- 5
imm_f2$YRSAFTRMIG[imm_f2$YRSAFTRMIG>25 & imm_f2$YRSAFTRMIG<=30] <- 6
imm_f2$YRSAFTRMIG[imm_f2$YRSAFTRMIG>30] <- 7

mvbnreg5 <- lm(mvrsbnst ~ factor(refgstts) + factor(YRSAFTRMIG) + factor(refgstts) * factor(YRSAFTRMIG)
               + EDUC + INCTOT + factor(country), data = imm_f2)

mvbnprob4 <- lm(mvrsbnst ~ factor(refgstts) + factor(YRSAFTRMIG) + factor(refgstts) * factor(YRSAFTRMIG)
               + EDUC + INCTOT + factor(country), data = imm_f2, family = binomial(link = "probit"))

stargazer(mvbnreg5, mvbnprob4, omit = "country", type = 'text')

# Next steps: 
# compare the coefficients in YRSAFTERMIG = 1 for refgs and immigs
# get familiar with plotting reg results