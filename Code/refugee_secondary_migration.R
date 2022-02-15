# Load packages
library(foreign)
library(tidyverse)
library(haven)
library(labelled)
library(stargazer)
library(plyr)

# set directory
setwd("C:/Users/Abdullah/Desktop/Grad_School/Research//Refugee_migration/Data/Raw/ASR_Refugee")

# Load data
ref18 <- read_dta("./2018-ASR_Public_Use_File.dta")
ref17 <- read_dta("./2017-ASR_Public_Use_File.dta")
ref16 <- read_dta("./2016-ASR_Public_Use_File.dta")

# Add a time-script
ref16$t <- 2016
ref17$t <- 2017
ref18$t <- 2018

names(ref16)[names(ref16)=="qn1jyear"] <- "arrivaly16"
names(ref17)[names(ref17)=="qn1jyear"] <- "arrivaly17"
names(ref18)[names(ref18)=="qn1jyear"] <- "arrivaly18"
refcombined <- bind_rows(ref16, ref17, ref18) #has info for year of arrival over three separate vars

refcombined$arrivaly16[is.na(refcombined$arrivaly16)] <- 0
refcombined$arrivaly17[is.na(refcombined$arrivaly17)] <- 0
refcombined$arrivaly18[is.na(refcombined$arrivaly18)] <- 0

refcombined$arrivaly <- refcombined$arrivaly16 + refcombined$arrivaly17 + refcombined$arrivaly18
refcombined$arrivaly[refcombined$arrivaly == 0] <- NA

# Keep only heads
heads <- subset(refcombined, qn1a == 1)

# select variables to keep
heads <- select(heads, hhid, numppl:qn1g, qn2a, qn4a, qn18a:qn18c, qn26b, qn26f, arrivaly, t)

# Rename and relabel variables
names(heads) <- c("hhid", "numppl", "seqn", "married", "age", "gender", "birthc", "educ", "englisha", "wkspy",
                   "hourspw", "anninc", "durneigh", "rmigrat", "arrivaly", "intervy")

var_label(heads) <- list("Household id", "Number of people", "Relationship to head", "Marriage status", 
                         "Age", "Gender", "Country of birth", "Years of schooling at arrival", 
                         "English proficiency at arrival", "Weeks worked in last year", 
                         "Hours per week last year", "Income last year", "Months in present location", 
                         "Reason for secondary migration", "Arrival year", "interview year")

# Relabel englisha numbers


# Create binary migrant for work variable
heads$wmigrant <- as.numeric(heads$rmigrat==1)
var_label(heads$wmigrant) <- "Dummy indicating migration for employment opportunities"

# Create binary migrant for family and friends reunification variable
heads$fmigrant <- as.numeric(heads$rmigrat==3 | heads$rmigrat==14)
var_label(heads$fmigrant) <- "Dummy indicating migration for family and friends reunification"

# Create years in the US variable
heads$usres <- heads$arrivaly - heads$intervy
var_label(heads$usres) <- "Years in US"

# Decode country of birth variable
heads$birthc <- as.numeric(heads$birthc)
heads$birthc[heads$birthc==1] <- "Afghanistan"
heads$birthc[heads$birthc==2] <- "Bhutan"
heads$birthc[heads$birthc==3] <- "Burma"
heads$birthc[heads$birthc==4] <- "Burundi"
heads$birthc[heads$birthc==5] <- "Cuba"
heads$birthc[heads$birthc==6] <- "Democratic Republic of the Congo"
heads$birthc[heads$birthc==7] <- "Eritrea"
heads$birthc[heads$birthc==8] <- "Ethiopia"
heads$birthc[heads$birthc==9] <- "Iran"
heads$birthc[heads$birthc==10] <- "Iraq"
heads$birthc[heads$birthc==11] <- "Jordan"
heads$birthc[heads$birthc==12] <- "Kenya"
heads$birthc[heads$birthc==13] <- "Malaysia"
heads$birthc[heads$birthc==14] <- "Nepal"
heads$birthc[heads$birthc==15] <- "Rwanda"
heads$birthc[heads$birthc==16] <- "Somalia"
heads$birthc[heads$birthc==17] <- "Sudan"
heads$birthc[heads$birthc==18] <- "Syria"
heads$birthc[heads$birthc==19] <- "Tanzania"
heads$birthc[heads$birthc==20] <- "Thailand"
heads$birthc[heads$birthc==21] <- "Uganda"
heads$birthc[heads$birthc==22] <- "Ukraine"
heads$birthc[heads$birthc==24] <- "United States"
heads$birthc[heads$birthc==25] <- "Colombia"
heads$birthc[heads$birthc==26] <- "El Salvador"
heads$birthc[heads$birthc==27] <- "Other"
heads$birthc[heads$birthc==28] <- "Don't know"
heads$birthc[heads$birthc==29] <- "Refused"

##########################################################################################################


# 
# # K-S test education of migrants vs full sample
# ks.test(heads$educ[heads$rmigrat == 1 & heads$educ<98], heads$educ[heads$educ<98])
# ks.test(heads$educ[heads$rmigrat == 1 & heads$educ<98], heads$educ[heads$educ<98], alternative = "greater")
# # significant at the 5% level when alternative hypothesis is "greater than" 
# # => rejects null that secondary immigrants have lower eduation => negative selection is not going on
# 
# # K-S test for country of origin (don't want this to be significant)
# hist(heads$birthc[heads$rmigrat==1])
# hist(heads$birthc)
# ks.test(heads$birthc[heads$rmigrat == 1 & heads$birthc<98], heads$birthc[heads$birthc<98])
# 
# # English ability of migrants vs entire refugee pop.
# ks.test(heads$englisha[heads$rmigrat == 1 & heads$englisha<8], heads$englisha[heads$englisha<8])
# 
# # Gender - migrants vs. general refugees
# ks.test(heads$gender[heads$rmigrat == 1 & heads$gender<8], heads$gender[heads$gender<8])



##########################################################################################################



# Regress migrate for work dummy on explanatory vars in Potocky-Tripodi (2004)
wmig_reg <- lm(wmigrant ~ educ + englisha + factor(birthc) + factor(gender) + usres, data = heads)
wmig_probit <- glm(wmigrant ~ educ + englisha + factor(birthc) + factor(gender) + usres, 
                   data = heads, family = binomial(link = "probit"))

fmig_reg <- lm(fmigrant ~ educ + englisha + factor(birthc) + factor(gender) + usres, data = heads)
fmig_probit <- glm(fmigrant ~ educ + englisha + factor(birthc) + factor(gender) + usres, 
                   data = heads, family = binomial(link = "probit"))

stargazer(wmig_reg, wmig_probit, fmig_reg, fmig_probit, type = 'text',
          title = "Work and family related migration", omit = "birthc") # linear probability specification

# With squared education term
wmig_probit2 <- glm(wmigrant ~ educ + I(educ^2) + englisha + factor(birthc) + factor(gender) + usres,
                    data = heads, family = binomial(link = "probit"))
stargazer(wmig_probit2, type = 'text', omit = "birthc") #squared educ


##########################################################################################################


# earnings conditional on migration for work
table(heads$wmigrant, heads$anninc>=9999998, useNA = "ifany") #355 + 1819 observations with valid annual income (don't know income counted as invalid)
m_earnings <- lm(anninc ~ wmigrant + fmigrant, data = subset(heads, subset = anninc < 9999998))
stargazer(m_earnings, type='text')

# capture both english ability and returns from migration
m_earnings1 <- lm(anninc ~ wmigrant + fmigrant + englisha, data = subset(heads, subset = anninc < 9999998))
stargazer(m_earnings1, type='text')

# time and age FE
m_earnings2 <- lm(anninc ~ wmigrant + fmigrant + educ + I(educ^2) + englisha + usres + durneigh + 
                  factor(birthc) + factor(gender) + age + intervy, 
                  data = subset(heads, subset = anninc < 9999998))

stargazer(m_earnings, m_earnings1, m_earnings2, type = 'text', title = "Income on migration and controls", omit = "birthc")

