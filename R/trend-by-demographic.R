## TITLE: Trend of patients with any mental health diagnosis among substance-related 
##        visits to US pediatric hospitals, 2016-2021
## AUTHOR: Alejandro D. Hernandez
## DATE LAST UPDATED: 2/8/2025

# Clear environment
rm(list = ls())

# Load relevant packages
library(tidyverse)  # whole bunch of useful packages
library(knitr)      # pretty print
library(lubridate)  # date formatting
library(ggplot2)    # data visualization
theme_set(theme_minimal())

source("R/summarize-growth.R")
source("R/model-growth.R")
source("R/preprocessing.R")


# Load data
phis_raw <- read_csv(file = "data/mh_subs_uw.csv")  # Ensure this is the right directory
nrow(phis_raw)  # 106,793 rows
names(phis_raw)

# Preprocess data
phis <- preprocessing(phis_raw)
nrow(phis)

phis_mh <- subset(phis, MH_ANY == 1)

## How large is each patient demographics?
# Race/Ethnicity
phis_mh %>% select(RACE_ETHNICITY) %>% 
  
  mutate(RACE_ETHNICITY = case_when(
    RACE_ETHNICITY == "Non-Hispanic White" ~ "NH White",
    RACE_ETHNICITY == "Black or African American" ~ "NH Black",
    RACE_ETHNICITY == "Hispanic" ~ "Hispanic",
    TRUE ~ "Other"
  )) %>%
  
  table %>% 
  t %>% data.frame %>% .[-1] %>%
  arrange(desc(Freq)) %>% mutate(Perc = 100*round(Freq / nrow(phis_mh), 4)) %>%
  ggplot(aes(y=Perc, x=RACE_ETHNICITY)) + 
    geom_col() + ylim(c(0,60)) +
    geom_text(aes(label=Perc), col="white", size=6, vjust=1.5) + 
    labs(x="", y="")

# Hospital assigned sex
phis_mh %>% select(HOSP_RECORDED_SEX) %>% table %>% 
  t %>% data.frame %>% .[-1] %>%
  arrange(desc(Freq)) %>% mutate(Perc = 100*round(Freq / nrow(phis_mh), 4)) %>%
  filter(HOSP_RECORDED_SEX != "Unknown") %>%
  ggplot(aes(y=Perc, x=HOSP_RECORDED_SEX)) + 
    geom_col() + ylim(c(0,60)) +
    geom_text(aes(label=Perc), col="white", size=6, vjust=1.5) + 
    labs(x="", y="")
# Region
phis_mh %>% select(REGION) %>% table %>% 
  t %>% data.frame %>% .[-1] %>%
  arrange(desc(Freq)) %>% mutate(Perc = round(Freq / nrow(phis_mh), 4))
# Age group
phis_mh %>% select(AGE_GRP) %>% table %>% 
  t %>% data.frame %>% .[-1] %>%
  arrange(desc(Freq)) %>% mutate(Perc = 100*round(Freq / nrow(phis_mh), 4)) %>% 
  ggplot(aes(y=Perc, x=AGE_GRP)) + 
  geom_col() + ylim(c(0,60)) +
  geom_text(aes(label=Perc), col="white", size=6, vjust=1.5) + 
  labs(x="", y="")


# Insurance types
phis_mh %>% select(INSURANCE_GROUP) %>% table %>% 
  t %>% data.frame %>% .[-1] %>%
  arrange(desc(Freq)) %>% mutate(Perc = round(Freq / nrow(phis_mh), 4))

# Emergency department encounter
phis_mh %>% select(ED_FLAG) %>% table %>% 
  t %>% data.frame %>% .[-1] %>%
  arrange(desc(Freq)) %>% mutate(Perc = round(Freq / nrow(phis_mh), 4))
# Inpatient encounter
phis_mh %>% select(INPATIENT_FLAG) %>% table %>% 
  t %>% data.frame %>% .[-1] %>%
  arrange(desc(Freq)) %>% mutate(Perc = round(Freq / nrow(phis_mh), 4))
# Intensive-care-unit encounter
phis_mh %>% select(ICU_FLAG) %>% table %>% 
  t %>% data.frame %>% .[-1] %>%
  arrange(desc(Freq)) %>% mutate(Perc = round(Freq / nrow(phis_mh), 4))

# Emergency department discharge
phis_mh %>% filter(ED_FLAG==1 & INPATIENT_FLAG==0 & ICU_FLAG==0) %>% 
  summarize(Freq=n()) %>% mutate(Perc = round(Freq / nrow(phis_mh), 4))


########################
#### RACE/ETHNICITY ####
########################

### Non-Hispanic White ###

# Subset Non-Hispanic White patients
phis_nhw <- subset(phis_mh, RACE_ETHNICITY == "Non-Hispanic White")
nrow(phis_nhw)  # 53,405 rows

## Visualize and summarize trends over time
res_nhw <- summarize_growth(phis_nhw)

# Plot of hospital counts
res_nhw$plots$hosp_counts_by_year

# Plots of percent growth
res_nhw$plots$growth_by_year + labs(title = "Non-Hispanic White")
res_nhw$plots$growth_by_qtr + labs(title = "Non-Hispanic White")

# Table of percent growth
res_nhw$tables$counts_by_year[1:3]

### Black or African American ###

# Subset Black or African American patients
phis_baf <- subset(phis_mh, RACE_ETHNICITY == "Black or African American")
nrow(phis_baf)  # 21,896 rows

## Visualize and summarize trends over time
res_baf <- summarize_growth(phis_baf)

# Plot of hospital counts
res_baf$plots$hosp_counts_by_year

# Plots of percent growth
res_baf$plots$growth_by_year + labs(title = "Black or African American")
res_baf$plots$growth_by_qtr + labs(title = "Black or African American")

# Table of percent growth
res_baf$tables$counts_by_year[1:3]

### Hispanic ###

# Subset Hispanic patients
phis_his <- subset(phis_mh, RACE_ETHNICITY == "Hispanic")
nrow(phis_his)  # 21,053 rows

## Visualize and summarize trends over time
res_his <- summarize_growth(phis_his)

# Plot of hospital counts
res_his$plots$hosp_counts_by_year

# Plots of counts
res_his$plots$counts_by_year + labs(title = "Hispanic")
res_his$plots$counts_by_qtr + labs(title = "Hispanic")

# Plots of percent growth
res_his$plots$growth_by_year + labs(title = "Hispanic")
res_his$plots$growth_by_qtr + labs(title = "Hispanic")

# Table of percent growth
res_his$tables$counts_by_year[1:3]

### Other ###

# Subset patients
phis_oth <- subset(phis_mh, RACE_ETHNICITY == "Other")
nrow(phis_oth)  # 3,847 rows

## Visualize and summarize trends over time
res_oth <- summarize_growth(phis_oth)

# Table of percent growth
res_oth$tables$counts_by_year[1:3]

### Unknown ###

# Subset patients
phis_unk <- subset(phis_mh, RACE_ETHNICITY == "Unknown")
nrow(phis_unk)  # 3,847 rows

## Visualize and summarize trends over time
res_unk <- summarize_growth(phis_unk)

# Table of percent growth
res_unk$tables$counts_by_year[1:3]


### Asian ###

# Subset patients
phis_asi <- subset(phis_mh, RACE_ETHNICITY == "Asian")
nrow(phis_asi)  # 1,343 rows

## Visualize and summarize trends over time
res_asi <- summarize_growth(phis_asi)

# Table of percent growth
res_asi$tables$counts_by_year$percentgrowth


### 2 or more races ###

# Subset patients
phis_2mo <- subset(phis_mh, RACE_ETHNICITY == "2 or more races")
nrow(phis_2mo)  # 1,232 rows

## Visualize and summarize trends over time
res_2mo <- summarize_growth(phis_2mo)

# Table of percent growth
res_2mo$tables$counts_by_year[1:3]


### American Indian or Alaska Native ###

# Subset patients
phis_aia <- subset(phis_mh, RACE_ETHNICITY == "American Indian or Alaska Native")
nrow(phis_aia)  # 413 rows

## Visualize and summarize trends over time
res_aia <- summarize_growth(phis_aia)

# Table of percent growth
res_aia$tables$counts_by_year[1:3]


### Native Hawaiian or Other Pacific Islander ###

# Subset patients
phis_nho <- subset(phis_mh, RACE_ETHNICITY == "Native Hawaiian or Other Pacific Islander")
nrow(phis_nho)  # 241 rows

## Visualize and summarize trends over time
res_nho <- summarize_growth(phis_nho)

# Table of percent growth
res_nho$tables$counts_by_year[1:3]


#### COMBINATION PLOT ####
res_otherrace <- subset(phis_mh, 
                        !RACE_ETHNICITY %in% c("Non-Hispanic White", 
                                               "Black or African American", 
                                               "Hispanic")) %>%
  summarize_growth()

  
res_race <- data.frame(
  group = c("Hispanic", "NH Black", "NH White", "Other"),
  cumulative = c(
    res_his$tables$counts_by_year$percentgrowth[6],
    res_baf$tables$counts_by_year$percentgrowth[6],
    res_nhw$tables$counts_by_year$percentgrowth[6],
    res_otherrace$tables$counts_by_year$percentgrowth[6]))
res_race$cumulative <- 100*round(res_race$cumulative, 4)
# Plot
ggplot(res_race, aes(x=group, y=cumulative)) + 
  geom_col() + ylim(c(0,70)) +
  geom_text(aes(label=cumulative), col="white", size=6, vjust=1.5) + 
  labs(x="", y="")


#############
#### SEX ####
#############

### Female ###

# Subset female patients
phis_fem <- subset(phis_mh, HOSP_RECORDED_SEX == "Female")
nrow(phis_fem)  # 36,152 rows

## Visualize and summarize trends over time
res_fem <- summarize_growth(phis_fem)

# Plot of hospital counts
res_fem$plots$hosp_counts_by_year

# Plots of percent growth
res_fem$plots$growth_by_year + labs(title = "Female")
res_fem$plots$growth_by_qtr + labs(title = "Female")

# Table of percent growth
res_fem$tables$counts_by_year[1:3]

### Male ###

# Subset male patients
phis_mal <- subset(phis_mh, HOSP_RECORDED_SEX == "Male")
nrow(phis_mal)  # 28,447 rows

## Visualize and summarize trends over time
res_mal <- summarize_growth(phis_mal)

# Plot of hospital counts
res_mal$plots$hosp_counts_by_year

# Plots of percent growth
res_mal$plots$growth_by_year + labs(title = "Male")
res_mal$plots$growth_by_qtr + labs(title = "Male")

# Table of percent growth
res_mal$tables$counts_by_year[1:3]

### Unknown ###

# Subset unknown sex patients
phis_unksex <- subset(phis_mh, HOSP_RECORDED_SEX == "Unknown")
nrow(phis_unksex)  # 18 rows

## Visualize and summarize trends over time
res_unksex <- summarize_growth(phis_unksex)

# Plot of hospital counts
res_unksex$plots$hosp_counts_by_year

# Plots of percent growth
res_unksex$plots$growth_by_year + labs(title = "Unknown")
res_unksex$plots$growth_by_qtr + labs(title = "Unknown")

# Table of percent growth
res_unksex$tables$counts_by_year[1:3]

### COMBINATION TABLE ###
res_fem$tables$counts_by_year[c(1,6),1:3]
res_mal$tables$counts_by_year[c(1,6),1:3]
res_unksex$tables$counts_by_year[c(1,6),1:3]

### COMBINATION PLOT ###
res_sex <- data.frame(
  group = c("Female", "Male"),
  cumulative = c(
    res_fem$tables$counts_by_year$percentgrowth[6],
    res_mal$tables$counts_by_year$percentgrowth[6]))
res_sex$cumulative <- 100*round(res_sex$cumulative, 4)
# Plot
ggplot(res_sex, aes(x=group, y=cumulative)) + 
  geom_col() + ylim(c(0,70)) +
  geom_text(aes(label=cumulative), col="white", size=6, vjust=1.5) + 
  labs(x="", y="")

################
#### REGION ####
################

### Midwest ###

# Subset patients
phis_mid <- subset(phis_mh, REGION == "Midwest")
nrow(phis_mid)  # 23,603 rows

## Visualize and summarize trends over time
res_mid <- summarize_growth(phis_mid)

# Plot of hospital counts
res_mid$plots$hosp_counts_by_year

# Plots of percent growth
res_mid$plots$growth_by_year + labs(title = "Midwest")
res_mid$plots$growth_by_qtr + labs(title = "Midwest")

# Table of percent growth
res_mid$tables$counts_by_year[1:3]

### South ###

# Subset patients
phis_sou <- subset(phis_mh, REGION == "South")
nrow(phis_sou)  # 21,702 rows

## Visualize and summarize trends over time
res_sou <- summarize_growth(phis_sou)

# Plot of hospital counts
res_sou$plots$hosp_counts_by_year

# Plots of percent growth
res_sou$plots$growth_by_year + labs(title = "South")
res_sou$plots$growth_by_qtr + labs(title = "South")

# Table of percent growth
res_sou$tables$counts_by_year[1:3]

### West ###

# Subset patients
phis_wes <- subset(phis_mh, REGION == "West")
nrow(phis_wes)  # 12,892 rows

## Visualize and summarize trends over time
res_wes <- summarize_growth(phis_wes)

# Plot of hospital counts
res_wes$plots$hosp_counts_by_year

# Plots of percent growth
res_wes$plots$growth_by_year + labs(title = "West")
res_wes$plots$growth_by_qtr + labs(title = "West")

# Table of percent growth
res_wes$tables$counts_by_year[1:3]

### Northwest ###

# Subset patients
phis_nor <- subset(phis_mh, REGION == "Northwest")
nrow(phis_nor)  # 6,420 rows

## Visualize and summarize trends over time
res_nor <- summarize_growth(phis_nor)

# Plot of hospital counts
res_nor$plots$hosp_counts_by_year

# Plots of percent growth
res_nor$plots$growth_by_year + labs(title = "Northwest")
res_nor$plots$growth_by_qtr + labs(title = "Northwest")

# Table of percent growth
res_nor$tables$counts_by_year[1:3]

###################
#### AGE GROUP ####
###################

### 12-15 ###

# Subset patients
phis_age1 <- subset(phis_mh, AGE_GRP == "12-15")
nrow(phis_age1)  # 13,082 rows

## Visualize and summarize trends over time
res_age1 <- summarize_growth(phis_age1)

# Plot of hospital counts
res_age1$plots$hosp_counts_by_year

# Plots of percent growth
res_age1$plots$growth_by_year + labs(title = "12-15")
res_age1$plots$growth_by_qtr + labs(title = "12-15")

# Table of percent growth
res_age1$tables$counts_by_year[1:3]

### 16-17 ###

# Subset patients
phis_age2 <- subset(phis_mh, AGE_GRP == "16-17")
nrow(phis_age2)  # 28,199 rows

## Visualize and summarize trends over time
res_age2 <- summarize_growth(phis_age2)

# Plot of hospital counts
res_age2$plots$hosp_counts_by_year

# Plots of percent growth
res_age2$plots$growth_by_year + labs(title = "16-17")
res_age2$plots$growth_by_qtr + labs(title = "16-17")

# Table of percent growth
res_age2$tables$counts_by_year[1:3]

### 18-21 ###

# Subset patients
phis_age3 <- subset(phis_mh, AGE_GRP == "18-21")
nrow(phis_age3)  # 22,762 rows

## Visualize and summarize trends over time
res_age3 <- summarize_growth(phis_age3)

# Plot of hospital counts
res_age3$plots$hosp_counts_by_year

# Plots of percent growth
res_age3$plots$growth_by_year + labs(title = "18-21")
res_age3$plots$growth_by_qtr + labs(title = "18-21")

# Table of percent growth
res_age3$tables$counts_by_year[1:3]

## COMBINATION PLOT
res_age <- data.frame(
  group = c("12-15", "16-17", "18-21"),
  cumulative = c(
    res_age1$tables$counts_by_year$percentgrowth[6],
    res_age2$tables$counts_by_year$percentgrowth[6],
    res_age3$tables$counts_by_year$percentgrowth[6]))
res_age$cumulative <- 100*round(res_age$cumulative, 4)
# Plot
ggplot(res_age, aes(x=group, y=cumulative)) + 
  geom_col() + ylim(c(0,70)) +
  geom_text(aes(label=cumulative), col="white", size=6, vjust=1.5) + 
  labs(x="", y="")


########################
#### INSURANCE TYPE ####
########################

### Government ###

# Subset patients
phis_gov <- subset(phis_mh, INSURANCE_GROUP == "Goverment")
nrow(phis_gov)  # 34,379 rows

## Visualize and summarize trends over time
res_gov <- summarize_growth(phis_gov)

# Plot of hospital counts
res_gov$plots$hosp_counts_by_year

# Plots of percent growth
res_gov$plots$growth_by_year + labs(title = "Government")
res_gov$plots$growth_by_qtr + labs(title = "Government")

# Table of percent growth
res_gov$tables$counts_by_year[1:3]


### Commercial ###

# Subset patients
phis_com <- subset(phis_mh, INSURANCE_GROUP == "Commercial")
nrow(phis_com)  # 26,495 rows

## Visualize and summarize trends over time
res_com <- summarize_growth(phis_com)

# Plot of hospital counts
res_com$plots$hosp_counts_by_year

# Plots of percent growth
res_com$plots$growth_by_year + labs(title = "Commercial")
res_com$plots$growth_by_qtr + labs(title = "Commercial")

# Table of percent growth
res_com$tables$counts_by_year[1:3]


### Other/Unknown ###

# Subset patients
phis_inso <- subset(phis_mh, INSURANCE_GROUP == "Other/Unknown")
nrow(phis_inso)  # 3,743 rows

## Visualize and summarize trends over time
res_inso <- summarize_growth(phis_inso)

# Plot of hospital counts
res_inso$plots$hosp_counts_by_year

# Plots of percent growth
res_inso$plots$growth_by_year + labs(title = "Other/Unknown")
res_inso$plots$growth_by_qtr + labs(title = "Other/Unknown")

# Table of percent growth
res_inso$tables$counts_by_year[1:3]






#############################
#### HOSPITAL DEPARTMENT ####
#############################

### Emergency Department ###

# Subset patients
phis_ed <- subset(phis_mh, ED_FLAG   == 1)
nrow(phis_ed)  # 48,157 rows

## Visualize and summarize trends over time
res_ed <- summarize_growth(phis_ed)

# Plot of hospital counts
res_ed$plots$hosp_counts_by_year

# Plots of percent growth
res_ed$plots$growth_by_year + labs(title = "Emergency Department")
res_ed$plots$growth_by_qtr + labs(title = "Emergency Department")

# Table of percent growth
res_ed$tables$counts_by_year[1:3]


### Inpatient ###

# Subset patients
phis_inp <- subset(phis_mh, INPATIENT_FLAG == 1)
nrow(phis_inp)  # 45,279 rows

## Visualize and summarize trends over time
res_inp <- summarize_growth(phis_inp)

# Plot of hospital counts
res_inp$plots$hosp_counts_by_year

# Plots of percent growth
res_inp$plots$growth_by_year + labs(title = "Inpatient")
res_inp$plots$growth_by_qtr + labs(title = "Inpatient")

# Table of percent growth
res_inp$tables$counts_by_year[1:3]


### Intensive-care-unit ###

# Subset patients
phis_icu <- subset(phis_mh, ICU_FLAG == 1)
nrow(phis_icu)  # 6,328 rows

## Visualize and summarize trends over time
res_icu <- summarize_growth(phis_icu)

# Plot of hospital counts
res_icu$plots$hosp_counts_by_year

# Plots of percent growth
res_icu$plots$growth_by_year + labs(title = "ICU")
res_icu$plots$growth_by_qtr + labs(title = "ICU")

# Table of percent growth
res_icu$tables$counts_by_year[1:3]


### ED Discharge (i.e., ED_FLAG==1 & INPATIENT_FLAG==0 & ICU_FLAG==0)
# Subset patients
phis_eddis <- subset(phis_mh, ED_FLAG==1 & INPATIENT_FLAG==0 & ICU_FLAG==0)
nrow(phis_eddis)  # 18,921 rows
## Visualize and summarize trends over time
res_eddis <- summarize_growth(phis_eddis)
res_eddis$tables$counts_by_year[1:3]

