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

# Load data
phis_raw <- read_csv(file = "data/mh_subs_uw.csv")  # Ensure this is the right directory
nrow(phis_raw)  # 106,793 rows
names(phis_raw)

# Preprocess data
source("R/preprocessing.R")
phis <- preprocessing(phis_raw)

# What are the 10 most popular mental health conditions?
phis %>% select(starts_with("mh_"), dependence) %>% colSums %>% 
  data.frame(count = .) %>% mutate(perc = round(count / nrow(phis), 4)) %>%
  arrange(desc(count)) %>% head(7)


phis_mh <- subset(phis, MH_ANY == 1)
top_conditions <- phis_mh %>% select(starts_with("mh_"), dependence) %>% colSums %>% 
  data.frame(Freq = .) %>% mutate(Perc = 100*round(Freq/nrow(phis_mh), 4)) %>%
  arrange(desc(Freq)) %>% head(7)
top_conditions$Condition <- rownames(top_conditions)
top_conditions <- top_conditions[-1,]

top_conditions <- top_conditions %>%
  mutate(Condition = case_when(
    Condition == "mh_7" ~ "Depressive",
    Condition == "mh_29" ~ "Self-Injury",
    Condition == "mh_3" ~ "Anxiety",
    Condition == "dependence" ~ "Substance Dependence",
    Condition == "mh_2" ~ "ADHD",
    Condition == "mh_30" ~ "Trauma/Stress",
    TRUE ~ "Other"))

top_conditions %>%
  ggplot(aes(y=Perc, x=Condition)) + 
    geom_col() + ylim(c(0,60)) +
    geom_text(aes(label=Perc), col="white", vjust=1.5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x="", y="")

######################
#### ALL PATIENTS ####
######################

nrow(phis)  # 106,693 rows

# Describe data
source("R/describe-data.R")
tab_all <- describe_data(phis)$subgroup_table  # Subgroups have sufficient size

## Visualize and summarize trends over time
source("R/summarize-growth.R")
res_all <- summarize_growth(phis)

# Plot of hospital counts
res_all$plots$hosp_counts_by_year

# Plots of percent growth
res_all$plots$counts_by_year + labs(title = "Substance-related visits")
res_all$plots$counts_by_qtr + labs(title = "Substance-related visits")

# Table of percent growth
res_all$tables$counts_by_year

###########################
#### ANY MENTAL HEATLH ####
###########################

# Describe data
tab_mh <- describe_data(phis, group_by = "MH_ANY")$subgroup_table  # Subgroups have sufficient size

# Subset patients with any mental health diagnosis
phis_mh <- subset(phis, MH_ANY == 1)
nrow(phis_mh)  # 64,617 rows

## Visualize and summarize trends over time
res_mh <- summarize_growth(phis_mh)

# Plot of hospital counts
res_mh$plots$hosp_counts_by_year

# Plots of percent growth
res_mh$plots$growth_by_year + labs(title = "Any mental health")
res_mh$plots$growth_by_qtr + labs(title = "Any mental health")

# Table of percent growth
res_mh$tables$counts_by_year

## Emergency department utilization
phis %>% select(MH_ANY, ED_FLAG) %>% table %>% addmargins %>% chisq.test()
# Related to ED visitation (p<0.001)

## Inpatient utilization
phis %>% select(MH_ANY, INPATIENT_FLAG) %>% table %>% addmargins %>% chisq.test()
# Related to inpatient admission (p<0.001)

## ICU utilization
phis %>% select(MH_ANY, ICU_FLAG) %>% table %>% addmargins %>% chisq.test()
# Related to ICU admission (p<0.001)


###########################
#### NO MENTAL HEATLH ####
###########################

# Subset patients with no mental health diagnosis
phis_nmh <- subset(phis, MH_ANY == 0)
nrow(phis_nmh)  # 42,076 rows

## Visualize and summarize trends over time
res_nmh <- summarize_growth(phis_nmh)

# Plot of hospital counts
res_nmh$plots$hosp_counts_by_year

# Plots of percent growth
res_nmh$plots$growth_by_year + labs(title = "No mental health")
res_nmh$plots$growth_by_qtr + labs(title = "No mental health")

# Table of percent growth
res_nmh$tables$counts_by_year

#### MH versus NO MH

## QUARTER
times <- res_mh$tables$counts_by_qtr$ADMIT_QTR_IDX
covid_qtr_idx <- 4 * 4 + 2  # 4 qtrs from 2016-19 (4 yrs), then the second qtr
mh_growth <- res_mh$tables$counts_by_qtr$percentgrowth
nmh_growth <- res_nmh$tables$counts_by_qtr$percentgrowth
dat.qtr <- data.frame(
  times = c(times, times),
  growth = c(mh_growth, nmh_growth),
  Characteristic = c(rep("Any mental health", length(times)),
                     rep("Not mental health", length(times)))
)
ggplot(dat.qtr, aes(x=times, y=growth, color=Characteristic)) +
  # Add line for second quarter of 2020
  geom_vline(xintercept = covid_qtr_idx, color = "black", lwd=0.5) +
  # Visualize trends in percent growth
  geom_line() + 
  geom_line() +
  # Label axes
  labs(x="Quarter of admission", y="Percent growth") +
  scale_x_continuous(breaks = seq(1,24,by=4),
                     labels = 2016:2021) +
  # Add legend
  theme(legend.position="inside", legend.position.inside=c(.15, .8)) +
  scale_color_manual(values=c("darkred", "orange")) +
  # Modify theme
  theme_bw()


## YEAR
times <- res_mh$tables$counts_by_year$ADMIT_YEAR
covid_year <- 2020
mh_growth <- res_mh$tables$counts_by_year$percentgrowth
nmh_growth <- res_nmh$tables$counts_by_year$percentgrowth

dat.year <- data.frame(
  times = c(times, times),
  growth = c(mh_growth, nmh_growth),
  Characteristic = c(rep("Any mental health", length(times)),
                     rep("Not mental health", length(times)))
)

ggplot(dat.year, aes(x=times, y=growth, color=Characteristic)) +
  # Add line for 2020
  geom_vline(xintercept = covid_year, color = "black", lwd=0.5) +
  # Visualize trends in percent growth
  geom_line() + 
  geom_line() +
  geom_point() + 
  geom_point() +
  # Label axes
  labs(x="Year of admission", y="Percent growth") +
  ylim(0,0.6) +
  # Add legend
  theme(legend.position="inside", legend.position.inside=c(.15, .8)) +
  scale_color_manual(values=c("darkred", "orange")) +
  # Modify theme
  theme_bw()



#########################
#### MENTAL HEATLH 7 ####
#########################

# Describe data
tab_mh7 <- describe_data(phis, group_by = "mh_7")$subgroup_table  # 11 subsamples are smaller than 30

# Subset patients with MH_7 diagnosis
phis_mh7 <- subset(phis, mh_7 == 1)
nrow(phis_mh7)  # 35,677 rows

## Visualize and summarize trends over time
res_mh7 <- summarize_growth(phis_mh7)

# Plot of hospital counts
res_mh7$plots$hosp_counts_by_year

# Plots of percent growth
res_mh7$plots$growth_by_year + labs(title = "Depressive Disorders")
res_mh7$plots$growth_by_qtr + labs(title = "Depressive Disorders")

# Table of percent growth
res_mh7$tables$counts_by_year

### Trend of Hospital Admission ###
res_mh7inpatient <- summarize_growth(subset(phis, INPATIENT_FLAG==1 & mh_7==1))
res_mh7inpatient$tables$counts_by_year[1:3]
res_mh7inpatient$plots$growth_by_qtr + 
  labs(title = "Inpatient Admissions for Depressive Disorders")


##########################
#### MENTAL HEATLH 29 ####
##########################

# Describe data
tab_mh29 <- describe_data(phis, group_by = "mh_29")$subgroup_table  # 13 subsamples are smaller than 30

# Subset patients with MH_29 diagnosis
phis_mh29 <- subset(phis, mh_29 == 1)
nrow(phis_mh29)  # 30,041 rows

## Visualize and summarize trends over time
res_mh29 <- summarize_growth(phis_mh29)

# Plot of hospital counts
res_mh29$plots$hosp_counts_by_year

# Plots of percent growth
res_mh29$plots$growth_by_year + labs(title = "Suicide or Self-Injury")
res_mh29$plots$growth_by_qtr + labs(title = "Suicide or Self-Injury")

# Table of percent growth
res_mh29$tables$counts_by_year

### Trend of Hospital Admission ###
res_mh29inpatient <- summarize_growth(subset(phis, INPATIENT_FLAG==1 & mh_29==1))
res_mh29inpatient$tables$counts_by_year[1:3]
res_mh29inpatient$plots$growth_by_qtr + 
  labs(title = "Inpatient Admissions for Self-Injury or Suicide")



# Describe data
tab_mhdepend <- describe_data(phis, group_by = "dependence")$subgroup_table
# Subset patients with DEPENDENCE diagnosis
phis_mhdepend <- subset(phis, dependence == 1)
nrow(phis_mhdepend)
## Visualize and summarize trends over time
res_mhdepend <- summarize_growth(phis_mhdepend)
res_mhdepend$tables$counts_by_year$percentgrowth

# Describe data
tab_mh3 <- describe_data(phis, group_by = "mh_3")$subgroup_table
# Subset patients with MH_3 diagnosis
phis_mh3 <- subset(phis, mh_3 == 1)
nrow(phis_mh3)
## Visualize and summarize trends over time
res_mh3 <- summarize_growth(phis_mh3)
res_mh3$tables$counts_by_year$percentgrowth

# Describe data
tab_mh2 <- describe_data(phis, group_by = "mh_2")$subgroup_table
# Subset patients with MH_2 diagnosis
phis_mh2 <- subset(phis, mh_2 == 1)
nrow(phis_mh2)
## Visualize and summarize trends over time
res_mh2 <- summarize_growth(phis_mh2)
res_mh2$tables$counts_by_year$percentgrowth

# Describe data
tab_mh30 <- describe_data(phis, group_by = "mh_30")$subgroup_table
# Subset patients with MH_30 diagnosis
phis_mh30 <- subset(phis, mh_30 == 1)
nrow(phis_mh30)
## Visualize and summarize trends over time
res_mh30 <- summarize_growth(phis_mh30)
res_mh30$tables$counts_by_year$percentgrowth


