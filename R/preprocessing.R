# TITLE: preprocessing.R

preprocessing <- function(data) {
  
  # Load relevant packages
  library(dplyr)
  library(lubridate)
  
  ##############################
  #### Create new variables ####
  ##############################
  
  ## Create admission year and quarter variables
  data <- data %>%
    mutate(
      ADMIT_DATE = lubridate::ymd(ADMIT_DATE),
      ADMIT_YEAR = lubridate::year(ADMIT_DATE),       # Year of admission
      ADMIT_MONTH = lubridate::month(ADMIT_DATE),     # Month of admission
      # ADMIT_SEASON = lubridate::??(ADMIT_DATE),  # Season of admission
      ADMIT_QTR = lubridate::quarter(ADMIT_DATE)      # Quarter of admission
    )
  # Select admissions that begin and end between 2016-2021
  data <- data %>% subset(ADMIT_YEAR %in% 2016:2021)
  
  # Create quarter index variable (continues past 4)
  min_year <- min(data$ADMIT_YEAR, na.rm = TRUE)
  data <- data %>% mutate(
    ADMIT_QTR_IDX = ADMIT_QTR + 4*(ADMIT_YEAR - min_year))
  
  # Alternative to creating quarter index variable
  temp <- with(data, lubridate::floor_date(ADMIT_DATE, "quarter"))
  
  
  ## Create age group variable
  data$AGE_GRP <- cut(
    data$AGE_YRS,
    breaks = c(12, 15, 17, 21),
    labels = c("12-15", "16-17", "18-21"),
    right = TRUE,
    include.lowest = TRUE)
  
  ##########################
  #### Modify variables ####
  ##########################
  
  # Convert cluster variable to factor
  data$HOSPITAL <- as.factor(data$HOSPITAL)
  
  # Convert MH_ANY variable to binary
  data$MH_ANY <- ifelse(data$MH_ANY == "One or more", 1, 0)
  
  return(data)
}