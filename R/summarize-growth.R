# TITLE: summarize-growth.R
# OUTPUT
#   Plot of total counts at year and quarterly scales
#   Plot of average count at year and quarterly scales
#   Plot of percent growth at year and quarterly scales
#   Table of total counts at year and quarterly scales
#   Table of average count at year and quarterly scales
#   Table of percent growth at year and quarterly scales

summarize_growth <- function (data) {

  # Load relevant packages
  library(dplyr)
  library(ggplot2)
  
  # Store min/max years
  min_year <- min(data$ADMIT_YEAR, na.rm = TRUE)
  max_year <- max(data$ADMIT_YEAR, na.rm = TRUE)
  covid_year <- 2020
  
  # Store min/max quarter indices
  min_qtr <- min(data$ADMIT_QTR_IDX, na.rm = TRUE)
  max_qtr <- max(data$ADMIT_QTR_IDX, na.rm = TRUE)
  covid_qtr_idx <- 4 * 4 + 2  # 4 qtrs from 2016-19 (4 yrs), then the second qtr
  
  
  ##########################
  #### Summarize trends ####
  ##########################
  
  ## ANNUAL TRENDS
  # Total number of visits by year 
  counts_by_year <- data %>% 
    summarise(count = n(), .by = ADMIT_YEAR) %>%
    arrange(ADMIT_YEAR)
  # Percent change in visits by year
  reference_count <- subset(counts_by_year, ADMIT_YEAR == min_year)$count
  counts_by_year <- counts_by_year %>%
    mutate(percentgrowth = (count - reference_count) / reference_count)
  # Total visits by hospital by year
  hosp_counts_by_year <- data %>% 
    summarise(count = n(), .by = c(HOSPITAL, ADMIT_YEAR)) %>%
    arrange(HOSPITAL, ADMIT_YEAR)
  
  ## QUARTERLY TRENDS
  # Total number of visits by quarter 
  counts_by_qtr <- data %>% 
    summarise(count = n(), .by = ADMIT_QTR_IDX) %>%
    arrange(ADMIT_QTR_IDX)
  # Percent change in visits by quarter
  reference_count <- subset(counts_by_qtr, ADMIT_QTR_IDX == min_qtr)$count
  counts_by_qtr <- counts_by_qtr %>%
    mutate(percentgrowth = (count - reference_count) / reference_count)
  # Total visits by hospital by quarter
  hosp_counts_by_qtr <- data %>% 
    summarise(count = n(), .by = c(HOSPITAL, ADMIT_QTR_IDX)) %>%
    arrange(ADMIT_QTR_IDX)
  
  ######################
  #### Model trends ####
  ######################
  
  # Fit a model of the average number of visits by year and quarter
  res_models <- model_growth(yeardata=hosp_counts_by_year,
                             qtrdata=hosp_counts_by_qtr)
  
  ## ANNUAL TRENDS
  # Get estimates
  hosp_counts_by_year$geefit <- res_models$geefit.year$fitted.values[,1]
  
  # Estimate total number of visits by year 
  fit_counts_by_year <- hosp_counts_by_year %>%
    summarise(fitcount = sum(geefit), .by = ADMIT_YEAR)
  counts_by_year$fitcount <- fit_counts_by_year$fitcount
  
  # Estimate percent change in visits by year
  reference_count <- subset(fit_counts_by_year, ADMIT_YEAR == min_year)$fitcount
  fit_counts_by_year <- fit_counts_by_year %>%
    mutate(fitpercentgrowth = (fitcount - reference_count) / reference_count)
  counts_by_year$fitpercentgrowth <- fit_counts_by_year$fitpercentgrowth

  
  ## QUARTERLY TRENDS
  # Get estimates
  hosp_counts_by_qtr$geefit <- res_models$geefit.qtr$fitted.values[,1]
  
  # Estimate total number of visits by quarter
  fit_counts_by_qtr <- hosp_counts_by_qtr %>%
    summarise(fitcount = sum(geefit), .by = ADMIT_QTR_IDX)
  counts_by_qtr$fitcount <- fit_counts_by_qtr$fitcount
  
  # Estimate percent change in visits by quarter
  reference_count <- subset(fit_counts_by_qtr, ADMIT_QTR_IDX  == min_qtr)$fitcount
  fit_counts_by_qtr <- fit_counts_by_qtr %>%
    mutate(fitpercentgrowth = (fitcount - reference_count) / reference_count)
  counts_by_qtr$fitpercentgrowth <- fit_counts_by_qtr$fitpercentgrowth
  
  
  #####################
  #### Plot trends ####
  #####################
  
  # Color selection
  colors <- c("#FC600A", # dark orange
              "#C21460", # dark pink
              "#3F0000",
              "#ADD8E6",
              "#00008B") # dark red
  
  ## ANNUAL TRENDS
  ## Total visits by year
  gg_counts_by_year <- counts_by_year %>%
    ggplot(aes(x = ADMIT_YEAR)) +
    # Draw line at average count in time period
    geom_hline(yintercept = mean(counts_by_year$count), col = "grey") +
    # Draw line at the year of COVID onset
    geom_vline(xintercept = covid_year, col = "red") +
    # Visualize observed trend in number of visits
    geom_line(aes(y = count)) + 
    geom_point(aes(y = count)) +
    # Visualize estimated trend in number of visits
    geom_line(aes(y = fitcount)) + 
    geom_point(aes(y = fitcount)) +
    # Label axes
    xlab("Year of admission") + ylab("Number of visits") +
    # Modify theme
    theme_bw() +
    # Modify x-axis breaks
    scale_x_continuous(breaks = min_year:max_year)
  
  ## Percent growth by year
  gg_growth_by_year <- counts_by_year %>%
    ggplot(aes(x = ADMIT_YEAR)) +
    # Draw line at baseline
    geom_hline(yintercept = 0, col = "red") +
    # Visualize observed trend in percent growth
    geom_line(aes(y = percentgrowth)) + 
    geom_point(aes(y = percentgrowth)) +
    # Visualize estimated trend in percent growth
    geom_line(aes(y = fitpercentgrowth)) + 
    geom_point(aes(y = fitpercentgrowth)) +
    # Label axes
    xlab("Year of admission") + ylab("Percent growth") +
    # Modify theme
    theme_bw() +
    # Modify x-axis breaks
    scale_x_continuous(breaks = min_year:max_year)
  
  ## Total visits by hospital by year
  gg_hosp_counts_by_year <- hosp_counts_by_year %>% 
    ggplot(aes(y = count, x = ADMIT_YEAR)) +
    # Plot hospital counts by year
    geom_line(aes(group = HOSPITAL), col = colors[1], alpha = 0.3) +
    # Add means
    geom_point(data = hosp_counts_by_year %>%
                 summarise(mean = mean(count), .by = ADMIT_YEAR),
               aes(x = ADMIT_YEAR, y = mean, color = "Median"),  size = 3) +
    # Add medians
    geom_point(data = hosp_counts_by_year %>%
                 summarise(median = median(count), .by = ADMIT_YEAR),
               aes(x = ADMIT_YEAR, y = median, color = "Mean"), size = 3) +
    # Create manual legend
    scale_color_manual(
      name = 'Data Summary',
      breaks = c('Median', 'Mean'),
      values = c('Median' = colors[2], 'Mean' = colors[3])) +
    labs(x = "Year of admission", y = "Number of visits") +
    theme_bw() +
    scale_x_continuous(breaks = min_year:max_year) +
    theme(axis.text.x = element_text(angle = 70, vjust = 0.5))
  
  
  ## QUARTERLY TRENDS
  ## Total visits by quarter
  gg_counts_by_qtr <- counts_by_qtr %>%
    ggplot(aes(x = ADMIT_QTR_IDX)) +
    # Draw line at average count in time period
    geom_hline(yintercept = mean(counts_by_qtr$count), col = "darkblue") +
    # Add quarter of COVID onset
    geom_vline(xintercept = covid_qtr_idx, col = "red") +
    # Visualize observed trend in number of visits
    geom_smooth(aes(y = count)) + # Smooth fit
    geom_line(aes(y = count)) +   # Line plot
    geom_point(aes(y = count)) +  # Scatter plot
    # Visualize estimated trend in number of visits
    geom_smooth(aes(y = fitcount)) +
    geom_line(aes(y = fitcount)) +
    geom_point(aes(y = fitcount)) +
    # Label axes
    xlab("Quarter of admission") + ylab("Number of visits") +
    # Modify theme
    theme_bw() +
    # Modify x-axis breaks
    scale_x_continuous(breaks = seq(min_qtr, max_qtr, by = 4))
  
  ## Percent growth by quarter
  gg_growth_by_qtr <- counts_by_qtr %>%
    ggplot(aes(x = ADMIT_QTR_IDX)) +
    # Draw line at baseline
    geom_hline(yintercept = 0, col = "black") +
    # Visualize observed trend in percent growth
    geom_smooth(aes(y = percentgrowth)) +
    geom_line(aes(y = percentgrowth)) + 
    geom_point(aes(y = percentgrowth)) +
    # Visualize estimated trend in percent growth
    geom_smooth(aes(y = fitpercentgrowth)) +
    geom_line(aes(y = fitpercentgrowth)) + 
    geom_point(aes(y = fitpercentgrowth)) +
    # Draw line at the quarter of COVID onset
    geom_vline(xintercept = covid_qtr_idx, col = "red") +
    # Label axes
    xlab("Quarter of admission") + ylab("Percent growth") +
    # Modify theme
    theme_bw() +
    # Modify x-axis breaks
    scale_x_continuous(breaks = seq(min_qtr, max_qtr, by = 4))
  
  #### Results ####
  res_tables <- list(
    counts_by_year = counts_by_year, 
    counts_by_qtr = counts_by_qtr, 
    hosp_counts_by_year = hosp_counts_by_year, 
    hosp_counts_by_qtr = hosp_counts_by_qtr)
  res_plots <- list(
    counts_by_year = gg_counts_by_year,
    growth_by_year = gg_growth_by_year,
    hosp_counts_by_year = gg_hosp_counts_by_year,
    counts_by_qtr = gg_counts_by_qtr,
    growth_by_qtr = gg_growth_by_qtr)
  
  return(list(data=data, model=res_models, tables=res_tables, plots=res_plots))
}
