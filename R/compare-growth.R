

compare_growth <- function(res1, res2, name1, name2) {
  
  ## Extract information from results
  # Get time values
  quarters <- res1$counts_by_qtr$ADMIT_QTR_IDX
  nquarters <- length(quarters)
  years <- res1$counts_by_year$ADMIT_YEAR
  nyears <- length(years)
  # Growth by quarter
  growth_qtr1 = res1$counts_by_qtr$percentgrowth
  growth_qtr2 = res2$counts_by_qtr$percentgrowth
  # Growth by year
  growth_year1 = res1$counts_by_year$percentgrowth
  growth_year2 = res2$counts_by_year$percentgrowth
  
  ## PLOT TRENDS
  # Quarter
  dat_qtr <- data.frame(
    times = c(quarters, quarters),
    growth = c(growth_qtr1, growth_qtr2),
    Characteristic = c(rep(name1, nquarters), rep(name2, nquarters)))
  
  gg_qtr <- ggplot(dat_qtr, aes(x=times, y=growth, color=Characteristic)) +
    # Add line for onset of COVID
    geom_vline(xintercept=18, color="black", lwd=0.5) +
    # Visualize trends in percent growth
    geom_line() +
    # geom_smooth(se=FALSE) +
    # Label axes
    labs(x="Quarter of admission", y="Percent growth") +
    scale_x_continuous(breaks = seq(1,24,by=4),
                       labels = paste0(2016:2021,"-Q1")) +
    # Add legend
    theme(legend.position="inside", legend.position.inside=c(.15, .8)) +
    scale_color_manual(values=c("darkred", "orange")) +
    # Modify theme
    theme_bw()
  
  # Year
  dat_year <- data.frame(
    times = c(years, years),
    growth = c(growth_year1, growth_year2),
    Characteristic = c(rep(name1, nyears), rep(name2, nyears)))
  
  gg_year <- ggplot(dat_year, aes(x=times, y=growth, color=Characteristic)) +
    # Add line for onset of COVID
    geom_vline(xintercept=2020, color="black", lwd=0.5) +
    # Visualize trends in percent growth
    geom_line() +
    geom_point() +
    # Label axes
    labs(x="Year of admission", y="Percent growth") +
    # Add legend
    theme(legend.position="inside", legend.position.inside=c(.15, .8)) +
    scale_color_manual(values=c("darkred", "orange")) +
    # Modify theme
    theme_bw()
  
  return(list(gg_year=gg_year, 
              gg_qtr=gg_qtr))
}