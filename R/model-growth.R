# TITLE: model-growth.R
# OUTPUT

model_growth <- function(yeardata, qtrdata) {

  # Load relevant packages
  library(geepack)  # generalized estimating equations

  # Arrange data by cluster (NECESSARY FOR `GEEGLM()` TO WORK PROPERLY) 
  yeardata <- yeardata %>% arrange(HOSPITAL)
  qtrdata <- qtrdata %>% arrange(HOSPITAL)
  
  # Model annual averages with generalized estimating equations
  geefit.year <- geepack::geeglm(formula = count ~ ADMIT_YEAR,
                                 data = yeardata,
                                 id = HOSPITAL,
                                 corstr = "ar1")
  
  # Model quarterly averages with generalized estimating equations
  geefit.qtr <- geeglm(formula = count ~ ADMIT_QTR_IDX,
                       data = qtrdata,
                       id = HOSPITAL,
                       corstr = "ar1")
  
  return(list(geefit.year=geefit.year,
              geefit.qtr=geefit.qtr))
}