
describe_data <- function(data, group_by=NULL, message=TRUE) {
  
  # Load relevant packages
  library(dplyr)
  
  # Prepare message
  msg <- ""
  
  # Flag for whether a grouping variable was provided
  stratify <- !is.null(group_by)
  
  # Define a subset by the grouping variable (if given)
  # Otherwise, this is just the data
  data_subset <- data
  if (stratify) { 
    data_subset <- data[data[group_by]==1,]
    msg <- paste0(msg, "Among subset ", group_by, "==1:", "\n")
    }
  
  # Count number of unique/repeat visitors and visits 
  visitors_tab <- data_subset %>%
    group_by(XPT) %>% 
    summarize(count = n()) %>% arrange(desc(count)) %>% 
    mutate(repeated = ifelse(count > 1, 1, 0))
  
  n.visits <- sum(visitors_tab$count)
  n.visitors <- nrow(visitors_tab)
  n.repeatvisitors <- sum(visitors_tab$repeated)
  perc.repeatvisitors <- round(100*n.repeatvisitors/n.visitors)
  
  msg <- paste0(msg, "There were ", n.visits, " substance-related visits from ", 
                n.visitors, " individuals, of whom ",
                n.repeatvisitors, " (", perc.repeatvisitors, 
                "%) visited multiple times.", "\n")
  
  
  # Count number of clusters
  n.clusters <- data_subset$HOSPITAL %>% unique %>% length
  msg <- paste0(msg, "There are ", n.clusters, " clusters.", "\n")
  
  # Describe sub-sample sizes (this uses the full data)
  subgroup_tab <- data %>% 
    select(any_of(c("ADMIT_YEAR", "HOSPITAL", group_by))) %>% 
    table
  msg <- paste0(msg, "There are ", sum(subgroup_tab < 30), " subgroups across ",
                ifelse(!is.null(group_by), paste0(group_by, ", "), ""),
                "HOSPITAL and YEAR with fewer than 30 individuals.")
  nzero <- sum(subgroup_tab == 0)
  if (nzero > 0) { msg <- paste(msg, nzero, "are empty (size 0).") }
  
  # Print message 
  if (message) cat(msg)
  
  return(list(visits=visitors_tab, subgroups=subgroup_tab))
}
