#--------------------------------------
# This script sets out to simulate
# synthetic time series to use as the
# basis for computation time analysis
#
# NOTE: setup.R must be run first
#--------------------------------------

#----------------------------------------
# Author: Trent Henderson, 20 August 2021
#----------------------------------------

#' Helper function to generate synthetic data
#' 
#' @param n the length of the time series to generate
#' @param nsims the number of time series of length n to generate
#' @author Trent Henderson
#' 

generate_processes <- function(n, nsims = 10){
  
  set.seed(123)
  
  # Normal version
  
  for(i in 1:nsims){
    
    tmp <- data.frame(values = c(rnorm(n, mean = 0, sd = 1)))
    
    write.csv(tmp, paste0("data/sims/",n,"_",i,".csv"))
  }
  
  # Add datetime for Kats
  
  for(i in 1:nsims){
    
    tmp <- data.frame(value = c(rnorm(n, mean = 0, sd = 1))) %>%
      mutate(timepoint = row_number())
    
    unique_times <- unique(tmp$timepoint)
    
    datetimes <- data.frame(timepoint = unique_times) %>%
      dplyr::mutate(time = seq(as.Date("1800-01-01"), by = "day", length.out = length(unique_times)))
    
    # Join in datetimes
    
    tmp2 <- tmp %>%
      dplyr::left_join(datetimes, by = c("timepoint" = "timepoint")) %>%
      dplyr::select(c(time, value))
    
    write.csv(tmp2, paste0("data/sims/kats/",n,"_",i,".csv"))
  }
  
  # Make no column header version for Latbal
  
  for(i in 1:nsims){
    
    values <- rnorm(n, mean = 0, sd = 1)
    write.table(values, paste0("data/sims/hctsa/",n,"_",i,".csv"), col.names = FALSE, sep = ",", row.names = FALSE)
  }
}

# Generate the data

generate_processes(n = 100, nsims = 10)
generate_processes(n = 250, nsims = 10)
generate_processes(n = 500, nsims = 10)
generate_processes(n = 750, nsims = 10)
generate_processes(n = 1000, nsims = 10)
