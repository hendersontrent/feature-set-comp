#--------------------------------------
# This script sets out to simulate
# synthetic time series to use as the
# basis for computation time analysis
#
# NOTE: setup.R must be run first
#--------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 24 September 2021
#-------------------------------------------

#' Helper function to generate synthetic data
#' 
#' @param n the length of the time series to generate
#' @param nsims the number of time series of length n to generate
#' @author Trent Henderson
#' 

generate_processes_sinusoid <- function(n, nsims = 10){
  
  set.seed(123)
  
  # Normal version
  
  for(i in 1:nsims){
    
    x <- seq(0, 3 * pi, length.out = n)
    epsilon <- rnorm(n, mean = 0, sd = 1)
    y <- 2 * sin(2 * x) + epsilon
    tmp <- data.frame(value = y)
    
    write.csv(tmp, paste0("data/sims/sinusoid/",n,"_",i,".csv"))
    
    # Add datetime for Kats
    
    tmp2 <- tmp %>%
      mutate(timepoint = row_number())
    
    unique_times <- unique(tmp2$timepoint)
    
    datetimes <- data.frame(timepoint = unique_times) %>%
      dplyr::mutate(time = seq(as.Date("1800-01-01"), by = "day", length.out = length(unique_times)))
    
    # Join in datetimes
    
    tmp3 <- tmp2 %>%
      dplyr::left_join(datetimes, by = c("timepoint" = "timepoint")) %>%
      dplyr::select(c(time, value))
    
    write.csv(tmp3, paste0("data/sims/sinusoid/kats/",n,"_",i,".csv"))
    
    # Make no column header version for Matlab
    
    values <- tmp$value
    write.table(values, paste0("data/sims/sinusoid/hctsa/",n,"_",i,".csv"), col.names = FALSE, sep = ",", row.names = FALSE)
  }
}

# Generate the data

generate_processes_sinusoid(n = 100, nsims = 10)
generate_processes_sinusoid(n = 250, nsims = 10)
generate_processes_sinusoid(n = 500, nsims = 10)
generate_processes_sinusoid(n = 750, nsims = 10)
generate_processes_sinusoid(n = 1000, nsims = 10)
