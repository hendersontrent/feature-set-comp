#--------------------------------------
# This script sets out to load in hctsa
# results if they are formatted as .csv
#--------------------------------------

#----------------------------------------
# Author: Trent Henderson, 23 August 2021
#----------------------------------------

process_hctsa_csv <- function(){
  
  #------------ Main data matrix ------------
  
  dataMatrix <- readr::read_csv("data/TS_DataMat.csv", col_names = FALSE) %>%
    mutate(id = row_number()) %>%
    pivot_longer(cols = 1:7730, names_to = "names", values_to = "values") %>%
    mutate(names = gsub("X", "\\1", names),
           names = as.numeric(names))
  
  #------------ Time Series -----------------
  
  TimeSeries <- readr::read_csv("data/TimeSeries.csv", col_names = TRUE) %>%
    dplyr::select(c(ID, Keywords)) %>%
    rename(id = ID,
           group = Keywords)
  
  #------------ Operations ------------------
  
  operations <- readr::read_csv("data/Operations.csv", col_names = TRUE) %>%
    dplyr::select(c(ID, CodeString)) %>%
    rename(featID = ID)
  
  #------------ Merge together --------------
  
  hctsaOutput <- dataMatrix %>%
    left_join(operations, by = c("names" = "featID")) %>%
    left_join(TimeSeries, by = c("id" = "id")) %>%
    dplyr::select(-c(names)) %>%
    rename(names = CodeString) %>%
    mutate(method = "hctsa",
           id = as.character(id))
  
  return (hctsaOutput)
}
