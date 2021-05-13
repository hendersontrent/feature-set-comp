#-------------------------------------
# Some {theft} helper functions to use
# until issues with {theft} are patched
#-------------------------------------

#-------------------------------------
# Author: Trent Henderson, 10 May 2021
#-------------------------------------

#------------- tsfresh ------------

calc_tsfresh <- function(data, column_id = "id", column_sort = "timepoint", cleanup){
  
  cleanuper <- cleanup
  
  # Load Python function
  
  reticulate::source_python(system.file("python", "tsfresh_calculator.py", package = "theft")) # Ships with package
  
  # Convert time index column to numeric to avoid {tsfresh} errors
  
  if(!is.numeric(data$id) || !is.numeric(data$timepoint)){
    
    ids <- data.frame(old_id = unique(data$id)) %>%
      dplyr::mutate(id = dplyr::row_number())
    
    temp <- data %>%
      dplyr::rename(old_id = id) %>%
      dplyr::left_join(ids, by = c("old_id" = "old_id")) %>%
      dplyr::group_by(id) %>%
      dplyr::arrange(timepoint) %>%
      dplyr::mutate(timepoint = as.numeric(dplyr::row_number())) %>%
      dplyr::ungroup()
    
    # Dropping columns with dplyr::select() isn't working, so just make a new dataframe
    
    temp1 <- data.frame(id = temp$id,
                        timepoint = temp$timepoint,
                        values = temp$values)
    
    # Compute features and re-join back correct id labels
    
    ids2 <- ids %>%
      dplyr::select(-c(id)) %>%
      dplyr::rename(id = old_id)
    
    outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanuper) %>%
      cbind(ids2) %>%
      tidyr::gather("names", "values", -id) %>%
      dplyr::mutate(method = "tsfresh")
    
  } else{
    temp1 <- data.frame(id = data$id,
                        timepoint = data$timepoint,
                        values = data$values)
    
    ids <- unique(temp1$id)
    
    # Do calculations
    
    outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanuper) %>%
      mutate(id = ids) %>%
      tidyr::gather("names", "values", -id) %>%
      dplyr::mutate(method = "tsfresh")
  }
  
  return(outData)
}

#------------- TSFEL --------------

calc_tsfel <- function(data){
  
  # Load Python function
  
  reticulate::source_python(system.file("python", "tsfel_calculator.py", package = "theft")) # Ships with package
  
  # Vectorised
  
  outData <- data %>%
    tibble::as_tibble() %>%
    dplyr::group_by(id) %>%
    dplyr::arrange(timepoint) %>%
    dplyr::summarise(tsfel_calculator(values)) %>%
    dplyr::ungroup() %>%
    tidyr::gather("names", "values", -id) %>%
    dplyr::mutate(method = "TSFEL")
  
  return(outData)
}
