#Function that calculates the differences between oceanic and nonoceanic parameter
#estimates from each model parameter across the nonoceanic parameter space
#The function requires a dataframe with all ML estiamtes and simulating values,
#currently


model_param_delta <- function(data, island_age, clado_rate, ext_rate, immig_rate = NULL, 
                              ana_rate = NULL, mainland, nonendemic, clado_diff = TRUE, 
                              ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE) {
  
  testit::assert(sum(c(clado_diff,ext_diff,immig_diff,ana_diff)) == 1)
  
  DAISIEdata <- as_tibble(DAISIETable)
  
  #separate data into oceanic and nonoceanic tibbles
  oceanic <- dplyr::filter(DAISIEdata, island_type == 'oceanic')

  #separate data into oceanic and nonoceanic tibbles
  nonoceanic <- dplyr::filter(DAISIEdata, island_type == 'nonoceanic')
  
  #isolate data for a given island age
  oceanic_time <- dplyr::filter(oceanic, time == island_age)
  
  #isolate island age of 4
  nonoceanic_time <- dplyr::filter(nonoceanic, time == island_age)
  
  if (clado_diff == TRUE) {
    relative_delta <- clado_param_delta()
  }
  
  if (ext_diff == TRUE) {
    relative_delta <- ext_param_delta()
  }
  
  if (immig_diff == TRUE) {
    relative_delta <- immig_param_delta()
  }
  
  if (ana_diff == TRUE) {
    relative_delta <- ana_param_delta()
  }
  return(relative_delta)
}


