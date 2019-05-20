#Function that calculates the differences between oceanic and nonoceanic parameter
#estimates from each model parameter across the nonoceanic parameter space
#The function requires a dataframe with all ML estiamtes and simulating values,
#currently

model_differences <- function(data, island_age, clado_rate, ext_rate, immig_rate, ana_rate,
                              mainland, nonendemic, clado_diff = TRUE, 
                              ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE) {
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
    #isolate for a given value of cladogenesis
    oceanic_time_lac <- dplyr::filter(oceanic_time, lambda_c_sim == clado_rate)  
    
    #isolate for a given value of extinction
    oceanic_time_lac_mu <- dplyr::filter(oceanic_time_lac, mu_sim == ext_rate)
    
    #calculate absolute difference between ML estimate and true value
    oceanic_abs_diff <- dplyr::mutate(oceanic_time_lac_mu, abs(lambda_c - lambda_c_sim))
    
    #remove any NA values
    oceanic_abs_diff <- tidyr::drop_na(oceanic_abs_diff)
    
    #deal with any outliers
    #anomalize::anomalize(oceanic_abs_diff, oceanic_abs_diff$lambda_c)
    
    #calculate the average of the absolute differences
    oceanic_mean_lac <- mean(oceanic_abs_diff$lambda_c)
    
    #isolate for a given value of cladogenesis
    nonoceanic_time_lac <- dplyr::filter(nonoceanic_time, lambda_c_sim == clado_rate)
    
    #isolate for a given value of extinction
    nonoceanic_time_lac_mu <- dplyr::filter(nonoceanic_time_lac, mu_sim == ext_rate)
    
    #isolate for a given proportion of mainland 
    nonoceanic_time_lac_mu_mainland <- dplyr::filter(nonoceanic_time_lac_mu, prop_mainland == mainland)
    
    #isolate for a given proportion of nonendemics 
    nonoceanic_time_lac_mu_mainland_nonend <- dplyr::filter(nonoceanic_time_lac_mu_mainland, prop_non_endemic == nonendemic)
    
    #calculate the absolute difference between ML estimate and true value
    nonoceanic_abs_diff <- dplyr::mutate(nonoceanic_time_lac_mu_mainland_nonend, abs(lambda_c - lambda_c_sim))
    
    #remove any NA values
    nonoceanic_abs_diff <- tidyr::drop_na(nonoceanic_abs_diff)
    
    #calculate the average of the absolute differences
    nonoceanic_mean_lac <- mean(nonoceanic_abs_diff$lambda_c)
    
    #Calculate the difference betweeen the oceanic differences and the nonoceanic difference
    delta <- (oceanic_mean_lac - nonoceanic_mean_lac)
    
    #Calculate the relative error of the differences
    relative_delta <- delta/mean(DAISIEdata$lambda_c_sim)
  }
  
  if (ext_diff == TRUE) {
    #isolate for a given value of cladogenesis
    oceanic_time_lac <- dplyr::filter(oceanic_time, lambda_c_sim == clado_rate)  
      
    #isolate for a given value of extinction
    oceanic_time_lac_mu <- dplyr::filter(oceanic_time_lac, mu_sim == ext_rate) 
      
    #calculate absolute difference between ML estimate and true value
    oceanic_abs_diff <- dplyr::mutate(oceanic_time_lac_mu, abs(mu - mu_sim))
      
    #remove any NA values
    oceanic_abs_diff <- tidyr::drop_na(oceanic_abs_diff)
      
    #deal with any outliers
    #anomalize::anomalize(oceanic_abs_diff, oceanic_abs_diff$mu)
      
    #calculate the average of the absolute differences
    oceanic_mean_mu <- mean(oceanic_abs_diff$mu)
    
    #isolate for a given value of cladogenesis
    nonoceanic_time_lac <- dplyr::filter(nonoceanic_time, lambda_c_sim == clado_rate)
    
    #isolate for a given value of extinction
    nonoceanic_time_lac_mu <- dplyr::filter(nonoceanic_time_lac, mu_sim == ext_rate)
    
    #calculate the absolute difference between ML estimate and true value
    nonoceanic_abs_diff <- dplyr::mutate(nonoceanic_time_lac_mu, abs(mu - mu_sim))
    
    #remove any NA values
    nonoceanic_abs_diff <- tidyr::drop_na(nonoceanic_abs_diff)
    
    #calculate the average of the absolute differences
    nonoceanic_mean_mu <- mean(nonoceanic_abs_diff$mu)
    
    #Calculate the difference betweeen the oceanic differences and the nonoceanic difference
    diff <- (oceanic_mean_mu - nonoceanic_mean_mu)
    
    #Calculate the relative error of the differences
    relative_delta <- diff/mean(DAISIEdata$mu_sim)
    
  }
  return(relative_delta)
}


