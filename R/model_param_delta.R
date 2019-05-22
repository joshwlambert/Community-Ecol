#Function that calculates the differences between oceanic and nonoceanic parameter
#estimates from each model parameter across the nonoceanic parameter space
#The function requires a dataframe with all ML estiamtes and simulating values,
#currently

library(tidyverse)

model_param_delta <- function(data, island_age, mainland, nonendemic, clado_rate, 
                              ext_rate, immig_rate, ana_rate, clado_diff = TRUE, 
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
    relative_delta <- clado_param_delta(oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time,
                                        clado_rate, mainland, nonendemic)
  }
  
  if (ext_diff == TRUE) {
    relative_delta <- ext_param_delta(oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time,
                                      ext_rate, mainland, nonendemic)
  }
  
  if (immig_diff == TRUE) {
    relative_delta <- immig_param_delta(oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time,data, 
                                        immig_rate, mainland, nonendemic)
  }
  
  if (ana_diff == TRUE) {
    relative_delta <- ana_param_delta(oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time,
                                      ana_rate, mainland, nonendemic)
  }
  return(relative_delta)
}

clado_param_delta <- function(oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time,
                              clado_rate, mainland, nonendemic) {
  
  #isolate for a given value of cladogenesis

  oceanic_abs_diff <- oceanic_time %>%
    dplyr::mutate(oceanic_delta_lac = abs(log(lambda_c) - log(lambda_c_sim))) %>% #calculate absolute difference between ML estimate and true value
    tidyr::drop_na() #remove any NA values
  #deal with any outliers
  oceanic_mean_lac <- mean(oceanic_abs_diff$oceanic_delta_lac)  #calculate the average of the absolute differences
  
  nonoceanic_abs_diff <- nonoceanic_time %>%
    dplyr::filter(prop_mainland == mainland) %>% #isolate for a given proportion of mainland 
    dplyr::filter(prop_non_endemic == nonendemic) %>% #isolate for a given proportion of nonendemics 
    dplyr::mutate(nonoceanic_delta_lac = abs(log(lambda_c) - log(lambda_c_sim))) %>% #calculate the absolute difference between ML estimate and true value
    tidyr::drop_na() #remove any NA values
  #deal with any outliers
  nonoceanic_mean_lac <- mean(nonoceanic_abs_diff$nonoceanic_delta_lac) #calculate the average of the absolute differences
  
  delta <- abs(nonoceanic_mean_lac - oceanic_mean_lac) #Calculate the difference betweeen the oceanic differences and the nonoceanic difference
  
  relative_delta <- delta/clado_rate #Calculate the relative error of the differences
  
  return(relative_delta)
}

ext_param_delta <- function(oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time,
                            ext_rate, mainland, nonendemic) {
  
  oceanic_abs_diff <- oceanic_time %>%
    dplyr::mutate(oceanic_delta_mu = abs(log(mu) - log(mu_sim))) %>% #calculate absolute difference between ML estimate and true value
    tidyr::drop_na() #remove any NA values
  #deal with any outliers
  oceanic_mean_mu <- mean(oceanic_abs_diff$oceanic_delta_mu) #calculate the average of the absolute differences
  
  nonoceanic_abs_diff <- nonoceanic_time %>%
    dplyr::filter(prop_mainland == mainland) %>% #isolate for a given proportion of mainland 
    dplyr::filter(prop_non_endemic == nonendemic) %>% #isolate for a given proportion of nonendemics 
    dplyr::mutate(nonoceanic_delta_mu = abs(log(mu) - log(mu_sim))) %>% #calculate the absolute difference between ML estimate and true value
    tidyr::drop_na() #remove any NA values
  #deal with any outliers
  nonoceanic_mean_mu <- mean(nonoceanic_abs_diff$nonoceanic_delta_mu) #calculate the average of the absolute differences

  delta <- abs(nonoceanic_mean_mu - oceanic_mean_mu) #Calculate the difference betweeen the oceanic differences and the nonoceanic difference
  
  relative_delta <- delta/ext_rate #Calculate the relative error of the differences
  
  return(relative_delta)
}

immig_param_delta <- function(oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time,data, 
                              immig_rate, mainland, nonendemic) {
  
  oceanic_abs_diff <- oceanic_time %>%
    dplyr::mutate(oceanic_delta_gam = abs(log(gamma) - log(gamma_sim))) %>% #calculate absolute difference between ML estimate and true value
    tidyr::drop_na() #remove any NA values
  #deal with any outliers
  oceanic_mean_gamma <- mean(oceanic_abs_diff$oceanic_delta_gam) #calculate the average of the absolute differences
  
  nonoceanic_abs_diff <- nonoceanic_time %>%
    dplyr::filter(prop_mainland == mainland) %>% #isolate for a given proportion of mainland 
    dplyr::filter(prop_non_endemic == nonendemic) %>% #isolate for a given proportion of nonendemics 
    dplyr::mutate(nonoceanic_delta_gam = abs(log(gamma) - log(gamma_sim))) %>% #calculate the absolute difference between ML estimate and true value
    tidyr::drop_na() #remove any NA values
  #deal with any outliers
  nonoceanic_mean_gamma <- mean(nonoceanic_abs_diff$nonoceanic_delta_gam) #calculate the average of the absolute differences

  delta <- abs(nonoceanic_mean_gamma - oceanic_mean_gamma) #Calculate the difference betweeen the oceanic differences and the nonoceanic difference

  relative_delta <- delta/immig_rate #Calculate the relative error of the differences
  
  return(relative_delta)
}

ana_param_delta <- function(oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time,
                            ana_rate, mainland, nonendemic) {
  
  oceanic_abs_diff <- oceanic_time %>%
    dplyr::mutate(nonoceanic_delta_laa = abs(log(lambda_a) - log(lambda_a_sim))) %>% #calculate absolute difference between ML estimate and true value
    tidyr::drop_na() #remove any NA values
  #deal with any outliers
  oceanic_mean_laa <- mean(oceanic_abs_diff$nonoceanic_delta_laa) #calculate the average of the absolute differences
  
  nonoceanic_abs_diff <- nonoceanic_time %>%
    dplyr::filter(prop_mainland == mainland) %>% #isolate for a given proportion of mainland 
    dplyr::filter(prop_non_endemic == nonendemic) %>% #isolate for a given proportion of nonendemics 
    dplyr::mutate(nonoceanic_delta_laa = abs(log(lambda_a) - log(lambda_a_sim))) %>% #calculate the absolute difference between ML estimate and true value
    tidyr::drop_na() #remove any NA values
  #deal with any outliers
  nonoceanic_mean_laa <- mean(nonoceanic_abs_diff$nonoceanic_delta_laa) #calculate the average of the absolute differences
  
  delta <- abs(nonoceanic_mean_laa - oceanic_mean_laa) #Calculate the difference betweeen the oceanic differences and the nonoceanic difference
  
  relative_delta <- delta/ana_rate #Calculate the relative error of the differences
  
  return(relative_delta)
}