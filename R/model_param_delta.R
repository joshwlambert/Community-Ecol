#Function that calculates the differences between oceanic and nonoceanic parameter
#estimates from each model parameter across the nonoceanic parameter space
#The function requires a dataframe with all ML estiamtes and simulating values,
#currently

library(tidyverse)

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
    relative_delta <- clado_param_delta(data, island_age, clado_rate, ext_rate, immig_rate, 
                                        ana_rate, mainland, nonendemic, clado_diff = TRUE, 
                                        ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE,
                                        oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time)
  }
  
  if (ext_diff == TRUE) {
    relative_delta <- ext_param_delta(data, island_age, clado_rate, ext_rate, immig_rate, 
                                      ana_rate, mainland, nonendemic, clado_diff = TRUE, 
                                      ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE,
                                      oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time)
  }
  
  if (immig_diff == TRUE) {
    relative_delta <- immig_param_delta(data, island_age, clado_rate, ext_rate, immig_rate, 
                                        ana_rate, mainland, nonendemic, clado_diff = TRUE, 
                                        ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE,
                                        oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time)
  }
  
  if (ana_diff == TRUE) {
    relative_delta <- ana_param_delta(data, island_age, clado_rate, ext_rate, immig_rate, 
                                      ana_rate, mainland, nonendemic, clado_diff = TRUE, 
                                      ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE,
                                      oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time)
  }
  return(relative_delta)
}

clado_param_delta <- function(data, island_age, clado_rate, ext_rate, immig_rate, 
                              mainland, nonendemic, clado_diff = TRUE, 
                              ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE,
                              oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time) {
  
  #isolate for a given value of cladogenesis

  oceanic_abs_diff <- oceanic_time %>%
    dplyr::filter(lambda_c_sim == clado_rate) %>%
    dplyr::filter(mu_sim == ext_rate) %>%
    dplyr::mutate(oceanic_delta_lac = abs(log(lambda_c) - log(lambda_c_sim))) %>%
    tidyr::drop_na()
  oceanic_mean_lac <- mean(oceanic_abs_diff$oceanic_delta_lac)
  
  nonoceanic_abs_diff <- nonoceanic_time %>%
    dplyr::filter(lambda_c_sim == clado_rate) %>%
    dplyr::filter(mu_sim == ext_rate) %>%
    dplyr::filter(prop_mainland == mainland) %>%
    dplyr::filter(prop_non_endemic == nonendemic) %>%
    dplyr::mutate(nonoceanic_delta_lac = abs(log(lambda_c) - log(lambda_c_sim))) %>%
    tidyr::drop_na()
  nonoceanic_mean_lac <- mean(nonoceanic_abs_diff$nonoceanic_delta_lac)
  
  delta <- (oceanic_mean_lac - nonoceanic_mean_lac)
  
  relative_delta <- delta/clado_rate
  
  return(relative_delta)
}

ext_param_delta <- function(data, island_age, clado_rate, ext_rate, immig_rate, 
                            ana_rate, mainland, nonendemic, clado_diff = TRUE, 
                            ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE,
                            oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time) {
  
  oceanic_abs_diff <- oceanic_time %>%
    dplyr::filter(lambda_c_sim == clado_rate) %>%
    dplyr::filter(mu_sim == ext_rate) %>%
    dplyr::mutate(oceanic_delta_mu = abs(log(mu) - log(mu_sim))) %>%
    tidyr::drop_na()

  oceanic_mean_mu <- mean(oceanic_abs_diff$oceanic_delta_mu)
  
  nonoceanic_abs_diff <- nonoceanic_time %>%
    dplyr::filter(lambda_c_sim == clado_rate) %>%
    dplyr::filter(mu_sim == ext_rate) %>%
    dplyr::filter(prop_mainland == mainland) %>%
    dplyr::filter(prop_non_endemic == nonendemic) %>%
    dplyr::mutate(nonoceanic_delta_mu = abs(log(mu) - log(mu_sim))) %>%
    tidyr::drop_na()
  nonoceanic_mean_mu <- mean(nonoceanic_abs_diff$nonoceanic_delta_mu)

  delta <- (oceanic_mean_mu - nonoceanic_mean_mu)
  
  relative_delta <- delta/ext_rate
  
  return(relative_delta)
}

immig_param_delta <- function(data, island_age, clado_rate, ext_rate, immig_rate, 
                              ana_rate, mainland, nonendemic, clado_diff = TRUE, 
                              ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE,
                              oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time) {
  
  oceanic_abs_diff <- oceanic_time %>%
    dplyr::filter(lambda_c_sim == clado_rate) %>%  
    dplyr::filter(mu_sim == ext_rate) %>%
    dplyr::mutate(oceanic_delta_gam = abs(log(gamma) - log(gamma_sim))) %>%
    tidyr::drop_na()
  oceanic_mean_gamma <- mean(oceanic_abs_diff$oceanic_delta_gam)
  
  nonoceanic_abs_diff <- nonoceanic_time %>%
    dplyr::filter(lambda_c_sim == clado_rate) %>%
    dplyr::filter(mu_sim == ext_rate) %>%
    dplyr::filter(prop_mainland == mainland) %>%
    dplyr::filter(prop_non_endemic == nonendemic) %>%
    dplyr::mutate(nonoceanic_delta_gam = abs(log(gamma) - log(gamma_sim))) %>%
    tidyr::drop_na() 
  nonoceanic_mean_gamma <- mean(nonoceanic_abs_diff$nonoceanic_delta_gam)

  delta <- (oceanic_mean_gamma - nonoceanic_mean_gamma)

  relative_delta <- delta/immig_rate
  
  return(relative_delta)
}

ana_param_delta <- function(data, island_age, clado_rate, ext_rate, immig_rate, 
                            ana_rate, mainland, nonendemic, clado_diff = TRUE, 
                            ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE,
                            oceanic_time = oceanic_time, nonoceanic_time = nonoceanic_time) {
  
  oceanic_abs_diff <- oceanic_time %>%
    dplyr::filter(lambda_c_sim == clado_rate) %>%  
    dplyr::filter(mu_sim == ext_rate) %>%
    dplyr::mutate(nonoceanic_delta_laa = abs(log(lambda_a) - log(lambda_a_sim))) %>%
    tidyr::drop_na()
  oceanic_mean_laa <- mean(oceanic_abs_diff$nonoceanic_delta_laa)
  
  nonoceanic_abs_diff <- nonoceanic_time %>%
    dplyr::filter(lambda_c_sim == clado_rate) %>%
    dplyr::filter(mu_sim == ext_rate) %>%
    dplyr::filter(prop_mainland == mainland) %>%
    dplyr::filter(prop_non_endemic == nonendemic) %>%
    dplyr::mutate(nonoceanic_delta_laa = abs(log(lambda_a) - log(lambda_a_sim))) %>%
    tidyr::drop_na()
  nonoceanic_mean_laa <- mean(nonoceanic_abs_diff$nonoceanic_delta_laa)
  
  delta <- (oceanic_mean_laa - nonoceanic_mean_laa)
  
  relative_delta <- delta/ana_rate
}