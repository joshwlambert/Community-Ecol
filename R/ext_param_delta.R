ext_param_delta <- function(data, island_age, clado_rate, ext_rate, immig_rate = NULL, 
                            ana_rate = NULL, mainland, nonendemic, clado_diff = TRUE, 
                            ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE)
#isolate for a given value of cladogenesis
oceanic_time_lac <- dplyr::filter(oceanic_time, lambda_c_sim == clado_rate)  

#isolate for a given value of extinction
oceanic_time_lac_mu <- dplyr::filter(oceanic_time_lac, mu_sim == ext_rate) 


#log transform lambda_c
oceanic_time_lac_mu <- dplyr::mutate(oceanic_time_lac_mu, log(mu))

#log transform lambda_c_sim
oceanic_time_lac_mu <- dplyr::mutate(oceanic_time_lac_mu, log(mu_sim))

#calculate absolute difference between ML estimate and true value
oceanic_abs_diff <- dplyr::mutate(oceanic_time_lac_mu, abs(log(mu) - log(mu_sim)))

#remove any NA values
oceanic_abs_diff <- tidyr::drop_na(oceanic_abs_diff)

#deal with any outliers
#anomalize::anomalize(oceanic_abs_diff, oceanic_abs_diff$mu)

#calculate the average of the absolute differences
oceanic_mean_mu <- mean(oceanic_abs_diff$`abs(log(mu) - log(mu_sim))`)

#isolate for a given value of cladogenesis
nonoceanic_time_lac <- dplyr::filter(nonoceanic_time, lambda_c_sim == clado_rate)

#isolate for a given value of extinction
nonoceanic_time_lac_mu <- dplyr::filter(nonoceanic_time_lac, mu_sim == ext_rate)

#isolate for a given proportion of mainland 
nonoceanic_time_lac_mu_mainland <- dplyr::filter(nonoceanic_time_lac_mu, prop_mainland == mainland)

#isolate for a given proportion of nonendemics 
nonoceanic_time_lac_mu_mainland_nonend <- dplyr::filter(nonoceanic_time_lac_mu_mainland, prop_non_endemic == nonendemic)

#log transform lambda_c
nonoceanic_time_lac_mu_mainland_nonend <- dplyr::mutate(nonoceanic_time_lac_mu, log(mu))

#log transform lambda_c_sim
nonoceanic_time_lac_mu_mainland_nonend <- dplyr::mutate(nonoceanic_time_lac_mu, log(mu_sim))

#calculate the absolute difference between ML estimate and true value
nonoceanic_abs_diff <- dplyr::mutate(nonoceanic_time_lac_mu_mainland_nonend, abs(log(mu) - log(mu_sim)))

#remove any NA values
nonoceanic_abs_diff <- tidyr::drop_na(nonoceanic_abs_diff)

#calculate the average of the absolute differences
nonoceanic_mean_mu <- mean(nonoceanic_abs_diff$`abs(log(mu) - log(mu_sim))`)

#Calculate the difference betweeen the oceanic differences and the nonoceanic difference
delta <- (oceanic_mean_mu - nonoceanic_mean_mu)

#Calculate the relative error of the differences
relative_delta <- delta/mean(DAISIEdata$mu_sim)

return(relative_delta)