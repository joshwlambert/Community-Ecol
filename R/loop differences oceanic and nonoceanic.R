#creates a new column to a dataframe with the difference for the rows'parameter values.
#column name and truth values can be changed to add other differences
for(i in 1:18) {
  
  time = DifTable$time[i]
  lambda_c_sim = DifTable$lambda_c_sim[i]
  mu_sim = DifTable$mu_sim[i]
  prop_mainland = DifTable$prop_mainland[i]
  prop_non_endemic = DifTable$prop_non_endemic[i]
  
  
  
  DifTable$clado_diff[i] <- model_param_delta(data=DAISIETable, island_age=time, clado_rate= lambda_c_sim , ext_rate = mu_sim, immig_rate = 0.01, ana_rate = 1, mainland = prop_mainland, nonendemic = prop_non_endemic , clado_diff = TRUE, ext_diff = FALSE, immig_diff = FALSE, ana_diff = FALSE)
}

save(DifTable, file="Table_delta_nonoceanic_oceanic.RDATA")
