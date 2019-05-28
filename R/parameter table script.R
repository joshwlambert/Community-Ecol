rm(list=ls())
library(dplyr)

setwd("~/studie biologie/jaar 2/Community Research")
load("~/studie biologie/jaar 2/Community Research/Full_dataframe.RDATA")

#empty dataframe with all parameters
DAISIEParaTable <- data.frame(time=numeric(),lambda_c_sim=numeric(),mu_sim=numeric(), K_sim=numeric(), gamma_sim=numeric(), lambda_a_sim=numeric(),prop_mainland=numeric(),prop_non_endemic=numeric(), island_type=character(), island=character())


#creates a dataframe containing all parameter value combinations used
for(i in 1:20) {
  rm(nonoceanic)
  nam2 <- paste("simulation_parameters/backup/Pars_", i, ".RDATA", sep = "")
  load(nam2)
  time <- time
  lambda_c_sim <- pars[1]
  mu_sim <- pars[2]
  K_sim <- pars[3]
  gamma_sim <- pars[4]
  lambda_a_sim <- pars[5]
  if(exists("nonoceanic")) prop_mainland <-nonoceanic[1] else prop_mainland <- 0
  if(exists("nonoceanic")) prop_non_endemic <- nonoceanic[2] else prop_non_endemic <- 0
  if(exists("nonoceanic")) island_type <- "nonoceanic" else island_type <- "oceanic"

  DAISIEParaSUM <- data.frame(time,lambda_c_sim,mu_sim, K_sim, gamma_sim, lambda_a_sim,prop_mainland,prop_non_endemic, island_type)
  
  DAISIEParaTable <- rbind(DAISIEParaTable,DAISIEParaSUM)
}

#filters the oceanic parameter combinations from the dataframe
DifTable <- dplyr::filter(DAISIEParaTable, island_type=="nonoceanic")

