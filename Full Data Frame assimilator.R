rm(list = ls())
   
setwd("~/studie biologie/jaar 2/Community Research")
   
DAISIEList <- list()

for(i in 1:29) {
  nam1 <- paste("Maximum_Likelihood/backup/ML_", i, ".RDATA", sep = "")
  nam2 <- paste("simulation_parameters/backup/pars_", i, ".RDATA", sep = "")
  load(nam1)
  load(nam2)
  DAISIEMLSUM <- DAISIEMLSUM[1:100,]
  time <- rep(time,100)
  lambda_c_sim <- rep(pars[1],100)
  mu_sim <- rep(pars[2],100)
  K_sim <- rep(pars[3],100)
  gamma_sim <- rep(pars[4],100)
  lambda_a_sim <- rep(pars[5],100)
  if(exists("nonoceanic")) prop_mainland <-rep(nonoceanic[1],100) else prop_mainland <- rep(NA,100)
  if(exists("nonoceanic")) prop_endemic <-rep(nonoceanic[2],100) else prop_endemic <- rep(NA,100)
  
  DAISIEMLSUM <- cbind(DAISIEMLSUM,time,lambda_c_sim,mu_sim,K_sim,gamma_sim,lambda_a_sim,prop_mainland,prop_endemic)
  
  
  DAISIEList[[i]] <- DAISIEMLSUM
}

DAISIETable <- DAISIEMLSUM[0,]

for(i in 1:29) {
  DAISIETable <- rbind(DAISIETable,DAISIEList[[i]])
}


