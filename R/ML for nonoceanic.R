rm(list=ls())

devtools::load_all(".")
library(ggplot2)

setwd("~/studie biologie/jaar 2/Community Research")

#determine the parameter values for the nonoceanic island simulation
parsHH = c(2.5,2.5,20,0.01,1) #cladogenis rate, extinction rate, carrying capacity, immigration, anagenesis
parsLH = c(1.5,2.5,20,0.01,1)
parsHL = c(2.5,1.5,20,0.01,1)
parsLL = c(1.5,1.5,20,0.01,1)
time=4 #island age in million of years (possible later )
M=1000  #mainland species pool



#create empty dataframe DASIEMLSUM
lambda_c <- factor()
mu <- factor()
K <- factor()
gamma <- factor()
lambda_a <- factor()
loglik <- factor()
df <- factor()
conv <- factor()
DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a,loglik,df,conv)

#############################first run##########################################################
################################################################################################
#restart DAISIEMLSUM and ML_cur
DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a,loglik,df,conv)

#set nonoceanic
nonoceanic <- c(0.4,0.5)
time=4
ML_cur <- 0
pars <- parsHH

#run the simulation
island_replicates=DAISIE_sim_nonoceanic(time=time,M=M,pars=pars,replicates=100, nonoceanic = nonoceanic, divdepmodel = "CS")

#save the simulation parameter values
save(pars,M, time, nonoceanic, file="simulation_parameters/Pars_HH_nonoceanic_time=4_no=c(0.4,0.5).Rdata")

for (i in (ML_cur + 1):length(island_replicates)){
    ML_cur <- i
    DAISIEML <- DAISIE_ML_CS(datalist=island_replicates[[i]], initparsopt = parsHH, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
    DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
    print(i)
}
  save(DAISIEMLSUM,file="Maximum_Likelihood/ML_HH_nonoceanic_time=4_no=c(0.4,0.5).Rdata")

  #############################second run##########################################################
  ################################################################################################
  #restart DAISIEMLSUM and ML_cur
  DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a,loglik,df,conv)
  
  #set nonoceanic
  nonoceanic <- c(0.4,0.9)
  time=4
  ML_cur <- 0
  pars <- parsHH
  
  #run the simulation
  island_replicates=DAISIE_sim_nonoceanic(time=time,M=M,pars=pars,replicates=100, nonoceanic = nonoceanic, divdepmodel = "CS")
  
  #save the simulation parameter values
  save(pars,M, time, nonoceanic, file="simulation_parameters/Pars_HH_nonoceanic_time=4_no=c(0.4,0.9).Rdata")
  
  for (i in (ML_cur + 1):length(island_replicates)){
    ML_cur <- i
    DAISIEML <- DAISIE_ML_CS(datalist=island_replicates[[i]], initparsopt = parsHH, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
    DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
    print(i)
  }
  save(DAISIEMLSUM,file="Maximum_Likelihood/ML_HH_nonoceanic_time=4_no=c(0.4,0.9).Rdata")
  
  #############################third run##########################################################
  ################################################################################################
  #restart DAISIEMLSUM and ML_cur
  DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a,loglik,df,conv)
  
  #set nonoceanic
  nonoceanic <- c(0.4,0.5)
  time=4
  ML_cur <- 0
  pars <- parsHL
  
  #run the simulation
  island_replicates=DAISIE_sim_nonoceanic(time=time,M=M,pars=pars,replicates=100, nonoceanic = nonoceanic, divdepmodel = "CS")
  
  #save the simulation parameter values
  save(pars,M, time, nonoceanic, file="simulation_parameters/Pars_HL_nonoceanic_time=4_no=c(0.4,0.5).Rdata")
  
  for (i in (ML_cur + 1):length(island_replicates)){
    ML_cur <- i
    DAISIEML <- DAISIE_ML_CS(datalist=island_replicates[[i]], initparsopt = parsHH, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
    DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
    print(i)
  }
  save(DAISIEMLSUM,file="Maximum_Likelihood/ML_HL_nonoceanic_time=4_no=c(0.4,0.5).Rdata")
  
  
  #############################4th run##########################################################
  ################################################################################################
  #restart DAISIEMLSUM and ML_cur
  DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a,loglik,df,conv)
  
  #set nonoceanic
  nonoceanic <- c(0.4,0.9)
  time=4
  ML_cur <- 0
  pars <- parsHL
  
  #run the simulation
  island_replicates=DAISIE_sim_nonoceanic(time=time,M=M,pars=pars,replicates=100, nonoceanic = nonoceanic, divdepmodel = "CS")
  
  #save the simulation parameter values
  save(pars,M, time, nonoceanic, file="simulation_parameters/Pars_HL_nonoceanic_time=4_no=c(0.4,0.9).Rdata")
  
  for (i in (ML_cur + 1):length(island_replicates)){
    ML_cur <- i
    DAISIEML <- DAISIE_ML_CS(datalist=island_replicates[[i]], initparsopt = parsHH, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
    DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
    print(i)
  }
  save(DAISIEMLSUM,file="Maximum_Likelihood/ML_HL_nonoceanic_time=4_no=c(0.4,0.9).Rdata")
  
  #############################5th run##########################################################
  ################################################################################################
  #restart DAISIEMLSUM and ML_cur
  DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a,loglik,df,conv)
  
  #set nonoceanic
  nonoceanic <- c(0.4,0.5)
  time=4
  ML_cur <- 0
  pars <- parsLH
  
  #run the simulation
  island_replicates=DAISIE_sim_nonoceanic(time=time,M=M,pars=pars,replicates=100, nonoceanic = nonoceanic, divdepmodel = "CS")
  
  #save the simulation parameter values
  save(pars,M, time, nonoceanic, file="simulation_parameters/Pars_LH_nonoceanic_time=4_no=c(0.4,0.5).Rdata")
  
  for (i in (ML_cur + 1):length(island_replicates)){
    ML_cur <- i
    DAISIEML <- DAISIE_ML_CS(datalist=island_replicates[[i]], initparsopt = parsHH, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
    DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
    print(i)
  }
  save(DAISIEMLSUM,file="Maximum_Likelihood/ML_LH_nonoceanic_time=4_no=c(0.4,0.5).Rdata")
  
  #############################6th run##########################################################
  ################################################################################################
  #restart DAISIEMLSUM and ML_cur
  DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a,loglik,df,conv)
  
  #set nonoceanic
  nonoceanic <- c(0.4,0.9)
  time=4
  ML_cur <- 0
  pars <- parsLH
  
  #run the simulation
  island_replicates=DAISIE_sim_nonoceanic(time=time,M=M,pars=pars,replicates=100, nonoceanic = nonoceanic, divdepmodel = "CS")
  
  #save the simulation parameter values
  save(pars,M, time, nonoceanic, file="simulation_parameters/Pars_HL_nonoceanic_time=4_no=c(0.4,0.9).Rdata")
  
  for (i in (ML_cur + 1):length(island_replicates)){
    ML_cur <- i
    DAISIEML <- DAISIE_ML_CS(datalist=island_replicates[[i]], initparsopt = parsHH, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
    DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
    print(i)
  }
  save(DAISIEMLSUM,file="Maximum_Likelihood/ML_HL_nonoceanic_time=4_no=c(0.4,0.9).Rdata")
  
  #############################7th run##########################################################
  ################################################################################################
  #restart DAISIEMLSUM and ML_cur
  DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a,loglik,df,conv)
  
  #set nonoceanic
  nonoceanic <- c(0.4,0.5)
  time=4
  ML_cur <- 0
  pars <- parsLL
  
  #run the simulation
  island_replicates=DAISIE_sim_nonoceanic(time=time,M=M,pars=pars,replicates=100, nonoceanic = nonoceanic, divdepmodel = "CS")
  
  #save the simulation parameter values
  save(pars,M, time, nonoceanic, file="simulation_parameters/Pars_LL_nonoceanic_time=4_no=c(0.4,0.5).Rdata")
  
  for (i in (ML_cur + 1):length(island_replicates)){
    ML_cur <- i
    DAISIEML <- DAISIE_ML_CS(datalist=island_replicates[[i]], initparsopt = parsHH, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
    DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
    print(i)
  }
  save(DAISIEMLSUM,file="Maximum_Likelihood/ML_LL_nonoceanic_time=4_no=c(0.4,0.5).Rdata")
  
  #############################8th run##########################################################
  ################################################################################################
  #restart DAISIEMLSUM and ML_cur
  DAISIEMLSUM <- data.frame(lambda_c,mu,K,gamma,lambda_a,loglik,df,conv)
  
  #set nonoceanic
  nonoceanic <- c(0.4,0.9)
  time=4
  ML_cur <- 0
  pars <- parsLL
  
  #run the simulation
  island_replicates=DAISIE_sim_nonoceanic(time=time,M=M,pars=pars,replicates=100, nonoceanic = nonoceanic, divdepmodel = "CS")
  
  #save the simulation parameter values
  save(pars,M, time, nonoceanic, file="simulation_parameters/Pars_LL_nonoceanic_time=4_no=c(0.4,0.9).Rdata")
  
  for (i in (ML_cur + 1):length(island_replicates)){
    ML_cur <- i
    DAISIEML <- DAISIE_ML_CS(datalist=island_replicates[[i]], initparsopt = parsHH, ddmodel=11, idparsopt = 1:5, parsfix = NULL, idparsfix = NULL)
    DAISIEMLSUM <- rbind(DAISIEMLSUM, DAISIEML)
    print(i)
  }
  save(DAISIEMLSUM,file="Maximum_Likelihood/ML_LL_nonoceanic_time=4_no=c(0.4,0.9).Rdata")
  
  
  