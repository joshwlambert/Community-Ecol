#load the required packages
library(ggplot2)
library(dplyr)
library(tidyr)

#Parameter differences for island_age = 4, mainland = 0.1, nonendemic = 0.9

Clado_hh = 0.205411
clado_lh =  0.6262914
clado_ll =  1.851296
clado_hl = 0.9706154

ext_hh = 0.002954286
ext_lh = 0.008827768
ext_ll = 0.04387181
ext_hl = 0.1591237

immig_hh = 9.986142
immig_lh = 3.455252
immig_ll = 13.12838
immig_hl = 30.25532

ana_hh = 0.8227131
ana_lh = 1.443557
ana_ll = 1.691687
ana_hl = 1.443557

clado = c(Clado_hh, clado_lh, clado_ll, clado_hl)
ext = c(ext_hh, ext_lh, ext_ll, ext_hl)
immig = c(immig_hh, immig_lh, immig_ll, immig_hl)
ana = c(ana_hh, ana_lh, ana_ll, ana_hl)
data <- data.frame(clado, ext, immig, ana) %>%
  gather(data)
  names <- c('HH', 'LH', 'LL', 'HL')
  data <- cbind(data, names)

#Bar plot 
p <- ggplot(data = data, aes (x = data$data, y = data$value)) +
  geom_col(aes(fill = names), position = 'fill', colour = 'black', fill = 'white') +
  geom_text(aes(label = data$names), position = position_fill(vjust = 0.5),  size = 3) +
  xlab("DAISIE Parameter") +
  ylab("Weight of model difference")
plot(p)


head(DAISIETable)
data <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::mutate(delta = lambda_c - lambda_c_sim) %>%
  tidyr::drop_na()

#Heatmaps
p <- ggplot(data = data, aes(x = prop_non_endemic, y = prop_mainland, fill = log(lambda_c))) + 
  geom_tile() +
  facet_grid(lambda_c_sim ~ time)
plot(p)  

p <- ggplot(data = data, aes(x = prop_non_endemic, y = prop_mainland, fill = (mu))) + 
  geom_tile() +
  facet_grid(mu_sim ~ time)
plot(p)  
  
p <- ggplot(data = data, aes(x = prop_non_endemic, y = prop_mainland, fill = (gamma))) +
  geom_tile() +
  facet_wrap( ~ time)
plot(p)

p <- ggplot(data = data, aes(x = prop_non_endemic, y = prop_mainland, fill = log(lambda_a))) +
  geom_tile() +
  facet_wrap( ~ time)
plot(p)
