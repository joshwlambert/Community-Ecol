#violin plot for lambda_c 
clado_plot <- ggplot(data = data) +
  geom_violin(mapping = aes(x = factor(time), y = lambda_c, fill = factor(time))) +
  geom_hline(yintercept = 2.5) +
  scale_y_log10() +
  facet_wrap(prop_non_endemic ~ prop_mainland, nrow = 1) +
  ylab('Cladogenesis rate') +
  xlab('Island Age') 
plot(clado_plot)

#violin plot for mu
ext_plot <- ggplot(data = data) +
  geom_violin(mapping = aes(x = factor(time), y = mu, fill = factor(time))) +
  geom_hline(yintercept = 2.5) +
  scale_y_log10() +
  ylim(0, 8) +
  facet_wrap(prop_non_endemic ~ prop_mainland, nrow = 1) +
  ylab('Extinction rate') +
  xlab('Island Age') 
plot(ext_plot)


data <- dplyr::filter(data, time == 4)
#violin plot for lambda_c at time = 4
plot1 <- ggplot(data = data) +
  geom_violin(mapping = aes(x = factor(time), y = lambda_c)) +
  ylim(0,20) +
  geom_hline(yintercept = 2.5) +
  scale_y_log10() +
  facet_wrap(prop_non_endemic ~ prop_mainland, nrow = 1) +
  ylab('Cladogenesis rate') +
  xlab('Island Age = 4')
plot(plot1)

data <- dplyr::filter(data, time == 10)
#violin plot for lambda_c at time = 10
p <- ggplot(data = data) +
  geom_violin(mapping = aes(x = factor(time), y = lambda_c)) +
  ylim(0,20) +
  geom_hline(yintercept = 2.5) +
  scale_y_log10() +
  facet_wrap(prop_non_endemic ~ prop_mainland, nrow = 1) +
  ylab('Cladogenesis rate') +
  xlab('Island Age = 10')
plot(p)
  


#Parameter differences

Clado_one_nine = 0.205411
clado_one_seven =  0.6262914
clado_one_five =  1.851296
clado_two_nine = 0.9706154
clado_two_seven  = 0.205411
clado_two_five = 0.6262914
clado_four_nine = 1.851296
clado_four_seven = 0.9706154
clado_four_five = 0.9706154

ext_one_nine = 0.002954286 
ext_one_seven = 0.008827768  
ext_one_five = 0.04387181  
ext_two_nine = 0.1591237 
ext_two_seven  = 0.002954286 
ext_two_five = 0.008827768  
ext_four_nine = 0.04387181   
ext_four_seven = 0.1591237 
ext_four_five = 0.1591237 
  
immig_one_nine = 9.986142
immig_one_seven = 3.455252   
immig_one_five = 13.12838   
immig_two_nine = 30.25532
immig_two_seven  = 9.986142
immig_two_five = 3.455252 
immig_four_nine = 13.12838  
immig_four_seven = 30.25532
immig_four_five = 30.25532

ana_one_nine = 0.8227131
ana_one_seven = 1.443557    
ana_one_five = 1.691687    
ana_two_nine = 1.443557 
ana_two_seven  = 0.8227131
ana_two_five = 1.443557
ana_four_nine = 1.691687 
ana_four_seven = 1.443557 
ana_four_five = 1.443557 

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


#Bar plot 
p <- ggplot(data = data, aes (x = data$data, y = data$value)) +
  geom_col(aes(fill = names), position = 'fill', colour = 'black', fill = 'white') +
  geom_text(aes(label = data$names), position = position_fill(vjust = 0.5),  size = 3) +
  xlab("DAISIE Parameter") +
  ylab("Weight of model difference")
plot(p)
