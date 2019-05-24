rm(list=ls())
library(ggplot2)


#add a column to the dataframe for the right sequence of facets
final_data$island_f <- factor(final_data$island, levels = c("O", "LL", "LM", "LH","ML", "MM", "MH", "HL", "HM", "HH"))


#violin plot for lambda_c 
clado_plot <- ggplot(data = final_data) +
  geom_violin(mapping = aes(x = factor(time), y = lambda_c, fill = factor(time))) +
  geom_hline(yintercept = 2.5) +
  scale_y_log10() +
  facet_wrap( ~ island_f, nrow = 1) +
  labs(x= "Island Age (myr)", y=bquote("Cladogenesis rate"~(myr^-1)), fill="Island Age (myr)") +
  ggtitle("Maximum likelihood cladogenesis value distribution") + theme(plot.title= element_text(hjust=0.5))

#dataframe containing the asterixes for the significance                                       
clado_ast <- data.frame(label= rep("*", 1), island_f=c("LM"), x = rep(1,1), y= rep(100000,1))

#add the asterixes to the plot
clado_plot + geom_text(data=clado_ast, mapping = aes(x=x, y=y, label= label))

#violin plot for mu
ext_plot <- ggplot(data = final_data) +
  geom_violin(mapping = aes(x = factor(time), y = mu, fill = factor(time))) +
  geom_hline(yintercept = 2.5) +
  ylim(0,8) +
  facet_wrap(~ island_f, nrow = 1) +
  labs(x= "Island Age (myr)", y=bquote("Extinction rate"~(myr^-1)), fill="Island Age (myr)") +
  ggtitle("Maximum likelihood extinction value distribution") + theme(plot.title= element_text(hjust=0.5)) +
  guides(fill= FALSE)
  
#dataframe containing the asterixes for the significance                                       
ext_ast <- data.frame(label= rep("*", 11), island_f=c("LL", "LM", "LH", "ML", "MM", "MH", "HL", "HM", "HH", "LL", "LH"), x = c(rep(1,9),2,2) , y= rep(8,11))

#add the asterixes to the plot
ext_plot + geom_text(data=ext_ast, mapping = aes(x=x, y=y, label= label))

#violin plot for gamma
immig_plot <- ggplot(data = final_data) +
  geom_violin(mapping = aes(x = factor(time), y = gamma, fill = factor(time))) +
  geom_hline(yintercept = 0.01) +
  scale_y_log10() +
<<<<<<< HEAD
  facet_wrap(~ island_f, nrow = 1) +
  labs(x= "Island Age (myr)", y=bquote("Immigration rate"~(myr^-1)), fill="Island Age (myr)")+
  ggtitle("Maximum likelihood immigration value distribution") + theme(plot.title= element_text(hjust=0.5)) +
  guides(fill= FALSE)

#dataframe containing the asterixes for the significance                                       
immig_ast <- data.frame(label= rep("*", 6), island_f=c("LM", "MM", "MH", "HL", "HM", "HH"), x = c(rep(1,6)) , y= rep(0.1,6))

#add the asterixes to the plot
immig_plot + geom_text(data=immig_ast, mapping = aes(x=x, y=y, label= label))

#violin plot for lambda a
ana_plot <- ggplot(data = final_data) +
  geom_violin(mapping = aes(x = factor(time), y = lambda_a, fill = factor(time))) +
  geom_hline(yintercept = 1) +
  scale_y_log10() +
  facet_wrap(~ island_f, nrow = 1) +
  labs(x= "Island Age (myr)", y=bquote("Anagenesis rate"~(myr^-1)), fill="Island Age (myr)") +
  ggtitle("Maximum likelihood anagenesis value distribution") + theme(plot.title= element_text(hjust=0.5)) +
  guides(fill= FALSE)

#dataframe containing the asterixes for the significance                                       
ana_ast <- data.frame(label="*", island_f="HL", x =1 , y= 100000000)

#add the asterixes to the plot
plot(ana_plot) + geom_text(data=ana_ast, aes(x=x, y=y, label=label))
=======
  facet_wrap( ~ island, nrow = 1) +
  ylab('Extinction rate') +
  xlab('Island Age') 
plot(ext_plot)
>>>>>>> 97fbec834a9df7ef81b713a22e8086b230baf19a

#violin plot for gamma
gamma_plot <- ggplot(data = data) +
  geom_violin(mapping = aes(x = factor(time), y = gamma, fill = factor(time))) +
  geom_hline(yintercept = 0.01) +
  scale_y_log10() +
  facet_wrap( ~ island, nrow = 1) +
  ylab('Immigration rate') +
  xlab('Island Age') 
plot(gamma_plot)

#violin plot for lambda_a
lambda_a_plot <- ggplot(data = data) +
  geom_violin(mapping = aes(x = factor(time), y = lambda_a, fill = factor(time))) +
  geom_hline(yintercept = 0.01) +
  scale_y_log10() +
  facet_wrap( ~ island, nrow = 1) +
  ylab('Anagenesis rate') +
  xlab('Island Age') 
plot(gamma_plot)


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


#Heatmaps deltas nonoceanic-oceanic in a prop_mainland prop_non_endemic grid faceted for time

#Heat map delta cladogenesis both times
clado_heatmap<- ggplot(DifTable, aes(x=prop_mainland,y=prop_non_endemic, fill=clado_diff)) + geom_tile() + facet_grid(. ~ time)
clado_heatmap

#Heat map delta extinction

ext_heatmap <- ggplot(DifTable, aes(x=prop_mainland,y=prop_non_endemic, fill=ext_diff)) + geom_tile() + facet_grid(. ~ time)
ext_heatmap


#Heat map delta immigration

immig_heatmap <- ggplot(DifTable, aes(x=prop_mainland,y=prop_non_endemic, fill=immig_diff)) + geom_tile() + facet_grid(. ~ time)
immig_heatmap

#Heat map delta anagenesis

ana_heatmap <- ggplot(DifTable, aes(x=prop_mainland,y=prop_non_endemic, fill=ana_diff)) + geom_tile() + facet_grid(. ~ time)
ana_heatmap
