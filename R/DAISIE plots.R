#violin plot for lambda_c 
clado_plot <- ggplot(data = data) +
  geom_violin(mapping = aes(x = factor(time), y = lambda_c, fill = factor(time))) +
  geom_hline(yintercept = 2.5) +
  scale_y_log10() +
  facet_wrap( ~ island, nrow = 1) +
  ylab('Cladogenesis rate') +
  xlab('Island Age') 
plot(clado_plot)

#violin plot for mu
ext_plot <- ggplot(data = data) +
  geom_violin(mapping = aes(x = factor(time), y = mu, fill = factor(time))) +
  geom_hline(yintercept = 2.5) +
  scale_y_log10() +
  facet_wrap( ~ island, nrow = 1) +
  ylab('Extinction rate') +
  xlab('Island Age') 
plot(ext_plot)

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
