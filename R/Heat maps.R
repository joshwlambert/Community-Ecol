library(ggplot2)


time.labs <- c("time=4","time=10")
names(time.labs) <- c("4","10")


immig_heatmap<- ggplot(DifTable, aes(x = prop_mainland, y = prop_non_endemic, fill = immig_diff)) + geom_tile() + facet_grid(. ~ time, labeller = labeller(time=time.labs))+ ggtitle("Relative difference in immigration value for nonoceanic and oceanic") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(0.5, 0.7, 0.9)) + scale_x_continuous(breaks = c(0.1, 0.25, 0.4))+ labs(x = "Proportion of the mainland", y = "Proportion species nonendemic", fill = "Δ Immigration")
immig_heatmap
ana_heatmap<- ggplot(DifTable, aes(x = prop_mainland, y = prop_non_endemic, fill = ana_diff)) + geom_tile() + facet_grid(. ~ time, labeller = labeller(time=time.labs)) + ggtitle("Relative difference in anagenesis value for nonoceanic and oceanic")+ theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = c(0.5, 0.7, 0.9)) + scale_x_continuous(breaks = c(0.1, 0.25, 0.4)) + labs(x = "Proportion of the mainland", y = "Proportion species nonendemic", fill = "Δ Anagenesis")
ana_heatmap
clado_heatmap<- ggplot(DifTable, aes(x = prop_mainland, y = prop_non_endemic, fill = clado_diff)) + geom_tile() + facet_grid(. ~ time, labeller = labeller(time=time.labs)) + ggtitle("Relative difference in cladogenesis value for nonoceanic and oceanic")+ theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = c(0.5, 0.7, 0.9)) + scale_x_continuous(breaks = c(0.1, 0.25, 0.4)) + labs(x = "Proportion of the mainland", y = "Proportion species nonendemic", fill = "Δ Cladogenesis")
clado_heatmap
ext_heatmap<- ggplot(DifTable, aes(x = prop_mainland, y = prop_non_endemic, fill = ext_diff)) + geom_tile() + facet_grid(. ~ time, labeller = labeller(time=time.labs)) + ggtitle("Relative difference in extinction value for nonoceanic and oceanic")+ theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = c(0.5, 0.7, 0.9)) + scale_x_continuous(breaks = c(0.1, 0.25, 0.4)) + labs(x = "Proportion of the mainland", y = "Proportion species nonendemic", fill = "Δ Extinction")
ext_heatmap