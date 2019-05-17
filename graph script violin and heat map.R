library(ggplot2)


#creates a heat map with the proportion endemic plotted against the proportion of the mainland area split into facets along time and cladogenesis rate
ggplot(DAISIETable, aes(x=prop_endemic,y=prop_mainland,fill=log(lambda_c))) + geom_tile() +facet_grid(lambda_c_sim ~ time)


#creates a subset for time=4 and a cladogenesis rate of 2.5
D1 <- subset(DAISIETable, time==10)
D2 <- subset(D1, lambda_c_sim==2.5)

#plots the maximum likelihood cladogenesis rate distribution in a grid of proportion of the mainland and proportion nonendemic
ggplot(D2, aes(x=0, y= lambda_c)) + geom_violin() + geom_dotplot(binaxis = "y", binwidth = .5, stackdir="center") + facet_grid(prop_endemic ~ prop_mainland) 

ggplot(D2, aes(x=0, y= gamma)) + geom_violin() + geom_dotplot(binaxis = "y", binwidth = .001, stackdir="center") + facet_grid(prop_endemic ~ prop_mainland) + geom_hline(yintercept = 0.01) + ylim(0,0.03)

ggplot(D2, aes(x=0, y= mu)) + geom_violin() + geom_dotplot(binaxis = "y", binwidth = .2, stackdir="center") + facet_grid(prop_endemic ~ prop_mainland)

ggplot(D2, aes(x=0, y= lambda_a)) + geom_violin() + geom_dotplot(binaxis = "y", binwidth = .5, stackdir="center") + facet_grid(prop_endemic ~ prop_mainland) + scale_y_log10()

ggplot(D2, aes(x=0, y= K)) + geom_violin() + scale_y_log10() + geom_dotplot(binaxis = "y", binwidth = .05, stackdir="center")

D3 <- subset(D2, K < 100)

ggplot(D2, aes(x=0, y= gamma)) + geom_density()  + facet_grid(prop_endemic ~ prop_mainland) 


ggplot(D3, aes(x=0, y= K)) + geom_violin() + scale_y_log10() + geom_dotplot(binaxis = "y", binwidth = .05, stackdir="center") + facet_grid(prop_endemic ~ prop_mainland)

ggplot(D2, aes(x=0, y= mu)) + geom_violin()+ facet_grid(prop_endemic ~ prop_mainland) + geom_hline(yintercept = 2.5)

