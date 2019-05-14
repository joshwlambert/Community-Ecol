library(ggplot2)

#creates a subset of the full data with only the data for time=10 and then a subset of this subset for a cladogenesis rate of 1
DAISIETable2 <- subset(DAISIETable, time==4)
DAISIETable3 <- subset(DAISIETable2, lambda_c_sim==2.5)


#creates a heat map with the proportion endemic plotted against the proportion of the mainland area split into facets along time and cladogenesis rate
ggplot(DAISIETable, aes(x=prop_endemic,y=prop_mainland,fill=log(lambda_c))) + geom_tile() +facet_grid(lambda_c_sim ~ time)


#creates a subset for time=4 and a cladogenesis rate of 2.5
D1 <- subset(DAISIETable, time==4)
D2 <- subset(D1, lambda_c_sim==2.5)

#plots the maximum likelihood cladogenesis rate distribution in a grid of proportion of the mainland and proportion nonendemic
ggplot(D2, aes(x=0, y= lambda_c)) + geom_violin() + geom_dotplot(binaxis = "y", binwidth = .5, stackdir="center") + facet_grid(prop_endemic ~ prop_mainland) + ylim(0,15)

ggplot(D2, aes(x=0, y= gamma)) + geom_violin() + geom_dotplot(binaxis = "y", binwidth = .001, stackdir="center") + facet_grid(prop_endemic ~ prop_mainland)
