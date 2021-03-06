#Plotting data set

data<- DAISIETable[, 1:5]
data <- cbind(data, DAISIETable[, 9:10])
data <- cbind(data, DAISIETable[, 15:17])
data <- data[, -7]
dataLL <- dplyr::filter(data, prop_mainland == 0.1)
dataLL <- dplyr::filter(dataLL, prop_non_endemic == 0.5)
dataLL <- cbind(dataLL, island = 'LL')
dataLM <- dplyr::filter(data, prop_mainland == 0.1)
dataLM <- dplyr::filter(dataLM, prop_non_endemic == 0.7)
dataLM <- cbind(dataLM, island = 'LM')
dataLH <- dplyr::filter(data, prop_mainland == 0.1)
dataLH <- dplyr::filter(dataLH, prop_non_endemic == 0.9)
dataLH <- cbind(dataLH, island = 'LH')
dataML <- dplyr::filter(data, prop_mainland == 0.25)
dataML <- dplyr::filter(dataML, prop_non_endemic == 0.5)
dataML <- cbind(dataML, island = 'ML')
dataMM <- dplyr::filter(data, prop_mainland == 0.25)
dataMM <- dplyr::filter(dataMM, prop_non_endemic == 0.7)
dataMM <- cbind(dataMM, island = 'MM')
dataMH <- dplyr::filter(data, prop_mainland == 0.25)
dataMH <- dplyr::filter(dataMH, prop_non_endemic == 0.9)
dataMH <- cbind(dataMH, island = 'MH')
dataHL <- dplyr::filter(data, prop_mainland == 0.4)
dataHL <- dplyr::filter(dataHL, prop_non_endemic == 0.5)
dataHL <- cbind(dataHL, island = 'HL')
dataHM <- dplyr::filter(data, prop_mainland == 0.4)
dataHM <- dplyr::filter(dataHM, prop_non_endemic == 0.7)
dataHM <- cbind(dataHM, island = 'HM')
dataHH <- dplyr::filter(data, prop_mainland == 0.4)
dataHH <- dplyr::filter(dataHH, prop_non_endemic == 0.9)
dataHH <- cbind(dataHH, island = 'HH')
dataO <- dplyr::filter(data, island_type == 'oceanic')
dataO <- cbind(dataO, island = 'O')
final_data <- rbind(dataLL, dataLM, dataLH, dataML, dataMM, dataMH, dataHL, dataHM, dataHH, dataO)

