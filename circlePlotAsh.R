library(ggplot2)
library(viridis)
library(circlize)
library(cowplot)
library(lubridate)
library(viridis)

setwd("~/Downloads/April_DTA_results/")
#data <- read.table('phylogeo_DTA.location.rates.log', header=T)
data <- read.table('location.Feb_VIC_travelHistory_rep2.loc.rates.log', header=T)


get_migration_mean <- function(data) {
	# note floor(0.1*length(data[,1]) alows for 10% burnin
	data <- data[-c(1:floor(0.1*length(data[,1]))),]
	# need adjacency matrix
	rates <- matrix(nrow=9,ncol=9)
	
	# from row
	rownames(rates) <- c('Africa', 'Asia', 'China', 'Europe', 'NorthAmerica','NewZealand','SouthAmerica', 'UnitedKingdom', 'Australia')
	# to columns
	colnames(rates) <- c('Africa', 'Asia', 'China', 'Europe', 'NorthAmerica', 'NewZealand', 'SouthAmerica', 'UnitedKingdom', 'Australia')

	rates[1,1] <- NA
	rates[1,2] <- mean(data$loc.rates.Africa.Asia)
	rates[1,3] <- mean(data$loc.rates.Africa.China)
	rates[1,4] <- mean(data$loc.rates.Africa.Europe)
	rates[1,5] <- mean(data$loc.rates.Africa.NorthAmerica)
	rates[1,6] <- mean(data$loc.rates.Africa.NewZealand)
	rates[1,7] <- mean(data$loc.rates.Africa.SouthAmerica)
	rates[1,8] <- mean(data$loc.rates.Africa.UnitedKingdom)
	rates[1,9] <- mean(data$loc.rates.Africa.Australia)
	
	
	rates[2,1] <- mean(data$loc.rates.Asia.Africa)
	rates[2,2] <- NA
	rates[2,3] <- mean(data$loc.rates.Asia.China)
	rates[2,4] <- mean(data$loc.rates.Asia.Europe)
	rates[2,5] <- mean(data$loc.rates.Asia.NorthAmerica)
	rates[2,6] <- mean(data$loc.rates.Asia.NewZealand)
	rates[2,7] <- mean(data$loc.rates.Asia.SouthAmerica)
	rates[2,8] <- mean(data$loc.rates.Asia.UnitedKingdom)
	rates[2,9] <- mean(data$loc.rates.Asia.Australia)

	rates[3,1] <- mean(data$loc.rates.China.Africa)
	rates[3,2] <- mean(data$loc.rates.China.Asia)
	rates[3,3] <- NA
	rates[3,4] <- mean(data$loc.rates.China.Europe)
	rates[3,5] <- mean(data$loc.rates.China.NorthAmerica)
	rates[3,6] <- mean(data$loc.rates.China.NewZealand)
	rates[3,7] <- mean(data$loc.rates.China.SouthAmerica)
	rates[3,8] <- mean(data$loc.rates.China.UnitedKingdom)
	rates[3,9] <- mean(data$loc.rates.China.Australia)

	rates[4,1] <- mean(data$loc.rates.Europe.Africa)
	rates[4,2] <- mean(data$loc.rates.Europe.Asia)
	rates[4,3] <- mean(data$loc.rates.Europe.China)
	rates[4,4] <- NA
	rates[4,5] <- mean(data$loc.rates.Europe.NorthAmerica)
	rates[4,6] <- mean(data$loc.rates.Europe.NewZealand)
	rates[4,7] <- mean(data$loc.rates.Europe.SouthAmerica)
	rates[4,8] <- mean(data$loc.rates.Europe.UnitedKingdom)
	rates[4,9] <- mean(data$loc.rates.Europe.Australia)
	
	rates[5,1] <- mean(data$loc.rates.NorthAmerica.Africa)
	rates[5,2] <- mean(data$loc.rates.NorthAmerica.Asia)
	rates[5,3] <- mean(data$loc.rates.NorthAmerica.China)
	rates[5,4] <- mean(data$loc.rates.NorthAmerica.Europe)
	rates[5,5] <- NA
	rates[5,6] <- mean(data$loc.rates.NorthAmerica.NewZealand)
	rates[5,7] <- mean(data$loc.rates.NorthAmerica.SouthAmerica)
	rates[5,8] <- mean(data$loc.rates.NorthAmerica.UnitedKingdom)
	rates[5,9] <- mean(data$loc.rates.NorthAmericaAustralia)
	
	rates[6,1] <- mean(data$loc.rates.NewZealand.Africa)
	rates[6,2] <- mean(data$loc.rates.NewZealand.Asia)
	rates[6,3] <- mean(data$loc.rates.NewZealand.Africa)
	rates[6,4] <- mean(data$loc.rates.NewZealand.Europe)
	rates[6,5] <- mean(data$loc.rates.NewZealand.NorthAmerica)
	rates[6,6] <- NA
	rates[6,7] <- mean(data$loc.rates.NewZealand.SouthAmerica)
	rates[6,8] <- mean(data$loc.rates.NewZealand.UnitedKingdom)
	rates[6,9] <- mean(data$loc.rates.NewZealand.Australia)
	
	rates[7,1] <- mean(data$loc.rates.SouthAmerica.Africa)
	rates[7,2] <- mean(data$loc.rates.SouthAmerica.Asia)
	rates[7,3] <- mean(data$loc.rates.SouthAmerica.China)
	rates[7,4] <- mean(data$loc.rates.SouthAmerica.Europe)
	rates[7,5] <- mean(data$loc.rates.SouthAmerica.NorthAmerica)
	rates[7,6] <- mean(data$loc.rates.SouthAmerica.NewZealand)
	rates[7,7] <- NA
	rates[7,8] <- mean(data$loc.rates.SouthAmerica.UnitedKingdom)
	rates[7,9] <- mean(data$loc.rates.SouthAmericaAustralia)
	
	rates[8,1] <- mean(data$loc.rates.UnitedKingdom.Africa)
	rates[8,2] <- mean(data$loc.rates.UnitedKingdom.Asia)
	rates[8,3] <- mean(data$loc.rates.UnitedKingdom.China)
	rates[8,4] <- mean(data$loc.rates.UnitedKingdom.Europe)
	rates[8,5] <- mean(data$loc.rates.UnitedKingdom.NorthAmerica)
	rates[8,6] <- mean(data$loc.rates.UnitedKingdom.NewZealand)
	rates[8,7] <- mean(data$loc.rates.UnitedKingdom.SouthAmerica)
	rates[8,8] <- NA
	rates[8,9] <- mean(data$loc.rates.UnitedKingdom.Australia)
	
	rates[9,1] <- mean(data$loc.rates.Australia.Africa)
	rates[9,2] <- mean(data$loc.rates.Australia.Asia)
	rates[9,3] <- mean(data$loc.rates.Australia.China)
	rates[9,4] <- mean(data$loc.rates.Australia.Europe)
	rates[9,5] <- mean(data$loc.rates.Australia.NorthAmerica)
	rates[9,6] <- mean(data$loc.rates.Australia.NewZealand)
	rates[9,7] <- mean(data$loc.rates.Australia.SouthAmerica)
	rates[9,8] <- mean(data$loc.rates.Australia.UnitedKingdom)
	rates[9,9] <- NA

	return(rates)
}

rates <- get_migration_mean(data)
#Plotting matrix in circle plot

#arrow colours
arr.col <- data.frame(expand.grid(rownames(rates), colnames(rates)), rep('black', times=81))

pdf(file='migration_plot_Feb_TH.pdf', useDingbats=F)
	chordDiagram(rates, grid.col=viridis(), order=colnames(rates),
	directional=1, direction.type = "arrows", link.arr.col = arr.col, link.arr.length = 0.2)
dev.off()


##### DONE ##### 

# you can use the below to save the circular plot as 'circ' if you want to put it in a panel figure later on, like below
chordDiagram(rates, grid.col=viridis(9), order=colnames(rates),
	directional=1, direction.type = "arrows", link.arr.col = arr.col, link.arr.length = 0.2)
circ <- recordPlot()

write.csv(rates, "Feb_TH_rates.csv", row.names = T)
print(rates)




