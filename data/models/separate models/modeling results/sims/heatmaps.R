
alldata <- read.table('kids.txt', header=F)
#alldata <- read.table('adults.txt', header=F)
#alldata <- read.table('bland.txt', header=F)
#alldata <- read.table('door.txt', header=F)


#outputs.append([subj, 'RL', B_value, sim, (subj*1000)+sim, trial, res, payoff, chose_door])

names(alldata) <- c('subj', 'model', 'beta', 'sim_num', 'sim_unique', 
					'trial', 'res', 'reward', 'chose_door', 'stay', 'nback', 'prev_rew')



alldata[alldata$trial == 0, ]$prev_rew <- -1


######################################################################
#kids
RL_subj <- c(62025, 62026, 62027, 50018, 50020, 50023, 50027, 
			 50028, 50122, 50123, 50124, 50030, 50136, 50137,
			 50139, 50140, 50141, 1000, 1001, 1003, 1007, 
			 1008, 1009, 1010, 1011, 1015, 2012, 1016, 1021, 
			 1023, 2014, 2018, 2019, 2020, 2021,
			 3092, 8120, 8122, 3098, 3100, 3101, 8123, 2105, 2106, 2107)

Lag_subj <- c(62021, 62022, 62023, 62024, 62028, 62029, 50019, 
			  50117, 50119, 50024, 50025, 50026, 50029, 50031, 
			  50032, 50033, 50034, 50138, 4105, 1004, 1013, 2011, 
			  2013, 1018, 2015, 2016, 2108)

random_subj <- c(50142, 1002, 1005, 1006, 1012, 1014, 2010, 1019, 1020, 2017, 3097)

door_subj <- c(3093, 3094, 3095, 8121, 3096, 3099, 3102, 3103, 3104, 8124, 8125, 8126, 8127, 
				8128, 8129, 2109, 2110, 2111, 2112, 2113, 2114, 2115, 2116, 2117)




######################################################################

RLdata <- alldata[alldata$subj %in% RL_subj & alldata$model == 'RL',]
Lagdata <- alldata[alldata$subj %in% Lag_subj & alldata$model == 'Lag',]
Randdata <- alldata[alldata$subj %in% random_subj & alldata$model == 'Random',]
Doordata <- alldata[alldata$subj %in% door_subj & alldata$model == 'Door',]

######################################################################

######################################################################################


RL_transitions <- tapply(RLdata$reward, list( RLdata$reward, RLdata$prev_rew), length)

RL_transitions_temp <- RL_transitions

sums <- colSums(RL_transitions)

for (i in seq(length(RL_transitions[,1]))) {
	RL_transitions[i,] <- RL_transitions_temp[i,]/sums
}




tx <- RL_transitions[order(as.integer(row.names(RL_transitions)), decreasing = T),]

library(gplots)
library("RColorBrewer")
heatmap.2(tx[,2:5], cellnote=round(tx[,2:5],3), Rowv=NA, Colv=NA, col=brewer.pal(9,"Blues"), 
			dendrogram = 'none', notecol='black', revC=T, notecex=1.5, trace='none',
			breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
			scale="none", xlab = "previous trial", ylab = 'trial', density.info="none")
			
#############################

Lag_transitions <- tapply(Lagdata$reward, list( Lagdata$reward, Lagdata$prev_rew), length)

Lag_transitions_temp <- Lag_transitions

sums <- colSums(Lag_transitions)

for (i in seq(length(Lag_transitions[,1]))) {
	Lag_transitions[i,] <- Lag_transitions_temp[i,]/sums
}




tx <- Lag_transitions[order(as.integer(row.names(Lag_transitions)), decreasing = T),]

heatmap.2(tx[,2:5], cellnote=round(tx[,2:5],3), Rowv=NA, Colv=NA, col=brewer.pal(9,"Blues"), 
			dendrogram = 'none', notecol='black', revC=T, notecex=1.5, trace='none',
			breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
			scale="none", xlab = "previous trial", ylab = 'trial', density.info="none")

################################


Rand_transitions <- tapply(Randdata$reward, list( Randdata$reward, Randdata$prev_rew), length)

Rand_transitions_temp <- Rand_transitions

sums <- colSums(Rand_transitions)

for (i in seq(length(Rand_transitions[,1]))) {
	Rand_transitions[i,] <- Rand_transitions_temp[i,]/sums
}




tx <- Rand_transitions[order(as.integer(row.names(Rand_transitions)), decreasing = T),]

heatmap.2(tx[,2:5], cellnote=round(tx[,2:5],3), Rowv=NA, Colv=NA, col=brewer.pal(9,"Blues"), 
			dendrogram = 'none', notecol='black', revC=T, notecex=1.5, trace='none',
			breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
			scale="none", xlab = "previous trial", ylab = 'trial')

#####################################

Door_transitions <- tapply(Doordata$reward, list( Doordata$reward, Doordata$prev_rew), length)

Door_transitions_temp <- Door_transitions

sums <- colSums(Door_transitions)

for (i in seq(length(Door_transitions[,1]))) {
	Door_transitions[i,] <- Door_transitions_temp[i,]/sums
}




tx <- Door_transitions[order(as.integer(row.names(Door_transitions)), decreasing = T),]

heatmap.2(tx[,2:5], cellnote=round(tx[,2:5],3), Rowv=NA, Colv=NA, col=brewer.pal(9,"Blues"), 
			dendrogram = 'none', notecol='black', revC=T, notecex=1.5, trace='none',
			breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
			scale="none", xlab = "previous trial", ylab = 'trial')

########################################
