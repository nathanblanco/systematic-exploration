

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

###############

hazard_lengths <- tapply( RLdata$nback, list(RLdata$nback, RLdata$sim_unique), length )
hazard_lengths[is.na(hazard_lengths)] <- 0

# cut out the zero row which is meaningless
hazard_lengths <- hazard_lengths[2:length(hazard_lengths[,1]),]

hazard_sums <- hazard_lengths

for (i in seq(2:length(hazard_lengths[,1]))) {
	
	hazard_sums[i,] <- colSums( hazard_lengths[i:length(hazard_lengths[,1]),] )

}

hazard_props <- hazard_lengths/hazard_sums
hazard_means <- rowMeans(hazard_props, na.rm=T)

library(matrixStats)

hazard_sds <- rowSds(hazard_props, na.rm=T)
hazard_Ns <- rowSums(!is.na(hazard_props))
hazard_ses <- hazard_sds/sqrt(hazard_Ns)

x= seq(10)
plot( hazard_means, type='l', xlim=c(1,7), main="Hazard rates", ylim=c(0.0,0.8), 
		col='red', ylab='Proportion of trials given option chosen',
		xlab='Trials since option last chosen', lwd=2)
segments(x, hazard_means[1:10]+hazard_ses[1:10], x, hazard_means[1:10]-hazard_ses[1:10], col='red')

#####################

hazard_lengths <- tapply( Lagdata$nback, list(Lagdata$nback, Lagdata$sim_unique), length )
hazard_lengths[is.na(hazard_lengths)] <- 0

# cut out the zero row which is meaningless
hazard_lengths <- hazard_lengths[2:length(hazard_lengths[,1]),]
hazard_sums <- hazard_lengths

for (i in seq(2:length(hazard_lengths[,1]))) {
	
	hazard_sums[i,] <- colSums( hazard_lengths[i:length(hazard_lengths[,1]),] )

}


hazard_props <- hazard_lengths/hazard_sums

hazard_means <- rowMeans(hazard_props, na.rm=T)

library(matrixStats)

hazard_sds <- rowSds(hazard_props, na.rm=T)

hazard_Ns <- rowSums(!is.na(hazard_props))

hazard_ses <- hazard_sds/sqrt(hazard_Ns)

#x= seq(10)
#plot( hazard_means, type='l', xlim=c(0,10), main="Lag sims")
lines( hazard_means,  lwd=2, col='purple')
segments(x, hazard_means[1:10]+hazard_ses[1:10], x, hazard_means[1:10]-hazard_ses[1:10], col='purple')


legend(1, 0.8, c("Value", "Lag"), 
	   col = c("purple", "red"), 
	   lty = c(1,1), cex = 1.2)

##############################
###########################################

hazard_lengths <- tapply( Randdata$nback, list(Randdata$nback, Randdata$sim_unique), length )
hazard_lengths[is.na(hazard_lengths)] <- 0

# cut out the zero row which is meaningless
hazard_lengths <- hazard_lengths[2:length(hazard_lengths[,1]),]

hazard_sums <- hazard_lengths

for (i in seq(2:length(hazard_lengths[,1]))) {
	
	hazard_sums[i,] <- colSums( hazard_lengths[i:length(hazard_lengths[,1]),] )

}


hazard_props <- hazard_lengths/hazard_sums

hazard_means <- rowMeans(hazard_props, na.rm=T)

library(matrixStats)

hazard_sds <- rowSds(hazard_props, na.rm=T)

hazard_Ns <- rowSums(!is.na(hazard_props))

hazard_ses <- hazard_sds/sqrt(hazard_Ns)

x= seq(10)
#plot( hazard_means, type='l', xlim=c(0,10), main="Random sims")
lines( hazard_means, lwd=2, col='black')
segments(x, hazard_means[1:10]+hazard_ses[1:10], x, hazard_means[1:10]-hazard_ses[1:10])

################################################

hazard_lengths <- tapply( Doordata$nback, list(Doordata$nback, Doordata$sim_unique), length )
hazard_lengths[is.na(hazard_lengths)] <- 0

# cut out the zero row which is meaningless
hazard_lengths <- hazard_lengths[2:length(hazard_lengths[,1]),]

hazard_sums <- hazard_lengths

for (i in seq(2:length(hazard_lengths[,1]))) {
	
	hazard_sums[i,] <- colSums( hazard_lengths[i:length(hazard_lengths[,1]),] )

}


hazard_props <- hazard_lengths/hazard_sums

hazard_means <- rowMeans(hazard_props, na.rm=T)

library(matrixStats)

hazard_sds <- rowSds(hazard_props, na.rm=T)

hazard_Ns <- rowSums(!is.na(hazard_props))

hazard_ses <- hazard_sds/sqrt(hazard_Ns)

x= seq(10)
plot( hazard_means, type='l', xlim=c(0,10), main="Door sims")
segments(x, hazard_means[1:10]+hazard_ses[1:10], x, hazard_means[1:10]-hazard_ses[1:10])



##############################

########################################################

# maybe break up door sims by the beta value

# do this based on best-fitting model

# we also need hazard rates

