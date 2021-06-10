

setwd('/Users/nathanblanco/Dropbox/Work/current_projects/exploration studies/Baseline/data/models/separate models/modeling results/sims')

alldata <- read.table('kids.txt', header=F)

patterndata <- read.table('pattern_sims.txt', header=F)

#outputs.append([subj, 'RL', B_value, sim, (subj*1000)+sim, trial, res, payoff, chose_door])

names(alldata) <- c('subj', 'model', 'beta', 'sim_num', 'sim_unique', 
					'trial', 'res', 'reward', 'chose_door', 'stay', 'nback', 'prev_rew')

names(patterndata) <- c('subj', 'model', 'cw', 'ccw', 'skip', 'sim_num', 'sim_unique', 
					'trial', 'res', 'reward', 'chose_door', 'stay', 'nback', 'prev_rew')

alldata[alldata$trial == 0, ]$prev_rew <- -1


######################################################################

Lagdata <- alldata[alldata$subj %in% Lag_subj & alldata$model == 'Lag',]

######################################################################

###############

hazard_lengths <- tapply( Lagdata$nback, list(Lagdata$nback, Lagdata$sim_unique), length )
hazard_lengths[is.na(hazard_lengths)] <- 0

# cut out the zero row which is meaningless
hazard_lengths <- hazard_lengths[1:length(hazard_lengths[,1]),]

hazard_sums <- hazard_lengths

for (i in seq(1:length(hazard_lengths[,1]))) {
	
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
		col='purple', ylab='Proportion of trials given option chosen',
		xlab='Trials since option last chosen', lwd=2)
segments(x, hazard_means[1:10]+hazard_ses[1:10], x, hazard_means[1:10]-hazard_ses[1:10], col='purple')

#####################


hazard_lengths <- tapply( patterndata$nback, list(patterndata$nback, patterndata$sim_unique), length )
hazard_lengths[is.na(hazard_lengths)] <- 0

# cut out the zero row which is meaningless
hazard_lengths <- hazard_lengths[1:length(hazard_lengths[,1]),]
hazard_sums <- hazard_lengths

for (i in seq(1:length(hazard_lengths[,1]))) {
	
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
lines( hazard_means,  lwd=2, col='black', lty=2)
segments(x, hazard_means[1:10]+hazard_ses[1:10], x, hazard_means[1:10]-hazard_ses[1:10], col='black', lty=2)


legend(1, 0.8, c("Lag", "Pattern"), 
	   col = c("purple", "black"), 
	   lty = c(1,2), cex = 1.2)