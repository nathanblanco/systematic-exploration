





alldata <- read.table('baseline_children.txt', header=F)
#alldata <- read.table('bland.txt', header=F)

#alldata <- read.table('baseline_adults.txt', header=F)

#alldata <- read.table('novelty1_adults.txt', header=F)

#alldata <- read.table('novelty10_children.txt', header=F)

#alldata <- read.table('creatures10_children.txt', header=F)
#alldata <- read.table('creature1_children.txt', header=F)

#alldata <- read.table('baseline5-7.txt', header=F)

#alldata <- read.table('BL_NV_combined_children.txt', header=F)
#alldata <- read.table('BL_NV_combined_adults.txt', header=F)

# self.output_trial([self.subj, 'main', self.trial, self.cond, res, rt, payoff, self.earnings, self.creatures[int(res)], self.rewards])

names(alldata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'r1', 'r2', 'r3', 'r4')

#alldata <- read.table('door_data.txt', header=F)

#names(alldata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'door_loc', 'door_chose', 'r1', 'r2', 'r3', 'r4')


order_list <- NULL
order_list$subj <- unique(alldata$subj)
order_list$order <- order(unique(alldata$subj))

order_list <- as.data.frame(order_list)

alldata$order <- NULL
for (i in 1:length(alldata[,1])) {
	for (j in  1:length(order_list[,1])) {
		if (alldata[i,]$subj == order_list[j,]$subj) {
			alldata$order[i] <- order_list[j,]$order
		}
	}
}

# for graphing group data without the optimizers
# alldata <- alldata[alldata$subj != 62025,]
# alldata <- alldata[alldata$subj != 62026,]
# alldata <- alldata[alldata$subj != 62039,]
# alldata <- alldata[alldata$subj != 1111,]
# alldata <- alldata[alldata$subj != 1112,]
# alldata <- alldata[alldata$subj != 50020,]
# alldata <- alldata[alldata$subj != 50023,]
# alldata <- alldata[alldata$subj != 50018,]



# just for checking without those with bad memory performance
#alldata <- alldata[alldata$subj != 50029,]
#alldata <- alldata[alldata$subj != 50124,]
#alldata <- alldata[alldata$subj != 50031,]
#alldata <- alldata[alldata$subj != 50137,]
#alldata <- alldata[alldata$subj != 50138,]

maindata <- alldata[alldata$phase == 'main',]
testdata <- alldata[alldata$phase == 'test',]

maindata$stay <- NULL
maindata$prev_rew <- NULL

for (i in 1:length(maindata[,1])) {
    maindata$stay[i] <- as.numeric(maindata[i,]$res == maindata[i-1,]$res)
    
    maindata$prev_rew[i] <- maindata[i-1,]$reward
    
    if (maindata[i,]$trial == 1) { 
    	maindata$stay[i] <- 0 
    	maindata$prev_rew[i] <- -1
    	}
    
    }


maindata$nback <- NA
for (i in 2:length(maindata[,1])) {
    found <- 0
    for (j in seq(maindata[i,]$trial-1)) {
    	if ( (maindata[i,]$res == maindata[i-j,]$res) & (found == 0)) {
    		
    		maindata$nback[i] <- j
    		found = 1
    		}    		
    	}
    }
    

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


#####################################################################

RLdata <- maindata[maindata$subj %in% RL_subj,]
Lagdata <- maindata[maindata$subj %in% Lag_subj,]
Randdata <- maindata[maindata$subj %in% random_subj,]
Doordata <- maindata[maindata$subj %in% door_subj,]

#####################################################################



hazard_lengths <- tapply( RLdata$nback, list(RLdata$nback, RLdata$subj), length )
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

hazard_lengths <- tapply( Lagdata$nback, list(Lagdata$nback, Lagdata$subj), length )
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
