

#alldata <- read.table('kids.txt', header=F)
#alldata <- read.table('adults.txt', header=F)
#alldata <- read.table('bland.txt', header=F)
alldata <- read.table('door.txt', header=F)


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


###############################################

choice_props <- tapply(RLdata$reward, list(RLdata$sim_unique, RLdata$reward), length)

block1 <- RLdata[RLdata$trial <= 20,]
block2 <- RLdata[RLdata$trial > 20 & RLdata$trial <= 40,]
block3 <- RLdata[RLdata$trial > 40 & RLdata$trial <= 60,]
block4 <- RLdata[RLdata$trial > 60 & RLdata$trial <= 80,]
block5 <- RLdata[RLdata$trial > 80 & RLdata$trial <= 100,]


block1_choices <- tapply(block1$reward, list(block1$sim_unique, block1$reward), length)
block1_choices[is.na(block1_choices)] <- 0

block2_choices <- tapply(block2$reward, list(block2$sim_unique, block2$reward), length)
block2_choices[is.na(block2_choices)] <- 0

block3_choices <- tapply(block3$reward, list(block3$sim_unique, block3$reward), length)
block3_choices[is.na(block3_choices)] <- 0

block4_choices <- tapply(block4$reward, list(block4$sim_unique, block4$reward), length)
block4_choices[is.na(block4_choices)] <- 0

block5_choices <- tapply(block5$reward, list(block5$sim_unique, block5$reward), length)
block5_choices[is.na(block5_choices)] <- 0


# means
line10_means <- c( mean(block1_choices[,4]), 
             mean(block2_choices[,4]),
             mean(block3_choices[,4]),
             mean(block4_choices[,4]),
             mean(block5_choices[,4]))
             
line3_means <- c( mean(block1_choices[,3]), 
             mean(block2_choices[,3]),
             mean(block3_choices[,3]),
             mean(block4_choices[,3]),
             mean(block5_choices[,3]))
             
line2_means <- c( mean(block1_choices[,2]), 
             mean(block2_choices[,2]),
             mean(block3_choices[,2]),
             mean(block4_choices[,2]),
             mean(block5_choices[,2]))
             
line1_means <- c( mean(block1_choices[,1]), 
             mean(block2_choices[,1]),
             mean(block3_choices[,1]),
             mean(block4_choices[,1]),
             mean(block5_choices[,1]))

# standard deviations
line10_sds <- c( sd(block1_choices[,4]), 
             sd(block2_choices[,4]),
             sd(block3_choices[,4]),
             sd(block4_choices[,4]),
             sd(block5_choices[,4]))
             
line3_sds <- c( sd(block1_choices[,3]), 
             sd(block2_choices[,3]),
             sd(block3_choices[,3]),
             sd(block4_choices[,3]),
             sd(block5_choices[,3]))
             
line2_sds <- c( sd(block1_choices[,2]), 
             sd(block2_choices[,2]),
             sd(block3_choices[,2]),
             sd(block4_choices[,2]),
             sd(block5_choices[,2]))
             
line1_sds <- c( sd(block1_choices[,1]), 
             sd(block2_choices[,1]),
             sd(block3_choices[,1]),
             sd(block4_choices[,1]),
             sd(block5_choices[,1]))

# Ns
line10_lengths <- c( length(block1_choices[,4]), 
             length(block2_choices[,4]),
             length(block3_choices[,4]),
             length(block4_choices[,4]),
             length(block5_choices[,4]))
             
line3_lengths <- c( length(block1_choices[,3]), 
             length(block2_choices[,3]),
             length(block3_choices[,3]),
             length(block4_choices[,3]),
             length(block5_choices[,3]))
             
line2_lengths <- c( length(block1_choices[,2]), 
             length(block2_choices[,2]),
             length(block3_choices[,2]),
             length(block4_choices[,2]),
             length(block5_choices[,2]))
             
line1_lengths <- c( length(block1_choices[,1]), 
             length(block2_choices[,1]),
             length(block3_choices[,1]),
             length(block4_choices[,1]),
             length(block5_choices[,1]))
             
line10_ses <- line10_sds/sqrt(line10_lengths)
line3_ses <- line3_sds/sqrt(line3_lengths)
line2_ses <- line2_sds/sqrt(line2_lengths)
line1_ses <- line1_sds/sqrt(line1_lengths)



plot(line10_means/20, type='l', col = 'blue', 
	 ylim=c(0,1), ylab='Proportion of selections', 
	 xlab="Block (20 trials)", main='RL sims', cex.lab= 1.4, cex.axis=1.3, lwd=3)
lines(line3_means/20, col = 'green', lwd=3)
lines(line2_means/20, col = 'yellow', lwd=3)
lines(line1_means/20, col = 'red', lwd=3)

legend(1, 1.0, c("10", "3", "2", "1"), 
	   col = c("blue", "green", "yellow", "red"), 
	   lty = c(1,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, line10_means/20+line10_ses/20, x, line10_means/20-line10_ses/20, col='blue')
segments(x, line3_means/20+line3_ses/20, x, line3_means/20-line3_ses/20, col='green')
segments(x, line2_means/20+line2_ses/20, x, line2_means/20-line2_ses/20, col='yellow')
segments(x, line1_means/20+line1_ses/20, x, line1_means/20-line1_ses/20, col='red')


###############################################

choice_props <- tapply(Lagdata$reward, list(Lagdata$sim_unique, Lagdata$reward), length)

block1 <- Lagdata[Lagdata$trial <= 20,]
block2 <- Lagdata[Lagdata$trial > 20 & Lagdata$trial <= 40,]
block3 <- Lagdata[Lagdata$trial > 40 & Lagdata$trial <= 60,]
block4 <- Lagdata[Lagdata$trial > 60 & Lagdata$trial <= 80,]
block5 <- Lagdata[Lagdata$trial > 80 & Lagdata$trial <= 100,]


block1_choices <- tapply(block1$reward, list(block1$sim_unique, block1$reward), length)
block1_choices[is.na(block1_choices)] <- 0

block2_choices <- tapply(block2$reward, list(block2$sim_unique, block2$reward), length)
block2_choices[is.na(block2_choices)] <- 0

block3_choices <- tapply(block3$reward, list(block3$sim_unique, block3$reward), length)
block3_choices[is.na(block3_choices)] <- 0

block4_choices <- tapply(block4$reward, list(block4$sim_unique, block4$reward), length)
block4_choices[is.na(block4_choices)] <- 0

block5_choices <- tapply(block5$reward, list(block5$sim_unique, block5$reward), length)
block5_choices[is.na(block5_choices)] <- 0


# means
line10_means <- c( mean(block1_choices[,4]), 
             mean(block2_choices[,4]),
             mean(block3_choices[,4]),
             mean(block4_choices[,4]),
             mean(block5_choices[,4]))
             
line3_means <- c( mean(block1_choices[,3]), 
             mean(block2_choices[,3]),
             mean(block3_choices[,3]),
             mean(block4_choices[,3]),
             mean(block5_choices[,3]))
             
line2_means <- c( mean(block1_choices[,2]), 
             mean(block2_choices[,2]),
             mean(block3_choices[,2]),
             mean(block4_choices[,2]),
             mean(block5_choices[,2]))
             
line1_means <- c( mean(block1_choices[,1]), 
             mean(block2_choices[,1]),
             mean(block3_choices[,1]),
             mean(block4_choices[,1]),
             mean(block5_choices[,1]))

# standard deviations
line10_sds <- c( sd(block1_choices[,4]), 
             sd(block2_choices[,4]),
             sd(block3_choices[,4]),
             sd(block4_choices[,4]),
             sd(block5_choices[,4]))
             
line3_sds <- c( sd(block1_choices[,3]), 
             sd(block2_choices[,3]),
             sd(block3_choices[,3]),
             sd(block4_choices[,3]),
             sd(block5_choices[,3]))
             
line2_sds <- c( sd(block1_choices[,2]), 
             sd(block2_choices[,2]),
             sd(block3_choices[,2]),
             sd(block4_choices[,2]),
             sd(block5_choices[,2]))
             
line1_sds <- c( sd(block1_choices[,1]), 
             sd(block2_choices[,1]),
             sd(block3_choices[,1]),
             sd(block4_choices[,1]),
             sd(block5_choices[,1]))

# Ns
line10_lengths <- c( length(block1_choices[,4]), 
             length(block2_choices[,4]),
             length(block3_choices[,4]),
             length(block4_choices[,4]),
             length(block5_choices[,4]))
             
line3_lengths <- c( length(block1_choices[,3]), 
             length(block2_choices[,3]),
             length(block3_choices[,3]),
             length(block4_choices[,3]),
             length(block5_choices[,3]))
             
line2_lengths <- c( length(block1_choices[,2]), 
             length(block2_choices[,2]),
             length(block3_choices[,2]),
             length(block4_choices[,2]),
             length(block5_choices[,2]))
             
line1_lengths <- c( length(block1_choices[,1]), 
             length(block2_choices[,1]),
             length(block3_choices[,1]),
             length(block4_choices[,1]),
             length(block5_choices[,1]))
             
line10_ses <- line10_sds/sqrt(line10_lengths)
line3_ses <- line3_sds/sqrt(line3_lengths)
line2_ses <- line2_sds/sqrt(line2_lengths)
line1_ses <- line1_sds/sqrt(line1_lengths)



plot(line10_means/20, type='l', col = 'blue', 
	 ylim=c(0,1), ylab='Proportion of selections', 
	 xlab="Block (20 trials)", main='Lag sims', cex.lab= 1.4, cex.axis=1.3, lwd=3)
lines(line3_means/20, col = 'green', lwd=3)
lines(line2_means/20, col = 'yellow', lwd=3)
lines(line1_means/20, col = 'red', lwd=3)

legend(1, 1.0, c("10", "3", "2", "1"), 
	   col = c("blue", "green", "yellow", "red"), 
	   lty = c(1,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, line10_means/20+line10_ses/20, x, line10_means/20-line10_ses/20, col='blue')
segments(x, line3_means/20+line3_ses/20, x, line3_means/20-line3_ses/20, col='green')
segments(x, line2_means/20+line2_ses/20, x, line2_means/20-line2_ses/20, col='yellow')
segments(x, line1_means/20+line1_ses/20, x, line1_means/20-line1_ses/20, col='red')


###############################################

choice_props <- tapply(Randdata$reward, list(Randdata$sim_unique, Randdata$reward), length)

block1 <- Randdata[Randdata$trial <= 20,]
block2 <- Randdata[Randdata$trial > 20 & Randdata$trial <= 40,]
block3 <- Randdata[Randdata$trial > 40 & Randdata$trial <= 60,]
block4 <- Randdata[Randdata$trial > 60 & Randdata$trial <= 80,]
block5 <- Randdata[Randdata$trial > 80 & Randdata$trial <= 100,]


block1_choices <- tapply(block1$reward, list(block1$sim_unique, block1$reward), length)
block1_choices[is.na(block1_choices)] <- 0

block2_choices <- tapply(block2$reward, list(block2$sim_unique, block2$reward), length)
block2_choices[is.na(block2_choices)] <- 0

block3_choices <- tapply(block3$reward, list(block3$sim_unique, block3$reward), length)
block3_choices[is.na(block3_choices)] <- 0

block4_choices <- tapply(block4$reward, list(block4$sim_unique, block4$reward), length)
block4_choices[is.na(block4_choices)] <- 0

block5_choices <- tapply(block5$reward, list(block5$sim_unique, block5$reward), length)
block5_choices[is.na(block5_choices)] <- 0


# means
line10_means <- c( mean(block1_choices[,4]), 
             mean(block2_choices[,4]),
             mean(block3_choices[,4]),
             mean(block4_choices[,4]),
             mean(block5_choices[,4]))
             
line3_means <- c( mean(block1_choices[,3]), 
             mean(block2_choices[,3]),
             mean(block3_choices[,3]),
             mean(block4_choices[,3]),
             mean(block5_choices[,3]))
             
line2_means <- c( mean(block1_choices[,2]), 
             mean(block2_choices[,2]),
             mean(block3_choices[,2]),
             mean(block4_choices[,2]),
             mean(block5_choices[,2]))
             
line1_means <- c( mean(block1_choices[,1]), 
             mean(block2_choices[,1]),
             mean(block3_choices[,1]),
             mean(block4_choices[,1]),
             mean(block5_choices[,1]))

# standard deviations
line10_sds <- c( sd(block1_choices[,4]), 
             sd(block2_choices[,4]),
             sd(block3_choices[,4]),
             sd(block4_choices[,4]),
             sd(block5_choices[,4]))
             
line3_sds <- c( sd(block1_choices[,3]), 
             sd(block2_choices[,3]),
             sd(block3_choices[,3]),
             sd(block4_choices[,3]),
             sd(block5_choices[,3]))
             
line2_sds <- c( sd(block1_choices[,2]), 
             sd(block2_choices[,2]),
             sd(block3_choices[,2]),
             sd(block4_choices[,2]),
             sd(block5_choices[,2]))
             
line1_sds <- c( sd(block1_choices[,1]), 
             sd(block2_choices[,1]),
             sd(block3_choices[,1]),
             sd(block4_choices[,1]),
             sd(block5_choices[,1]))

# Ns
line10_lengths <- c( length(block1_choices[,4]), 
             length(block2_choices[,4]),
             length(block3_choices[,4]),
             length(block4_choices[,4]),
             length(block5_choices[,4]))
             
line3_lengths <- c( length(block1_choices[,3]), 
             length(block2_choices[,3]),
             length(block3_choices[,3]),
             length(block4_choices[,3]),
             length(block5_choices[,3]))
             
line2_lengths <- c( length(block1_choices[,2]), 
             length(block2_choices[,2]),
             length(block3_choices[,2]),
             length(block4_choices[,2]),
             length(block5_choices[,2]))
             
line1_lengths <- c( length(block1_choices[,1]), 
             length(block2_choices[,1]),
             length(block3_choices[,1]),
             length(block4_choices[,1]),
             length(block5_choices[,1]))
             
line10_ses <- line10_sds/sqrt(line10_lengths)
line3_ses <- line3_sds/sqrt(line3_lengths)
line2_ses <- line2_sds/sqrt(line2_lengths)
line1_ses <- line1_sds/sqrt(line1_lengths)



plot(line10_means/20, type='l', col = 'blue', 
	 ylim=c(0,1), ylab='Proportion of selections', 
	 xlab="Block (20 trials)", main='Random sims', cex.lab= 1.4, cex.axis=1.3, lwd=3)
lines(line3_means/20, col = 'green', lwd=3)
lines(line2_means/20, col = 'yellow', lwd=3)
lines(line1_means/20, col = 'red', lwd=3)

legend(1, 1.0, c("10", "3", "2", "1"), 
	   col = c("blue", "green", "yellow", "red"), 
	   lty = c(1,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, line10_means/20+line10_ses/20, x, line10_means/20-line10_ses/20, col='blue')
segments(x, line3_means/20+line3_ses/20, x, line3_means/20-line3_ses/20, col='green')
segments(x, line2_means/20+line2_ses/20, x, line2_means/20-line2_ses/20, col='yellow')
segments(x, line1_means/20+line1_ses/20, x, line1_means/20-line1_ses/20, col='red')



###############################################

choice_props <- tapply(Doordata$reward, list(Doordata$sim_unique, Doordata$reward), length)

block1 <- Doordata[Doordata$trial <= 20,]
block2 <- Doordata[Doordata$trial > 20 & Doordata$trial <= 40,]
block3 <- Doordata[Doordata$trial > 40 & Doordata$trial <= 60,]
block4 <- Doordata[Doordata$trial > 60 & Doordata$trial <= 80,]
block5 <- Doordata[Doordata$trial > 80 & Doordata$trial <= 100,]


block1_choices <- tapply(block1$reward, list(block1$sim_unique, block1$reward), length)
block1_choices[is.na(block1_choices)] <- 0

block2_choices <- tapply(block2$reward, list(block2$sim_unique, block2$reward), length)
block2_choices[is.na(block2_choices)] <- 0

block3_choices <- tapply(block3$reward, list(block3$sim_unique, block3$reward), length)
block3_choices[is.na(block3_choices)] <- 0

block4_choices <- tapply(block4$reward, list(block4$sim_unique, block4$reward), length)
block4_choices[is.na(block4_choices)] <- 0

block5_choices <- tapply(block5$reward, list(block5$sim_unique, block5$reward), length)
block5_choices[is.na(block5_choices)] <- 0


# means
line10_means <- c( mean(block1_choices[,4]), 
             mean(block2_choices[,4]),
             mean(block3_choices[,4]),
             mean(block4_choices[,4]),
             mean(block5_choices[,4]))
             
line3_means <- c( mean(block1_choices[,3]), 
             mean(block2_choices[,3]),
             mean(block3_choices[,3]),
             mean(block4_choices[,3]),
             mean(block5_choices[,3]))
             
line2_means <- c( mean(block1_choices[,2]), 
             mean(block2_choices[,2]),
             mean(block3_choices[,2]),
             mean(block4_choices[,2]),
             mean(block5_choices[,2]))
             
line1_means <- c( mean(block1_choices[,1]), 
             mean(block2_choices[,1]),
             mean(block3_choices[,1]),
             mean(block4_choices[,1]),
             mean(block5_choices[,1]))

# standard deviations
line10_sds <- c( sd(block1_choices[,4]), 
             sd(block2_choices[,4]),
             sd(block3_choices[,4]),
             sd(block4_choices[,4]),
             sd(block5_choices[,4]))
             
line3_sds <- c( sd(block1_choices[,3]), 
             sd(block2_choices[,3]),
             sd(block3_choices[,3]),
             sd(block4_choices[,3]),
             sd(block5_choices[,3]))
             
line2_sds <- c( sd(block1_choices[,2]), 
             sd(block2_choices[,2]),
             sd(block3_choices[,2]),
             sd(block4_choices[,2]),
             sd(block5_choices[,2]))
             
line1_sds <- c( sd(block1_choices[,1]), 
             sd(block2_choices[,1]),
             sd(block3_choices[,1]),
             sd(block4_choices[,1]),
             sd(block5_choices[,1]))

# Ns
line10_lengths <- c( length(block1_choices[,4]), 
             length(block2_choices[,4]),
             length(block3_choices[,4]),
             length(block4_choices[,4]),
             length(block5_choices[,4]))
             
line3_lengths <- c( length(block1_choices[,3]), 
             length(block2_choices[,3]),
             length(block3_choices[,3]),
             length(block4_choices[,3]),
             length(block5_choices[,3]))
             
line2_lengths <- c( length(block1_choices[,2]), 
             length(block2_choices[,2]),
             length(block3_choices[,2]),
             length(block4_choices[,2]),
             length(block5_choices[,2]))
             
line1_lengths <- c( length(block1_choices[,1]), 
             length(block2_choices[,1]),
             length(block3_choices[,1]),
             length(block4_choices[,1]),
             length(block5_choices[,1]))
             
line10_ses <- line10_sds/sqrt(line10_lengths)
line3_ses <- line3_sds/sqrt(line3_lengths)
line2_ses <- line2_sds/sqrt(line2_lengths)
line1_ses <- line1_sds/sqrt(line1_lengths)



plot(line10_means/20, type='l', col = 'blue', 
	 ylim=c(0,1), ylab='Proportion of selections', 
	 xlab="Block (20 trials)", main='Door sims', cex.lab= 1.4, cex.axis=1.3, lwd=3)
lines(line3_means/20, col = 'green', lwd=3)
lines(line2_means/20, col = 'yellow', lwd=3)
lines(line1_means/20, col = 'red', lwd=3)

legend(1, 1.0, c("10", "3", "2", "1"), 
	   col = c("blue", "green", "yellow", "red"), 
	   lty = c(1,1,1,1), cex = 1.2)

x <- seq(5)
segments(x, line10_means/20+line10_ses/20, x, line10_means/20-line10_ses/20, col='blue')
segments(x, line3_means/20+line3_ses/20, x, line3_means/20-line3_ses/20, col='green')
segments(x, line2_means/20+line2_ses/20, x, line2_means/20-line2_ses/20, col='yellow')
segments(x, line1_means/20+line1_ses/20, x, line1_means/20-line1_ses/20, col='red')




