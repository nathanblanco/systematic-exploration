


library(matrixStats)


kiddata <- read.table('door_data.txt', header=F)

#self.subj, 'main', self.trial, self.cond, res, rt, payoff, self.earnings, str(self.rewards[int(res)])+'candy'+self.candy_types[int(res)]+'-bland.png', door_index, chose_door, self.rewards])


names(kiddata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'door_loc', 'door_chose', 'r1', 'r2', 'r3', 'r4')

kiddata$block <- 0

kiddata[kiddata$trial <= 20,]$block <- 1
kiddata[kiddata$trial > 20 & kiddata$trial <= 40,]$block <- 2
kiddata[kiddata$trial > 40 & kiddata$trial <= 60,]$block <- 3
kiddata[kiddata$trial > 60 & kiddata$trial <= 80,]$block <- 4
kiddata[kiddata$trial > 80 & kiddata$trial <= 100,]$block <- 5



adultdata <- read.table('door_data_adults.txt', header=F)

#self.subj, 'main', self.trial, self.cond, res, rt, payoff, self.earnings, str(self.rewards[int(res)])+'candy'+self.candy_types[int(res)]+'-bland.png', door_index, chose_door, self.rewards])


names(adultdata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'door_loc', 'door_chose', 'r1', 'r2', 'r3', 'r4')

adultdata$block <- 0

adultdata[adultdata$trial <= 20,]$block <- 1
adultdata[adultdata$trial > 20 & adultdata$trial <= 40,]$block <- 2
adultdata[adultdata$trial > 40 & adultdata$trial <= 60,]$block <- 3
adultdata[adultdata$trial > 60 & adultdata$trial <= 80,]$block <- 4
adultdata[adultdata$trial > 80 & adultdata$trial <= 100,]$block <- 5


##############################################

kid_door_choices <- tapply(kiddata$door_chose, list(kiddata$subj, kiddata$block), mean)
adult_door_choices <- tapply(adultdata$door_chose, list(adultdata$subj, adultdata$block), mean)

kid_means <- apply(kid_door_choices,2,mean)
adult_means <- apply(adult_door_choices,2,mean)

kid_sds <- apply(kid_door_choices,2,sd)
adult_sds <- apply(adult_door_choices,2,sd)

kid_lengths <- apply(kid_door_choices,2,length)
adult_lengths <- apply(adult_door_choices,2,length)

kid_ses <- kid_sds/sqrt(kid_lengths)
adult_ses <- adult_sds/sqrt(adult_lengths)

plot(kid_means, col='purple', ylim= c(0,1), ylab = 'Proportion covered option chosen',
		xlab="Block (20 trials)", cex.lab= 1.4, cex.axis=1.3, lwd=3, type='l')
lines(adult_means, col='orange', lwd=3)
x = seq(5)
segments(x, kid_means+kid_ses, x, kid_means-kid_ses, col='purple')
segments(x, adult_means+adult_ses, x, adult_means-adult_ses, col='orange')

abline(0.25, 0, col='red', lty=2) 


legend(1, 1.0, c("Children", "Adults"), 
	   col = c("purple", "orange"), 
	   lty = c(1,1), cex = 1.2)