kids <- read.table('mixture1_novelty_children.txt', header=F)
adults <- read.table('mixture1_novelty_adults.txt', header=F)

names(kids) <- c('subj', 'aic', 'param1', 'param2', 'param3', 'mixture')
names(adults) <- c('subj', 'aic', 'param1', 'param2', 'param3', 'mixture')

par(mfrow=c(2,1))
hist(kids$mixture, xlim=c(0,1), main="Children", xlab="mixture param")
hist(adults$mixture, xlim=c(0,1), main="Adults", xlab="mixture param")