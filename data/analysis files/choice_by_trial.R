
library(matrixStats)
###########################################

choice_by_trial <- tapply(maindata$reward, list(maindata$subj, maindata$trial, maindata$reward), length)

choice_by_trial[is.na(choice_by_trial)] <-0

library(matrixStats)

means <- colMeans(choice_by_trial)

sds <- matrix( nrow = 100, ncol= 4)

sds[,1] <- colSds(choice_by_trial[,,1])
sds[,2] <- colSds(choice_by_trial[,,2])
sds[,3] <- colSds(choice_by_trial[,,3])
sds[,4] <- colSds(choice_by_trial[,,4])

ses <- sds/sqrt(length(unique(maindata$subj)))

plot( means[,1], type='l', col='red', ylim=c(0,1) )
lines( means[,2], col='yellow')
lines( means[,3], col='green')
lines( means[,4], col='blue', lty = 2)

