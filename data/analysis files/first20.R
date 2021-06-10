

child_first20 <- child_data[child_data$trial <= 20,]
adult_first20 <- adult_data[adult_data$trial <= 20,]



library(matrixStats)
###########################################



library(matrixStats)

par(mfrow=c(1,2))

child_choice_by_trial <- tapply(child_first20$reward, list(child_first20$subj, child_first20$trial, child_first20$reward), length)

child_choice_by_trial[is.na(child_choice_by_trial)] <-0



child_means <- colMeans(child_choice_by_trial)

child_sds <- matrix( nrow = 100, ncol= 4)

child_sds[,1] <- colSds(child_choice_by_trial[,,1])
child_sds[,2] <- colSds(child_choice_by_trial[,,2])
child_sds[,3] <- colSds(child_choice_by_trial[,,3])
child_sds[,4] <- colSds(child_choice_by_trial[,,4])

child_ses <- child_sds/sqrt(length(unique(child_first20$subj)))

x <- seq(20)
plot( child_means[,1], type='l', col='red', ylim=c(0,1), xlab='Trial', ylab='Proportion of selections', main='Children' )
lines( child_means[,2], col='yellow')
lines( child_means[,3], col='green')
lines( child_means[,4], col='blue')

segments(x, child_means[,1]+child_ses[,1], x, child_means[,1]-child_ses[,1], col='red' )
segments(x, child_means[,2]+child_ses[,2], x, child_means[,2]-child_ses[,2], col='yellow' )
segments(x, child_means[,3]+child_ses[,3], x, child_means[,3]-child_ses[,3], col='green' )
segments(x, child_means[,4]+child_ses[,4], x, child_means[,4]-child_ses[,4], col='blue' )



adult_choice_by_trial <- tapply(adult_first20$reward, list(adult_first20$subj, adult_first20$trial, adult_first20$reward), length)

adult_choice_by_trial[is.na(adult_choice_by_trial)] <-0

adult_means <- colMeans(adult_choice_by_trial)

adult_sds <- matrix( nrow = 100, ncol= 4)

adult_sds[,1] <- colSds(adult_choice_by_trial[,,1])
adult_sds[,2] <- colSds(adult_choice_by_trial[,,2])
adult_sds[,3] <- colSds(adult_choice_by_trial[,,3])
adult_sds[,4] <- colSds(adult_choice_by_trial[,,4])

adult_ses <- adult_sds/sqrt(length(unique(adult_first20$subj)))

plot( adult_means[,1], type='l', col='red', ylim=c(0,1), xlab='Trial', ylab='Proportion of selections', main='Adults'  )
lines( adult_means[,2], col='yellow')
lines( adult_means[,3], col='green')
lines( adult_means[,4], col='blue')


segments(x, adult_means[,1]+adult_ses[,1], x, adult_means[,1]-adult_ses[,1], col='red' )
segments(x, adult_means[,2]+adult_ses[,2], x, adult_means[,2]-adult_ses[,2], col='yellow' )
segments(x, adult_means[,3]+adult_ses[,3], x, adult_means[,3]-adult_ses[,3], col='green' )
segments(x, adult_means[,4]+adult_ses[,4], x, adult_means[,4]-adult_ses[,4], col='blue' )




###########
child_overall_means <- apply(child_choice_by_trial[,,4],1,mean)
child_overall_means

adult_overall_means <- apply(adult_choice_by_trial[,,4],1,mean)
adult_overall_means

t.test(child_overall_means, adult_overall_means, var.equal=T)


BASELINE ------------------------------------------------------------

	Two Sample t-test

data:  child_overall_means and adult_overall_means
t = -13.108, df = 64, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.5029072 -0.3698869
sample estimates:
mean of x mean of y 
0.3062500 0.7426471 




DOOR------------------------------------------------------------------

	Two Sample t-test

data:  child_overall_means and adult_overall_means
t = -13.844, df = 71, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.5306582 -0.3970445
sample estimates:
mean of x mean of y 
0.3875000 0.8513514 