

setwd('/Users/nathanblanco/Dropbox/Work/current_projects/exploration studies/Baseline/data/models/modeling results')

child.data <- read.table('EBM_aics_children.txt', header=T)
adult.data <- read.table('EBM_aics_adults.txt', header=T)
bland.data <- read.table('EBM_aics_bland.txt', header=T)


child.data <- child.data[child.data$subj != 20162,]
child.data <- child.data[child.data$subj !=20163,]
child.data <- child.data[child.data$subj !=20164 ,]
child.data <- child.data[child.data$subj !=20169  ,]
child.data <- child.data[child.data$subj !=62036 ,]
child.data <- child.data[child.data$subj !=62037 ,]
child.data <- child.data[child.data$subj !=62038 ,]
child.data <- child.data[child.data$subj !=62039 ,]
child.data <- child.data[child.data$subj !=62040 ,]
child.data <- child.data[child.data$subj !=1111  ,]
child.data <- child.data[child.data$subj !=1112  ,]
child.data <- child.data[child.data$subj !=1113 ,]
child.data <- child.data[child.data$subj !=1114  ,]
child.data <- child.data[child.data$subj !=1115 ,]
child.data <- child.data[child.data$subj !=62041  ,]
child.data <- child.data[child.data$subj !=62042 ,]
child.data <- child.data[child.data$subj !=62043 ,]

median(child.data$eb_Blag)
median(adult.data$eb_Blag)
median(bland.data$eb_Blag)

par(mfrow=c(3,1))
hist(child.data$eb_Blag)
hist(adult.data$eb_Blag)
hist(bland.data$eb_Blag)

wilcox.test(child.data$eb_Blag , adult.data$eb_Blag, correct=F)
wilcox.test(child.data$eb_Blag , bland.data$eb_Blag, correct=F)
wilcox.test(adult.data$eb_Blag , bland.data$eb_Blag, correct=F)

median(child.data$eb_Bev)
median(adult.data$eb_Bev)
median(bland.data$eb_Bev)

par(mfrow=c(3,1))
hist(child.data$eb_Bev)
hist(adult.data$eb_Bev)
hist(bland.data$eb_Bev)

wilcox.test(child.data$eb_Bev , adult.data$eb_Bev, correct=F)
wilcox.test(child.data$eb_Bev , bland.data$eb_Bev, correct=F)
wilcox.test(adult.data$eb_Bev , bland.data$eb_Bev, correct=F)

child.ratio <- child.data$eb_Bev/child.data$eb_Blag
adult.ratio <- adult.data$eb_Bev/adult.data$eb_Blag

median(child.ratio)
median(adult.ratio)

wilcox.test(child.ratio, adult.ratio, correct=F)

########
median(child.data$ebm_weight)
median(adult.data$ebm_weight)
median(bland.data$ebm_weight)

mean(child.data$ebm_weight)
mean(adult.data$ebm_weight)
mean(bland.data$ebm_weight)


child.data <- read.table('EBM_params_children.txt',header=T)
adult.data <- read.table('EBM_params_adults.txt',header=T)
par(mfrow=c(2,3))


hist(child.data$ebm_alpha, xlim=c(0,1), ylim=c(0,15), 
    breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
    main='Children', xlab="Alpha", col = 'blue', 
    density=45, border='black', cex.lab=1.3, cex.axis=1.3)
abline(v=mean(child.data$ebm_alpha), lty=2, lwd=2)
abline(v=median(child.data$ebm_alpha), lty=1, lwd=2)

hist(log(child.data$ebm_Bev), xlim=c(-4,10), ylim=c(0,20), 
     main='Children', xlab="log(Beta)", col = 'blue', 
     density=45, border='black', cex.lab=1.3, cex.axis=1.3)
abline(v=mean(log(child.data$ebm_Bev)), lty=2, lwd=2)
abline(v=median(log(child.data$ebm_Bev)), lty=1, lwd=2)

hist(child.data$ebm_weight, xlim=c(0,1), ylim=c(0,20), 
     breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
     main='Children', xlab="Phi", col = 'blue', 
     density=45, border='black', cex.lab=1.3, cex.axis=1.3)
abline(v=mean(child.data$ebm_weight), lty=2, lwd=2)
abline(v=median(child.data$ebm_weight), lty=1, lwd=2)

hist(adult.data$ebm_alpha, xlim=c(0,1), ylim=c(0,15), 
     breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
     main='Adults', xlab='Alpha', col = 'green', 
     density=45, border='black', cex.lab=1.3, cex.axis=1.3)
abline(v=mean(adult.data$ebm_alpha), lty=2, lwd=2)
abline(v=median(adult.data$ebm_alpha), lty=1, lwd=2)

hist(log(adult.data$ebm_Bev), xlim=c(-4,10), ylim=c(0,20),  
     main='Adults', xlab='log(Beta)', col = 'green', 
     density=45, border='black', cex.lab=1.3, cex.axis=1.3)
abline(v=mean(log(adult.data$ebm_Bev)), lty=2, lwd=2)
abline(v=median(log(adult.data$ebm_Bev)), lty=1, lwd=2)

hist(adult.data$ebm_weight, xlim=c(0,1), ylim=c(0,20), 
     breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
     main='Adults', xlab='Phi', col = 'green', 
     density=45, border='black', cex.lab=1.3, cex.axis=1.3)
abline(v=mean(adult.data$ebm_weight), lty=2, lwd=2)
abline(v=median(adult.data$ebm_weight), lty=1, lwd=2)

wilcox.test(child.data$ebm_alpha, adult.data$ebm_alpha, correct=F)
wilcox.test(child.data$ebm_Bev, adult.data$ebm_Bev, correct=F)
wilcox.test(child.data$ebm_weight, adult.data$ebm_weight, correct=F)


median(child.data$ebm_alpha)
median(adult.data$ebm_alpha)

mean(child.data$ebm_alpha)
mean(adult.data$ebm_alpha)

median(child.data$ebm_Bev)
median(adult.data$ebm_Bev)

mean(child.data$ebm_Bev)
mean(adult.data$ebm_Bev)

median(child.data$ebm_weight)
median(adult.data$ebm_weight)

mean(child.data$ebm_weight)
mean(adult.data$ebm_weight)



child.data <- read.table('RL_params_children.txt',header=T)
adult.data <- read.table('RL_params_adults.txt',header=T)
par(mfrow=c(2,3))

hist(child.data$RL_alpha, xlim=c(0,1), ylim=c(0,15), 
     breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
     main='Children', xlab="Alpha", col = 'blue', 
     density=45, border='black', cex.lab=1.3, cex.axis=1.3)
abline(v=mean(child.data$RL_alpha), lty=2, lwd=2)
abline(v=median(child.data$RL_alpha), lty=1, lwd=2)

hist(log(child.data$RL_Bev),  ylim=c(0,30), xlim=c(-40,10),
     main='Children', xlab="log(Beta)", col = 'blue', 
     breaks=c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10),
     density=45, border='black', cex.lab=1.3, cex.axis=1.3)
abline(v=mean(log(child.data$RL_Bev)), lty=2, lwd=2)
abline(v=median(log(child.data$RL_Bev)), lty=1, lwd=2)

plot(NULL)

hist(adult.data$RL_alpha, xlim=c(0,1), ylim=c(0,15), 
     breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
     main='Adults', xlab='Alpha', col = 'green', 
     density=45, border='black', cex.lab=1.3, cex.axis=1.3)
abline(v=mean(adult.data$RL_alpha), lty=2, lwd=2)
abline(v=median(adult.data$RL_alpha), lty=1, lwd=2)

hist(log(adult.data$RL_Bev),  ylim=c(0,30),  xlim=c(-40,10),
     main='Adults', xlab='log(Beta)', col = 'green', 
     breaks=c(-40,-35,-30,-25,-20,-15,-10,-5,0,5,10),
     density=45, border='black', cex.lab=1.3, cex.axis=1.3)
abline(v=mean(log(adult.data$RL_Bev)), lty=2, lwd=2)
abline(v=median(log(adult.data$RL_Bev)), lty=1, lwd=2)

wilcox.test(child.data$RL_alpha, adult.data$RL_alpha, correct=F)
wilcox.test(child.data$RL_Bev, adult.data$RL_Bev, correct=F)


# mean is dashed, median is solid

