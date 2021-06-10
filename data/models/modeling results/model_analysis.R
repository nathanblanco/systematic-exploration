
setwd('/Users/nathanblanco/Dropbox/Work/current_projects/exploration studies/Baseline/data/models/modeling results')

model.data  <- read.table('aics_children.txt', header=T)

#model.data  <- read.table('aics_adults.txt', header=T)

#model.data  <- read.table('aics_bland.txt', header=T)

#model.data  <- read.table('aics_40_trials_BL.txt', header=T)

#subj RL RL_auto switch switch_prob bias random lag

#hist(model.data$switch_prob)

#subj RL RL_auto switch switch_prob bias random lag Bev Blag


model.data$RL_best <- (model.data$RL < model.data$switch) & (model.data$RL < model.data$bias) & (model.data$RL < model.data$RL_auto) & (model.data$RL < model.data$lag)
model.data$switch_best <- (model.data$switch < model.data$RL) & (model.data$switch < model.data$bias) & (model.data$switch < model.data$RL_auto) & (model.data$switch < model.data$lag)
model.data$auto_best <- (model.data$RL_auto < model.data$RL) & (model.data$RL_auto < model.data$bias) & (model.data$RL_auto < model.data$switch) & (model.data$RL_auto < model.data$lag)
model.data$lag_best <- (model.data$lag < model.data$RL) & (model.data$lag < model.data$bias) & (model.data$lag < model.data$switch) & (model.data$lag < model.data$RL_auto)
model.data$bias_best <- (model.data$bias < model.data$RL) & (model.data$bias < model.data$lag) & (model.data$bias < model.data$switch) & (model.data$bias < model.data$RL_auto)

model.data$lag_over_bias <- model.data$lag < model.data$bias
model.data$lag_over_RL <- model.data$lag < model.data$RL

model.data$lag_best2 <- (model.data$lag < model.data$bias) & (model.data$lag < model.data$RL)
model.data$RL_best2 <- (model.data$RL < model.data$bias) & (model.data$RL < model.data$lag)
model.data$bias_best2 <- (model.data$bias < model.data$lag) & (model.data$bias < model.data$RL)


# remove the children from the novelty condition to only look at baseline

model.data <- model.data[model.data$subj !=1111  ,]
model.data <- model.data[model.data$subj !=1112  ,]
model.data <- model.data[model.data$subj !=1113 ,]
model.data <- model.data[model.data$subj !=1114  ,]
model.data <- model.data[model.data$subj !=1115 ,]

model.data <- model.data[model.data$subj != 20162,]
model.data <- model.data[model.data$subj !=20163,]
model.data <- model.data[model.data$subj !=20164 ,]
model.data <- model.data[model.data$subj !=20169  ,]

model.data <- model.data[model.data$subj !=62036 ,]
model.data <- model.data[model.data$subj !=62037 ,]
model.data <- model.data[model.data$subj !=62038 ,]
model.data <- model.data[model.data$subj !=62039 ,]
model.data <- model.data[model.data$subj !=62040 ,]
model.data <- model.data[model.data$subj !=62041  ,]
model.data <- model.data[model.data$subj !=62042 ,]
model.data <- model.data[model.data$subj !=62043 ,]



length(model.data$subj)

sum(as.numeric(model.data$RL_best))
sum(as.numeric(model.data$switch_best))
sum(as.numeric(model.data$auto_best))
sum(as.numeric(model.data$lag_best))
sum(as.numeric(model.data$bias_best))

sum(as.numeric(model.data$lag_over_bias))

sum(as.numeric(model.data$lag_over_RL))

sum(as.numeric(model.data$RL_best2))
sum(as.numeric(model.data$bias_best2))
sum(as.numeric(model.data$lag_best2))

# to look at lag parameters for only those best fit by lag instead of bias
#model.data <- model.data[model.data$lag_over_bias == T,]

child.bev <- model.data$Bev
child.blag <- model.data$Blag



> length(model.data$subj)
[1] 32
> 
> sum(as.numeric(model.data$RL_best))
[1] 6
> sum(as.numeric(model.data$switch_best))
[1] 2
> sum(as.numeric(model.data$auto_best))
[1] 4
> sum(as.numeric(model.data$lag_best))
[1] 19
> sum(as.numeric(model.data$bias_best))
[1] 1


model.data$RL_nllh <- (model.data$RL-4)/2
model.data$lag_nllh <- (model.data$lag-6)/2

library(extRemes)

for (i in seq(length(model.data[,1]))) {
	x<-lr.test(model.data$RL_nllh[i], model.data$lag_nllh[i],df=1, alpha=0.05)
	print(x)
}
lr.test(sum(model.data$RL_nllh), sum(model.data$lag_nllh), df=length(model.data$RL_nllh), alpha=0.05)

model.data  <- read.table('aics_adults.txt', header=T)


#subj RL RL_auto switch switch_prob bias random lag

#hist(model.data$switch_prob)

#subj RL RL_auto switch switch_prob bias random lag Bev Blag


model.data$RL_best <- (model.data$RL < model.data$switch) & (model.data$RL < model.data$bias) & (model.data$RL < model.data$RL_auto) & (model.data$RL < model.data$lag)
model.data$switch_best <- (model.data$switch < model.data$RL) & (model.data$switch < model.data$bias) & (model.data$switch < model.data$RL_auto) & (model.data$switch < model.data$lag)
model.data$auto_best <- (model.data$RL_auto < model.data$RL) & (model.data$RL_auto < model.data$bias) & (model.data$RL_auto < model.data$switch) & (model.data$RL_auto < model.data$lag)
model.data$lag_best <- (model.data$lag < model.data$RL) & (model.data$lag < model.data$bias) & (model.data$lag < model.data$switch) & (model.data$lag < model.data$RL_auto)
model.data$bias_best <- (model.data$bias < model.data$RL) & (model.data$bias < model.data$lag) & (model.data$bias < model.data$switch) & (model.data$bias < model.data$RL_auto)

model.data$lag_over_bias <- model.data$lag < model.data$bias
model.data$lag_over_RL <- model.data$lag < model.data$RL


model.data$lag_best2 <- (model.data$lag < model.data$bias) & (model.data$lag < model.data$RL)
model.data$RL_best2 <- (model.data$RL < model.data$bias) & (model.data$RL < model.data$lag)
model.data$bias_best2 <- (model.data$bias < model.data$lag) & (model.data$bias < model.data$RL)

# to look at lag parameters for only those best fit by lag instead of bias


adult.bev <- model.data$Bev
adult.blag <- model.data$Blag


length(model.data$subj)

sum(as.numeric(model.data$RL_best))
sum(as.numeric(model.data$switch_best))
sum(as.numeric(model.data$auto_best))
sum(as.numeric(model.data$lag_best))
sum(as.numeric(model.data$bias_best))

sum(as.numeric(model.data$lag_over_bias))
sum(as.numeric(model.data$lag_over_RL))


sum(as.numeric(model.data$RL_best2))
sum(as.numeric(model.data$bias_best2))
sum(as.numeric(model.data$lag_best2))

> sum(as.numeric(model.data$RL_best))
[1] 11
> sum(as.numeric(model.data$switch_best))
[1] 1
> sum(as.numeric(model.data$auto_best))
[1] 0
> sum(as.numeric(model.data$lag_best))
[1] 20
> sum(as.numeric(model.data$bias_best))
[1] 2


model.data$RL_nllh <- (model.data$RL-4)/2
model.data$lag_nllh <- (model.data$lag-6)/2

library(extRemes)

for (i in seq(length(model.data[,1]))) {
	x<-lr.test(model.data$RL_nllh[i], model.data$lag_nllh[i],df=1, alpha=0.05)
	print(x)
}
lr.test(sum(model.data$RL_nllh), sum(model.data$lag_nllh), df=length(model.data$RL_nllh), alpha=0.05)


model.data  <- read.table('aics_bland.txt', header=T)


#subj RL RL_auto switch switch_prob bias random lag

#hist(model.data$switch_prob)

#subj RL RL_auto switch switch_prob bias random lag Bev Blag


model.data$RL_best <- (model.data$RL < model.data$switch) & (model.data$RL < model.data$bias) & (model.data$RL < model.data$RL_auto) & (model.data$RL < model.data$lag)
model.data$switch_best <- (model.data$switch < model.data$RL) & (model.data$switch < model.data$bias) & (model.data$switch < model.data$RL_auto) & (model.data$switch < model.data$lag)
model.data$auto_best <- (model.data$RL_auto < model.data$RL) & (model.data$RL_auto < model.data$bias) & (model.data$RL_auto < model.data$switch) & (model.data$RL_auto < model.data$lag)
model.data$lag_best <- (model.data$lag < model.data$RL) & (model.data$lag < model.data$bias) & (model.data$lag < model.data$switch) & (model.data$lag < model.data$RL_auto)
model.data$bias_best <- (model.data$bias < model.data$RL) & (model.data$bias < model.data$lag) & (model.data$bias < model.data$switch) & (model.data$bias < model.data$RL_auto)


model.data$lag_over_bias <- model.data$lag < model.data$bias
model.data$lag_over_RL <- model.data$lag < model.data$RL

model.data$lag_best2 <- (model.data$lag < model.data$bias) & (model.data$lag < model.data$RL)
model.data$RL_best2 <- (model.data$RL < model.data$bias) & (model.data$RL < model.data$lag)
model.data$bias_best2 <- (model.data$bias < model.data$lag) & (model.data$bias < model.data$RL)


# to look at lag parameters for only those best fit by lag instead of bias
#model.data <- model.data[model.data$lag_over_bias == T,]

bland.bev <- model.data$Bev
bland.blag <- model.data$Blag


length(model.data$subj)

sum(as.numeric(model.data$RL_best))
sum(as.numeric(model.data$switch_best))
sum(as.numeric(model.data$auto_best))
sum(as.numeric(model.data$lag_best))
sum(as.numeric(model.data$bias_best))


sum(as.numeric(model.data$lag_over_bias))

sum(as.numeric(model.data$RL_best2))
sum(as.numeric(model.data$bias_best2))
sum(as.numeric(model.data$lag_best2))

> sum(as.numeric(model.data$RL_best))
[1] 8
> sum(as.numeric(model.data$switch_best))
[1] 5
> sum(as.numeric(model.data$auto_best))
[1] 6
> sum(as.numeric(model.data$lag_best))
[1] 4
> sum(as.numeric(model.data$bias_best))
[1] 2



t.test(child.bev, adult.bev, var.equal=T)
t.test(child.blag, adult.blag, var.equal=T)

t.test(child.bev, bland.bev, var.equal=T)
t.test(child.blag, bland.blag, var.equal=T)


child.ratio <- child.bev/child.blag
adult.ratio <- adult.bev/adult.blag
bland.ratio <- bland.bev/bland.blag

par(mfrow=c(3,1))
hist(child.ratio)
hist(adult.ratio)
hist(bland.ratio)



wilcox.test(child.bev , adult.bev, correct=F)
wilcox.test(child.blag , adult.blag, correct=F)

wilcox.test(child.bev , bland.bev, correct=F)
wilcox.test(child.blag , bland.blag, correct=F)

wilcox.test(child.ratio , adult.ratio, correct=F)
wilcox.test(child.ratio , bland.ratio, correct=F)

wilcox.test(adult.ratio , bland.ratio, correct=F)


> median(child.ratio)
[1] 1.298401
> median(adult.ratio)
[1] 12.66289
> median(bland.ratio)
[1] 56.53115



wilcox.test(adult.bev , bland.bev, correct=F)
wilcox.test(adult.blag , bland.blag, correct=F)


############################################################

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

par(mfrow=c(2,1))
hist(child.data$ebm_weight, xlim=c(0,1), ylim=c(0,20), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), main='Children', xlab="Phi", col = 'blue', density=45, border='black')
hist(adult.data$ebm_weight, xlim=c(0,1), ylim=c(0,20), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), main='Adults', xlab='Phi', col = 'green', density=45, border='black')




ylim=c(0,20), 
ylim=c(0,20),
breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),

par(mfrow=c(2,1))
hist(log(child.data$ebm_Bev), xlim=c(-4,10), ylim=c(0,20),  main='Children', xlab="log(Beta)", col = 'blue', density=45, border='black')
hist(log(adult.data$ebm_Bev), xlim=c(-4,10), ylim=c(0,20),  main='Adults', xlab='log(Beta)', col = 'green', density=45, border='black')



child.data <- read.table('EBM_params_children.txt',header=T)
adult.data <- read.table('EBM_params_adults.txt',header=T)


par(mfrow=c(2,1))
hist(child.data$ebm_alpha, xlim=c(0,1), ylim=c(0,15), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), main='Children', xlab="Alpha", col = 'blue', density=45, border='black')
hist(adult.data$ebm_alpha, xlim=c(0,1), ylim=c(0,15), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), main='Adults', xlab='Alpha', col = 'green', density=45, border='black')





hist(bland.data$ebm_weight, xlim=c(0,1), ylim=c(0,25), breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), main='Children: exposed rewards', xlab='Phi', col = 'grey')

wilcox.test(child.data$ebm_weight , adult.data$ebm_weight, correct=F)
wilcox.test(child.data$ebm_weight , bland.data$ebm_weight, correct=F)
wilcox.test(bland.data$ebm_weight , adult.data$ebm_weight, correct=F)


wilcox.test(child.data$ebm_Bev , adult.data$ebm_Bev, correct=F)
wilcox.test(child.data$ebm_Bev , bland.data$ebm_Bev, correct=F)
wilcox.test(bland.data$ebm_Bev , adult.data$ebm_Bev, correct=F)


median(child.data$ebm_Bev)
median(adult.data$ebm_Bev)
median(bland.data$ebm_Bev)

##################

par(mfrow=c(3,1))
barplot(child.data$ebm_weight[order(child.data$ebm_weight)], ylim=c(0,1), main='Children')
barplot(adult.data$ebm_weight[order(adult.data$ebm_weight)], ylim=c(0,1), main='Adults')
barplot(bland.data$ebm_weight[order(bland.data$ebm_weight)], ylim=c(0,1), main='Exposed Rewards')



barplot(c(0,0,child.data$ebm_weight[order(child.data$ebm_weight)]), ylim=c(0,1), col='blue', main='Directed exploration', ylab='phi parameter')
barplot(adult.data$ebm_weight[order(adult.data$ebm_weight)],  col = 'green', xlab="Individual participants", add =T)


barplot(c(0,0,child.data$ebm_weight[order(child.data$ebm_weight)]), ylim=c(0,1), col='blue', 
main='Directed exploration', ylab='phi parameter', density = 30)

barplot(adult.data$ebm_weight[order(adult.data$ebm_weight)],  col = 'green', xlab="Individual participants", ylim=c(0,1))


boxplot(child.data$eb_Bev, adult.data$eb_Bev,
		main ="Choice consistency", names.arg = c("Children", "Adults"),
		ylim=c(0,10),
		ylab="Beta parameter",
		 cex.axis=1.5, cex.lab=1.5)



boxplot(child.data$eb_Blag, adult.data$eb_Blag,
		main ="Exploration bonus", names.arg = c("Children", "Adults"),
		ylim=c(0,10),
		ylab="Phi parameter",
		 cex.axis=1.5, cex.lab=1.5)


############################################################

child.data <- child.data[order(child.data$subj),]

child_age_data <- read.table('AGE_MONTHS.TXT', header=T)
child_age_data <- child_age_data[order(child_age_data$subj),]


cor.test(child.data$ebm_weight, child_age_data$months)


x <- lm(child.data$ebm_weight~child_age_data$months)
summary(x)

x <- glm(child.data$ebm_weight~child_age_data$months, family = 'binomial')
summary(x)


model.data <- model.data[order(model.data$subj),]



x <- glm(model.data$RL_best2~child_age_data$months, family = 'binomial')
summary(x)

x <- glm(as.numeric(model.data$RL_best2)~child_age_data$months, family = 'binomial')
summary(x)



#####################################################

# ROTATION MODELS ANALYSIS

children.data <- read.table('aics_children_rotate.txt', header=T)

children.data$lag_best <- as.numeric( (children.data$Lag < children.data$CW) & (children.data$Lag < children.data$CCW) )


children.data <- children.data[children.data$subj != 20162,]
children.data <- children.data[children.data$subj !=20163,]
children.data <- children.data[children.data$subj !=20164 ,]
children.data <- children.data[children.data$subj !=20169  ,]
children.data <- children.data[children.data$subj !=62036 ,]
children.data <- children.data[children.data$subj !=62037 ,]
children.data <- children.data[children.data$subj !=62038 ,]
children.data <- children.data[children.data$subj !=62039 ,]
children.data <- children.data[children.data$subj !=62040 ,]
children.data <- children.data[children.data$subj !=1111  ,]
children.data <- children.data[children.data$subj !=1112  ,]
children.data <- children.data[children.data$subj !=1113 ,]
children.data <- children.data[children.data$subj !=1114  ,]
children.data <- children.data[children.data$subj !=1115 ,]
children.data <- children.data[children.data$subj !=62041  ,]
children.data <- children.data[children.data$subj !=62042 ,]
children.data <- children.data[children.data$subj !=62043 ,]

