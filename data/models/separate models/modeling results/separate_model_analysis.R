
setwd('/Users/nathanblanco/Dropbox/Work/current_projects/exploration studies/Baseline/data/models/separate models/modeling results')

#model.data  <- read.table('separate_model_fits-bland.txt', header=T)
model.data  <- read.table('separate_model_fits-kids.txt', header=T)
#model.data  <- read.table('separate_model_fits-adults.txt', header=T)

#subj RL RL_auto switch switch_prob bias random lag

#hist(model.data$switch_prob)

#subj RL RL_auto switch switch_prob bias random lag Bev Blag



model.data$RL_best <- (model.data$RL_llh < model.data$Lag_llh) 
					  
model.data$Lag_best <- (model.data$Lag_llh < model.data$RL_llh) 
					  

					  

sum(as.numeric(model.data$RL_best))
sum(as.numeric(model.data$Lag_best))



model.data$RL_aic <- (model.data$RL_llh * 2) + 4
model.data$Lag_aic <- (model.data$Lag_llh * 2) + 2
model.data$random_aic <- -log(0.25)*100*2


model.data$RL_best <- (model.data$RL_aic < model.data$Lag_aic) &  
					  (model.data$RL_aic < model.data$random_aic)
					  
model.data$Lag_best <- (model.data$Lag_aic < model.data$RL_aic) & 
					  (model.data$Lag_aic < model.data$random_aic) 
					  

model.data$random_best <- (model.data$random_aic < model.data$RL_aic) & 
						(model.data$random_aic < model.data$Lag_aic) 
						



sum(as.numeric(model.data$RL_best))
sum(as.numeric(model.data$Lag_best))
sum(as.numeric(model.data$random_best))

unique(model.data[model.data$RL_best==TRUE,]$subj)
unique(model.data[model.data$Lag_best==TRUE,]$subj)
unique(model.data[model.data$random_best==TRUE,]$subj)



######################

child.data  <- read.table('separate_model_fits-kids.txt', header=T)
adult.data  <- read.table('separate_model_fits-adults.txt', header=T)

par(mfrow=c(2,1))
hist(child.data$B_value, xlim=c(0,1), ylim=c(0,25),  main='Children', xlab="B_value", col = 'blue', density=45, border='black')
hist(adult.data$B_value, xlim=c(0,1), ylim=c(0,25),  main='Adults', xlab='B_value', col = 'green', density=45, border='black')


 xlim=c(0,1),
par(mfrow=c(2,1))
hist(child.data$B_lag,  xlim=c(0,2), ylim=c(0,35), main='Children', breaks=c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0),
xlab="B_lag", col = 'blue', density=45, border='black')

hist(adult.data$B_lag, xlim=c(0,2), ylim=c(0,35), breaks=c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0),
main='Adults', xlab='B_lag', col = 'green', density=45, border='black')

###########################
child.data  <- read.table('separate_model_fits-door.txt', header=T)
adult.data  <- read.table('separate_model_fits-door-adults.txt', header=T)

par(mfrow=c(2,1))
hist(child.data$B_value, xlim=c(0,1), ylim=c(0,20),  main='Children', breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,200),
xlab="B_value", col = 'blue', density=45, border='black')
hist(adult.data$B_value, xlim=c(0,1), ylim=c(0,20),  main='Adults',  breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,200), xlab='B_value', col = 'green', density=45, border='black')

par(mfrow=c(2,1))
hist(child.data$B_door, main='Children', xlab="B_value", col = 'blue', density=45, border='black')
hist(adult.data$B_door, main='Adults', xlab='B_value', col = 'green', density=45, border='black')


child.data$door_best <- as.numeric((child.data$Door_llh < child.data$Lag_llh) & (child.data$Door_llh < child.data$RL_llh))

door_best_data <- child.data[child.data$door_best == 1,]

hidden_approachers <- door_best_data[door_best_data$B_door > 0,]$subj
hidden_avoiders <- door_best_data[door_best_data$B_door < 0,]$subj

# 3097 is a best fit by random
hidden_approachers <- hidden_approachers[hidden_approachers != 3097]

#3094 8121 3096 3099 3103 3104 8124 8125 2109 2110 2112 2116 2117


#3093 3095 3102 8126 8127 8128 8129 2111 2113 2114 2115




#alldata <- read.table('door_data_adults.txt', header=F)
alldata <- read.table('door_data.txt', header=F)

#self.subj, 'main', self.trial, self.cond, res, rt, payoff, self.earnings, str(self.rewards[int(res)])+'candy'+self.candy_types[int(res)]+'-bland.png', door_index, chose_door, self.rewards])


names(alldata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'door_loc', 'door_chose', 'r1', 'r2', 'r3', 'r4')




######################
choices <- tapply(alldata$reward, list(alldata$subj, alldata$reward), length)
choice_props <- choices[,4]/100
t.test(choice_props, mu=0.25)

approach.data <- alldata[alldata$subj %in% hidden_approachers,]
avoid.data <- alldata[alldata$subj %in% hidden_avoiders,]


app_choices <- tapply(approach.data$reward, list(approach.data$subj, approach.data$reward), length)
app_props <- app_choices[,4]/100

av_choices <- tapply(avoid.data$reward, list(avoid.data$subj, avoid.data$reward), length)
av_props <- av_choices[,4]/100

mean(app_props)
mean(av_props)


non.data <- alldata[!(alldata$subj %in% hidden_approachers),]
non.data <- non.data[!(non.data$subj %in% hidden_avoiders),]
non.data <- non.data[non.data$subj != 3097,]

non_choices <- tapply(non.data$reward, list(non.data$subj, non.data$reward), length)
non_props <- non_choices[,4]/100

non_choices[is.na(non_choices)] <- 0

t.test(av_props, app_props, var.equal=T)

t.test(av_props, non_props, var.equal=T)

t.test(non_props, app_props, var.equal=T)


props <- c(app_props, av_props, non_props)
cond <- as.factor(c( rep(0, length(app_props)),
		   rep(1, length(app_props)),
		   rep(2, length(app_props))
          ))
          
results <- as.data.frame(cbind(props, cond))

x <- aov(props~cond, data=results)