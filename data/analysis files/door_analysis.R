
setwd("/Users/nathanblanco/Dropbox/Work/current_projects/exploration studies/Baseline/data")

alldata <- read.table('door_data_adults.txt', header=F)
#alldata <- read.table('door_data_children.txt', header=F)

#self.subj, 'main', self.trial, self.cond, res, rt, payoff, self.earnings, str(self.rewards[int(res)])+'candy'+self.candy_types[int(res)]+'-bland.png', door_index, chose_door, self.rewards])


names(alldata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'door_loc', 'door_chose', 'r1', 'r2', 'r3', 'r4')

alldata$door_reward
for (i in 1:length(alldata[,1])) {
	
	alldata$door_reward[i] <- alldata[i,12+alldata$door_loc[i]]
	
}


alldata$stay <- NULL
for (i in 1:length(alldata[,1])) {
    alldata$stay[i] <- as.numeric(alldata[i,]$res == alldata[i-1,]$res)
    
    if (alldata[i,]$trial == 1) { alldata$stay[i] <- 0 }
    
    }

stay_total <- tapply(alldata$stay, alldata$subj, sum)

for (i in 1:length(stay_total)) {
	x <- binom.test(stay_total[i], 100, 0.25)
	print(x)
	
}

for (i in seq(100)) {
	x <- binom.test(i, 100, 0.25)
	print(x)
	
}

65-83

#binom.test(x, n, p = 0.5,
#           alternative = c("two.sided", "less", "greater"),
#           conf.level = 0.95)




stay_props <- tapply(alldata$stay, alldata$subj, mean)

hist(stay_props, main= "Children:  Stay proportions", xlab="Proportion of repeat responses", col='grey')

barplot(stay_props[order(stay_props)],
		main ='',
		ylim=c(0,1),
		ylab="Proportion of repeat responses",
		xlab="Participant")


barplot(1-stay_props[order(stay_props)],
		main ="Childrens's Switch Responses",
		ylim=c(0,1),
		ylab="Proportion of switch responses",
		xlab="Participant", cex.axis=1.5, cex.lab=1.5)
abline(0.65, 0, col='red')
abline(0.83, 0, col='red')

stay_props_byoptions <- tapply(alldata$stay, list(alldata$reward, alldata$subj), mean)

# only for baseline kids because of one kid that never chose 2 option
#stay_props_byoptions[2,5] <- 0


means <- c( mean(stay_props_byoptions[1,], na.rm=T),
			mean(stay_props_byoptions[2,] , na.rm=T),
			mean(stay_props_byoptions[3,] , na.rm=T), 
			mean(stay_props_byoptions[4,] , na.rm=T))
			
sds <- c( sd(stay_props_byoptions[1,], na.rm=T),
			sd(stay_props_byoptions[2,], na.rm=T),
			sd(stay_props_byoptions[3,], na.rm=T),
			sd(stay_props_byoptions[4,], na.rm=T))

ns <- c( length(stay_props_byoptions[1,]),
			length(stay_props_byoptions[2,]),
			length(stay_props_byoptions[3,]),
			length(stay_props_byoptions[4,]))
			
ses <- sds/sqrt(ns)

x <- barplot(means, names.arg = c("1", "2", "3", "10"),
			 main = "Creature Novelty: children",
			 xlab = "option",
			 ylab = "stay proportion",
			 ylim = c(0,1))
segments(x, means+ses, x, means-ses)

######################
choices <- tapply(alldata$reward, list(alldata$subj, alldata$reward), length)
choice_props <- choices[,4]/100
t.test(choice_props, mu=0.25)

#####


##########################







t.test(choice_props_adults, choice_props_kids, var.equal=T)

d.t.unpaired<-function(t.val,n1,n2){
  d<-t.val*sqrt((n1+n2)/(n1*n2))
  names(d)<-"effect size d"
  return(d)
}


block1 <- alldata[alldata$trial <= 20,]
block2 <- alldata[alldata$trial > 20 & alldata$trial <= 40,]
block3 <- alldata[alldata$trial > 40 & alldata$trial <= 60,]
block4 <- alldata[alldata$trial > 60 & alldata$trial <= 80,]
block5 <- alldata[alldata$trial > 80 & alldata$trial <= 100,]


block1_choices <- tapply(block1$reward, list(block1$subj, block1$reward), length)
block1_choices[is.na(block1_choices)] <- 0

block2_choices <- tapply(block2$reward, list(block2$subj, block2$reward), length)
block2_choices[is.na(block2_choices)] <- 0

block3_choices <- tapply(block3$reward, list(block3$subj, block3$reward), length)
block3_choices[is.na(block3_choices)] <- 0

block4_choices <- tapply(block4$reward, list(block4$subj, block4$reward), length)
block4_choices[is.na(block4_choices)] <- 0

block5_choices <- tapply(block5$reward, list(block5$subj, block5$reward), length)
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
	 xlab="Block (20 trials)", main='', cex.lab= 1.4, cex.axis=1.3, lwd=3)
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

####################################################################

choice_props_door <- tapply(alldata$reward, list(alldata$subj, alldata$reward, alldata$door_chose), length)
choice_props_door[is.na(choice_props_door)] <- 0

#choice_props_door <- choice_props_door/100
choice_props_door[,,1] <- choice_props_door[,,1]/75
choice_props_door[,,2] <- choice_props_door[,,2]/25

means <- apply(choice_props_door, c(2,3), mean)
sds <- apply(choice_props_door, c(2,3), sd)
Ns <- apply(choice_props_door, c(2,3), length)

ses <- sds/sqrt(Ns)

x <- barplot(t(means), beside=T, xlab= "Value", ylab="Probability of choosing option",
			 legend.text= c("Exposed", "Covered"), ylim=c(0,1.0
			 ))
segments(x,t(means)+t(ses), x, t(means)-t(ses))


#weighted_means <- c(means[,1]/0.75, means[,2]/0.25)
#weighted_means2 <- t(matrix(weighted_means, 4))

#x <- barplot(weighted_means2, beside=T, xlab= "Value", ylab="Weighted Mean proportions",
#			 legend.text= c("Exposed", "Covered"), ylim=c(0,1.0),
#			 names.arg=c('1','2','3','4'))
#segments(x,weighted_means2+t(ses), x, weighted_means2-t(ses))

#####################################################################





#####################################################################

door_choices <- tapply(alldata$door_chose, alldata$subj, mean)

t.test(door_choices, mu=0.25)




barplot(door_choices[order(door_choices)], ylim=c(0,1), xlab="Individual Participant", ylab = "Door chosen proportion")

abline(0.165, 0, col='red')
abline(0.34, 0, col='red')

abline(0.5, 0, col='green')

hist(door_choices, breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), 
col='blue',border='black', density=35,xlab='Proportion of trials covered option selected', main='')

stripchart(door_choices, vertical = TRUE,  
    method = "jitter", pch = 19, col = 'blue', ylab='Proportion of trials covered option selected')
    
    
abline(0.165, 0, col='red')
abline(0.34, 0, col='red')  
    
    

non_door_data <- alldata[alldata$door_chose == 0,]

choices <- tapply(non_door_data$reward, list(non_door_data$subj, non_door_data$reward), length)
choices[is.na(choices)] <- 0

door_n <- tapply(alldata$door_chose, alldata$subj, sum)
non_door_n <- 100 - door_n

choice_props <- choices[,4]/non_door_n[-5]


barplot(choices[,4]/100, ylim=c(0,1), xlab="Subject", main="Door not chosen trials", ylab = "10 options chosen proportion")




######################################################################

 subjects <- unique(alldata$subj)
 
 graphs_per_row = 5
 
 #par(mfrow=c( ceiling(length(subjects) / graphs_per_row), graphs_per_row))
 par(mfrow=c(2,5))
 
 for (i in subjects) {
		graphdata <- alldata[alldata$subj == i,]
 
 
 		block1 <- graphdata[graphdata$trial <= 20,]
 		block2 <- graphdata[graphdata$trial > 20 & graphdata$trial <= 40,]
 		block3 <- graphdata[graphdata$trial > 40 & graphdata$trial <= 60,]
 		block4 <- graphdata[graphdata$trial > 60 & graphdata$trial <= 80,]
 		block5 <- graphdata[graphdata$trial > 80 & graphdata$trial <= 100,]
   
 		# means
 		line <- c( sum(block1$door_chose)/20, 
 					 sum(block2$door_chose)/20,
 					 sum(block3$door_chose)/20,
 					 sum(block4$door_chose)/20,
 					 sum(block5$door_chose)/20)

 
 		plot(line, type='l', col = 'blue', ylim=c(0,1), ylab='Door choice proportion',
 		xlab="Block", mar = c(0.1,0.1,0.1,0.1)
 		, main=toString(i))

 
 	   }

#############################################################
hidden_best <- alldata[alldata$door_reward == 10,]
hidden_not_best <- alldata[alldata$door_reward != 10,]

hidden_best_choices <- tapply(hidden_best$reward, list(hidden_best$subj, hidden_best$reward), length)
hidden_not_best_choices <- tapply(hidden_not_best$reward, list(hidden_not_best$subj, hidden_not_best$reward), length)

hidden_best_choices[is.na(hidden_best_choices)] <- 0
hidden_not_best_choices[is.na(hidden_not_best_choices)] <- 0


hidden_best_choices <- as.data.frame(hidden_best_choices)
hidden_not_best_choices <- as.data.frame(hidden_not_best_choices)

hidden_best_choices$sum <- hidden_best_choices$'1'+hidden_best_choices$'2'+hidden_best_choices$'3'+hidden_best_choices$'10'

hidden_not_best_choices$sum <- hidden_not_best_choices$'1'+hidden_not_best_choices$'2'+hidden_not_best_choices$'3'+hidden_not_best_choices$'10'

hidden_best_choices$best_prop <- hidden_best_choices$'10'/hidden_best_choices$sum
hidden_best_choices$other_prop <- 1 - hidden_best_choices$best_prop

hidden_not_best_choices$best_prop <- hidden_not_best_choices$'10'/hidden_not_best_choices$sum
hidden_not_best_choices$other_prop <- 1 - hidden_not_best_choices$best_prop




#################################################
adults_best_by_hidden <- NULL
adults_best_by_hidden$best_hidden <- hidden_best_choices$best_prop
adults_best_by_hidden$best_exposed <- hidden_not_best_choices$best_prop

adults_best_by_hidden <- as.data.frame(adults_best_by_hidden)




#################################################
children_best_by_hidden <- NULL
children_best_by_hidden$best_hidden <- hidden_best_choices$best_prop
children_best_by_hidden$best_exposed <- hidden_not_best_choices$best_prop

children_best_by_hidden <- as.data.frame(children_best_by_hidden)


