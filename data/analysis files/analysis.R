



setwd("/Users/nathanblanco/Dropbox/Work/current_projects/exploration studies/Baseline/data")

alldata <- read.table('baseline_children.txt', header=F)
#alldata <- read.table('bland.txt', header=F)

#alldata <- read.table('BL_NV_combined_adults.txt', header=F)


#alldata <- read.table('baseline_adults.txt', header=F)

#alldata <- read.table('novelty1_adults.txt', header=F)

#alldata <- read.table('novelty10_children.txt', header=F)

#alldata <- read.table('creatures10_children.txt', header=F)
#alldata <- read.table('creature1_children.txt', header=F)

#alldata <- read.table('baseline5-7.txt', header=F)

#alldata <- read.table('BL_NV_combined_children.txt', header=F)

# self.output_trial([self.subj, 'main', self.trial, self.cond, res, rt, payoff, self.earnings, self.creatures[int(res)], self.rewards])

names(alldata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'r1', 'r2', 'r3', 'r4')

#alldata <- read.table('door_data.txt', header=F)
#alldata <- read.table('door_data_adults.txt', header=F)

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

child_age_data <- read.table('AGE_MONTHS.TXT', header=T)

child_age_data <- child_age_data[order(child_age_data$subj),]

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

stay_total <- tapply(maindata$stay, maindata$subj, sum)

for (i in 1:length(stay_total)) {
	x <- binom.test(stay_total[i], 100, 0.25)
	print(x)
	
}

for (i in seq(100)) {
	x <- binom.test(i, 100, 0.75)
	print(x)
	
}



for (i in seq(30)) {
	x <- binom.test(i, 30, 1/3)
	print(x)
	
}

65-83

#binom.test(x, n, p = 0.5,
#           alternative = c("two.sided", "less", "greater"),
#           conf.level = 0.95)

for (i in 1:10) {
	x <- binom.test(i, 10, 0.5)
	print(x)
	
}


stay_props <- tapply(maindata$stay, maindata$subj, mean)

hist(stay_props, main= "Children:  Stay proportions", xlab="Proportion of repeat responses", col='grey')

barplot(stay_props[order(stay_props)],
		main ='Baseline: adults',
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

stay_props_byoptions <- tapply(maindata$stay, list(maindata$reward, maindata$subj), mean)

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


transitions_subj <- tapply(maindata$reward, list( maindata$reward, maindata$prev_rew, maindata$subj), length)


transitions <- tapply(maindata$reward, list( maindata$reward, maindata$prev_rew), length)

transitions_temp <- transitions

sums <- colSums(transitions)



for (i in seq(length(transitions[,1]))) {
	transitions[i,] <- transitions_temp[i,]/sums
}

transitions[,2:5]

library("RColorBrewer")
#display all colour schemes
display.brewer.all()



tx <- transitions[order(as.integer(row.names(transitions)), decreasing = T),]

#heatmap(tx[,2:5], Rowv=NA, Colv=NA, col=brewer.pal(9,"Blues"), scale="none",
#			xlab = "previous trial", ylab = 'trial')

library(gplots)
heatmap.2(tx[,2:5], cellnote=round(tx[,2:5],3), Rowv=NA, Colv=NA, col=brewer.pal(9,"Blues"), 
			dendrogram = 'none', notecol='black', revC=T, notecex=1.5, trace='none',
			breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
			scale="none", xlab = "previous trial", ylab = 'trial')

			

RLdata <- maindata[maindata$subj %in% RL_subj,]
Lagdata <- maindata[maindata$subj %in% Lag_subj,]
Randdata <- maindata[maindata$subj %in% random_subj,]
Doordata <- maindata[maindata$subj %in% door_subj,]


RL_transitions <- tapply(RLdata$reward, list( RLdata$reward, RLdata$prev_rew), length)

RL_transitions_temp <- RL_transitions

sums <- colSums(RL_transitions)

for (i in seq(length(RL_transitions[,1]))) {
	RL_transitions[i,] <- RL_transitions_temp[i,]/sums
}




tx <- RL_transitions[order(as.integer(row.names(RL_transitions)), decreasing = T),]

heatmap.2(tx[,2:5], cellnote=round(tx[,2:5],3), Rowv=NA, Colv=NA, col=brewer.pal(9,"Blues"), 
			dendrogram = 'none', notecol='black', revC=T, notecex=1.5, trace='none',
			breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
			scale="none", xlab = "previous trial", ylab = 'trial')

########

Rand_transitions <- tapply(Randdata$reward, list( Randdata$reward, Randdata$prev_rew), length)

Rand_transitions_temp <- Rand_transitions

sums <- colSums(Rand_transitions)

for (i in seq(length(Rand_transitions[,1]))) {
	Rand_transitions[i,] <- Rand_transitions_temp[i,]/sums
}




tx <- Rand_transitions[order(as.integer(row.names(Rand_transitions)), decreasing = T),]

heatmap.2(tx[,2:5], cellnote=round(tx[,2:5],3), Rowv=NA, Colv=NA, col=brewer.pal(9,"Blues"), 
			dendrogram = 'none', notecol='black', revC=T, notecex=1.5, trace='none',
			breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
			scale="none", xlab = "previous trial", ylab = 'trial')



Lag_transitions <- tapply(Lagdata$reward, list( Lagdata$reward, Lagdata$prev_rew), length)

Lag_transitions_temp <- Lag_transitions

sums <- colSums(Lag_transitions)

for (i in seq(length(Lag_transitions[,1]))) {
	Lag_transitions[i,] <- Lag_transitions_temp[i,]/sums
}




tx <- Lag_transitions[order(as.integer(row.names(Lag_transitions)), decreasing = T),]

heatmap.2(tx[,2:5], cellnote=round(tx[,2:5],3), Rowv=NA, Colv=NA, col=brewer.pal(9,"Blues"), 
			dendrogram = 'none', notecol='black', revC=T, notecex=1.5, trace='none',
			breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
			scale="none", xlab = "previous trial", ylab = 'trial')


















####################################################################

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
    
    
  hist(maindata$nback, breaks = c(0,1,2,3,4,5,6,7,8,9,10, 100000000000000), xlim=c(0,10), col='grey')  

RLdata <- maindata[maindata$subj %in% RL_subj,]
Lagdata <- maindata[maindata$subj %in% Lag_subj,]

  hist(RLdata$nback, breaks = c(0,1,2,3,4,5,6,7,8,9,10, 100000000000000), xlim=c(0,10), col='grey', main='RL')  
  hist(Lagdata$nback, breaks = c(0,1,2,3,4,5,6,7,8,9,10, 100000000000000), xlim=c(0,10), col='grey', main='Lag')  



# hazard rate should be able to  be computer from proportion of length(x == nback)/length(x >= nback)

hazard_lengths <- tapply( maindata$nback, list(maindata$nback, maindata$subj), length )
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
plot( hazard_means, type='l', xlim=c(0,10))
segments(x, hazard_means[1:10]+hazard_ses[1:10], x, hazard_means[1:10]-hazard_ses[1:10])

###############

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
plot( hazard_means, type='l', xlim=c(0,10))
segments(x, hazard_means[1:10]+hazard_ses[1:10], x, hazard_means[1:10]-hazard_ses[1:10])

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

x= seq(10)
plot( hazard_means, type='l', xlim=c(0,10))
segments(x, hazard_means[1:10]+hazard_ses[1:10], x, hazard_means[1:10]-hazard_ses[1:10])

##############################


hazard_lengths <- tapply( Randdata$nback, list(Randdata$nback, Randdata$subj), length )
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
plot( hazard_means, type='l', xlim=c(0,10))
segments(x, hazard_means[1:10]+hazard_ses[1:10], x, hazard_means[1:10]-hazard_ses[1:10])



####################################################################


choice_props <- tapply(alldata$reward, list(alldata$subj, alldata$reward), length)

0.4290625

t.test(choice_props_adults, choice_props_kids, var.equal=T)

d.t.unpaired<-function(t.val,n1,n2){
  d<-t.val*sqrt((n1+n2)/(n1*n2))
  names(d)<-"effect size d"
  return(d)
}


block1 <- maindata[maindata$trial <= 20,]
block2 <- maindata[maindata$trial > 20 & maindata$trial <= 40,]
block3 <- maindata[maindata$trial > 40 & maindata$trial <= 60,]
block4 <- maindata[maindata$trial > 60 & maindata$trial <= 80,]
block5 <- maindata[maindata$trial > 80 & maindata$trial <= 100,]


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

x <- seq(5)
segments(x, line10_means/20+line10_ses/20, x, line10_means/20-line10_ses/20, col='blue')
segments(x, line3_means/20+line3_ses/20, x, line3_means/20-line3_ses/20, col='green')
segments(x, line2_means/20+line2_ses/20, x, line2_means/20-line2_ses/20, col='yellow')
segments(x, line1_means/20+line1_ses/20, x, line1_means/20-line1_ses/20, col='red')

legend(1, 1.0, c("10", "3", "2", "1"), 
	   col = c("blue", "green", "yellow", "red"), 
	   lty = c(1,1,1,1), cex = 1.2)



 subjects <- unique(alldata$subj)
 
 graphs_per_row = 5
 
 #par(mfrow=c( ceiling(length(subjects) / graphs_per_row), graphs_per_row))
 par(mfrow=c(3,5))
 
 for (i in subjects) {
		graphdata <- maindata[maindata$subj == i,]
 
 
 		block1 <- graphdata[graphdata$trial <= 20,]
 		block2 <- graphdata[graphdata$trial > 20 & graphdata$trial <= 40,]
 		block3 <- graphdata[graphdata$trial > 40 & graphdata$trial <= 60,]
 		block4 <- graphdata[graphdata$trial > 60 & graphdata$trial <= 80,]
 		block5 <- graphdata[graphdata$trial > 80 & graphdata$trial <= 100,]
 
         block1_choices <- c(0,0,0,0)
         block2_choices <- c(0,0,0,0)
         block3_choices <- c(0,0,0,0)
         block4_choices <- c(0,0,0,0)
         block5_choices <- c(0,0,0,0)
 
 
 		block1_choices[1] <- length(block1[block1$reward == 1,]$reward)
 		block1_choices[2] <- length(block1[block1$reward == 2,]$reward)
 		block1_choices[3] <- length(block1[block1$reward == 3,]$reward)
 		block1_choices[4] <- length(block1[block1$reward == 10,]$reward)
 		block1_choices[is.na(block1_choices)] <- 0
 		
 		block2_choices[1] <- length(block2[block2$reward == 1,]$reward)
 		block2_choices[2] <- length(block2[block2$reward == 2,]$reward)
 		block2_choices[3] <- length(block2[block2$reward == 3,]$reward)
 		block2_choices[4] <- length(block2[block2$reward == 10,]$reward)
 		block2_choices[is.na(block2_choices)] <- 0
 		
 		
 		block3_choices[1] <- length(block3[block3$reward == 1,]$reward)
 		block3_choices[2] <- length(block3[block3$reward == 2,]$reward)
 		block3_choices[3] <- length(block3[block3$reward == 3,]$reward)
 		block3_choices[4] <- length(block3[block3$reward == 10,]$reward)
 		block3_choices[is.na(block3_choices)] <- 0
 		
 		
 		block4_choices[1] <- length(block4[block4$reward == 1,]$reward)
 		block4_choices[2] <- length(block4[block4$reward == 2,]$reward)
 		block4_choices[3] <- length(block4[block4$reward == 3,]$reward)
 		block4_choices[4] <- length(block4[block4$reward == 10,]$reward)
 		block4_choices[is.na(block4_choices)] <- 0
 		
 		
 		block5_choices[1] <- length(block5[block5$reward == 1,]$reward)
 		block5_choices[2] <- length(block5[block5$reward == 2,]$reward)
 		block5_choices[3] <- length(block5[block5$reward == 3,]$reward)
 		block5_choices[4] <- length(block5[block5$reward == 10,]$reward)
 		block5_choices[is.na(block5_choices)] <- 0
 
 
 
 
 		# means
 		line10 <- c( block1_choices[4]/20, 
 					 block2_choices[4]/20,
 					 block3_choices[4]/20,
 					 block4_choices[4]/20,
 					 block5_choices[4]/20)
 	 
 		line3 <- c( block1_choices[3]/20, 
 					 block2_choices[3]/20,
 					 block3_choices[3]/20,
 					 block4_choices[3]/20,
 					 block5_choices[3]/20)
 	 
 		line2 <- c( block1_choices[2]/20, 
 					 block2_choices[2]/20,
 					 block3_choices[2]/20,
 					 block4_choices[2]/20,
 					 block5_choices[2]/20)
 	 
 		line1 <- c( block1_choices[1]/20, 
 					 block2_choices[1]/20,
 					 block3_choices[1]/20,
 					 block4_choices[1]/20,
 					 block5_choices[1]/20)
 
 		plot(line10, type='l', col = 'blue', ylim=c(0,1), ylab='',
 		xlab="Block", mar = c(0.1,0.1,0.1,0.1))
 		#, main=toString(i))
 		lines(line3, col = 'green')
 		lines(line2, col = 'yellow')
 		lines(line1, col = 'red')
 
 
 	   }
#####################################################################################

###############################################

choice_props <- tapply(RLdata$reward, list(RLdata$subj, RLdata$reward), length)

block1 <- RLdata[RLdata$trial <= 20,]
block2 <- RLdata[RLdata$trial > 20 & RLdata$trial <= 40,]
block3 <- RLdata[RLdata$trial > 40 & RLdata$trial <= 60,]
block4 <- RLdata[RLdata$trial > 60 & RLdata$trial <= 80,]
block5 <- RLdata[RLdata$trial > 80 & RLdata$trial <= 100,]


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
	 xlab="Block (20 trials)", main='Value Subjects', cex.lab= 1.4, cex.axis=1.3, lwd=3)
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

choice_props <- tapply(Lagdata$reward, list(Lagdata$subj, Lagdata$reward), length)

block1 <- Lagdata[Lagdata$trial <= 20,]
block2 <- Lagdata[Lagdata$trial > 20 & Lagdata$trial <= 40,]
block3 <- Lagdata[Lagdata$trial > 40 & Lagdata$trial <= 60,]
block4 <- Lagdata[Lagdata$trial > 60 & Lagdata$trial <= 80,]
block5 <- Lagdata[Lagdata$trial > 80 & Lagdata$trial <= 100,]


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
	 xlab="Block (20 trials)", main='Lag subjects', cex.lab= 1.4, cex.axis=1.3, lwd=3)
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






#####################################################################################
RL_stay_total <- tapply(RLdata$stay, RLdata$subj, mean)

boxplot(1-RL_stay_total, 
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,1),
 main = 'RL kids')
stripchart(1-RL_stay_total,  vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue'))
abline(0.83, 0, col='red')
abline(0.65, 0, col='red')




Lag_stay_total <- tapply(Lagdata$stay, Lagdata$subj, mean)

boxplot(1-Lag_stay_total, 
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,1),
 main = 'Lag kids')
stripchart(1-Lag_stay_total,  vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue'))
abline(0.83, 0, col='red')
abline(0.65, 0, col='red')

child_data<-alldata
#adult_data <- alldata


################################3

best_props <- choices[,4]/100

x <- lm(best_props~child_age_data$months)
summary(x)

x <- glm(best_props~child_age_data$months, family = 'binomial')
summary(x)


# library(lme4)
# 
# x <- glmer(res~right_higher+right_rarer+prev_res + (1|subj), family = "binomial", data=main.child.data[main.child.data$prev_res != -2,])
# summary(x)