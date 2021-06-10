



setwd("/Users/nathanblanco/Dropbox/Work/current_projects/exploration studies/Baseline/data")

adult_data <- read.table('door_data_adults.txt', header=F)
child_data <- read.table('door_data_children.txt', header=F)

names(adult_data) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'door_loc', 'door_chose', 'r1', 'r2', 'r3', 'r4')

names(child_data) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'door_loc', 'door_chose', 'r1', 'r2', 'r3', 'r4')

for (i in 1:length(adult_data[,1])) {
	
	adult_data$door_reward[i] <- adult_data[i,12+adult_data$door_loc[i]]
	
}

for (i in 1:length(child_data[,1])) {
  
  child_data$door_reward[i] <- child_data[i,12+child_data$door_loc[i]]
  
}

adult_door_choices <- tapply(adult_data$door_chose, adult_data$subj, mean)
child_door_choices <- tapply(child_data$door_chose, child_data$subj, mean)


graph_data <- NULL
graph_data$props <- c(child_door_choices, adult_door_choices)
graph_data$group <- factor(c(rep("Children", length(child_door_choices)),
                             rep("z Adults", length(adult_door_choices))))
graph_data <- as.data.frame(graph_data)

boxplot(props ~ group, data=graph_data,
        ylab = 'Proportion of trials hidden option selected', col='white', ylim=c(0,1),
        main = 'Hidden option choices', names.arg=c('Children', 'Adults'), cex.axis=1.2, cex.lab=1.2)
stripchart(props ~ group, data=graph_data, vertical = TRUE,  
           method = "jitter", add = TRUE, pch = 19, col = c('blue', 'green'))



##################################################################################

#############################################################
adult_hidden_best <- adult_data[adult_data$door_reward == 10,]
adult_hidden_not_best <- adult_data[adult_data$door_reward != 10,]

adult_hidden_best_choices <- tapply(adult_hidden_best$reward, list(adult_hidden_best$subj, adult_hidden_best$reward), length)
adult_hidden_not_best_choices <- tapply(adult_hidden_not_best$reward, list(adult_hidden_not_best$subj, adult_hidden_not_best$reward), length)

adult_hidden_best_choices[is.na(adult_hidden_best_choices)] <- 0
adult_hidden_not_best_choices[is.na(adult_hidden_not_best_choices)] <- 0


adult_hidden_best_choices <- as.data.frame(adult_hidden_best_choices)
adult_hidden_not_best_choices <- as.data.frame(adult_hidden_not_best_choices)

adult_hidden_best_choices$sum <- adult_hidden_best_choices$'1'+adult_hidden_best_choices$'2'+adult_hidden_best_choices$'3'+adult_hidden_best_choices$'10'

adult_hidden_not_best_choices$sum <- adult_hidden_not_best_choices$'1'+adult_hidden_not_best_choices$'2'+adult_hidden_not_best_choices$'3'+adult_hidden_not_best_choices$'10'

adult_hidden_best_choices$best_prop <- adult_hidden_best_choices$'10'/adult_hidden_best_choices$sum
adult_hidden_best_choices$other_prop <- 1 - adult_hidden_best_choices$best_prop

adult_hidden_not_best_choices$best_prop <- adult_hidden_not_best_choices$'10'/adult_hidden_not_best_choices$sum
adult_hidden_not_best_choices$other_prop <- 1 - adult_hidden_not_best_choices$best_prop

############################################################

children_hidden_best <- child_data[child_data$door_reward == 10,]
children_hidden_not_best <- child_data[child_data$door_reward != 10,]

children_hidden_best_choices <- tapply(children_hidden_best$reward, list(children_hidden_best$subj, children_hidden_best$reward), length)
children_hidden_not_best_choices <- tapply(children_hidden_not_best$reward, list(children_hidden_not_best$subj, children_hidden_not_best$reward), length)

children_hidden_best_choices[is.na(children_hidden_best_choices)] <- 0
children_hidden_not_best_choices[is.na(children_hidden_not_best_choices)] <- 0


children_hidden_best_choices <- as.data.frame(children_hidden_best_choices)
children_hidden_not_best_choices <- as.data.frame(children_hidden_not_best_choices)

children_hidden_best_choices$sum <- children_hidden_best_choices$'1'+children_hidden_best_choices$'2'+children_hidden_best_choices$'3'+children_hidden_best_choices$'10'

children_hidden_not_best_choices$sum <- children_hidden_not_best_choices$'1'+children_hidden_not_best_choices$'2'+children_hidden_not_best_choices$'3'+children_hidden_not_best_choices$'10'

children_hidden_best_choices$best_prop <- children_hidden_best_choices$'10'/children_hidden_best_choices$sum
children_hidden_best_choices$other_prop <- 1 - children_hidden_best_choices$best_prop

children_hidden_not_best_choices$best_prop <- children_hidden_not_best_choices$'10'/children_hidden_not_best_choices$sum
children_hidden_not_best_choices$other_prop <- 1 - children_hidden_not_best_choices$best_prop

#################################################
adults_best_by_hidden <- NULL
adults_best_by_hidden$best_hidden <- adult_hidden_best_choices$best_prop
adults_best_by_hidden$best_exposed <- adult_hidden_not_best_choices$best_prop

adults_best_by_hidden <- as.data.frame(adults_best_by_hidden)




#################################################
children_best_by_hidden <- NULL
children_best_by_hidden$best_hidden <- children_hidden_best_choices$best_prop
children_best_by_hidden$best_exposed <- children_hidden_not_best_choices$best_prop

children_best_by_hidden <- as.data.frame(children_best_by_hidden)

###########################################################

#par(mfrow=c(1,2))
###########################

graph_data <- NULL
graph_data$props <- c(children_best_by_hidden$best_exposed, adults_best_by_hidden$best_exposed)
graph_data$group <- factor(c(rep("Children", length(children_best_by_hidden$best_exposed)),
							 rep("z Adults", length(adults_best_by_hidden$best_exposed))))
graph_data <- as.data.frame(graph_data)


boxplot(props ~ group, data=graph_data,
 ylab = 'Proportion of trials best option selected', col='white', ylim=c(0,1),
 main = 'Best option exposed', names.arg=c('Children', 'Adults'), cex.axis=1.2, cex.lab=1.2)
stripchart(props ~ group, data=graph_data, vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue', 'green'))


#abline(0.25, 0, col='red')




graph_data <- NULL
graph_data$props <- c(children_best_by_hidden$best_hidden, adults_best_by_hidden$best_hidden)
graph_data$group <- factor(c(rep("Children", length(children_best_by_hidden$best_hidden)),
							 rep("z Adults", length(adults_best_by_hidden$best_hidden))))
graph_data <- as.data.frame(graph_data)


boxplot(props ~ group, data=graph_data,
 ylab = 'Proportion of trial best option selected', col='white', ylim=c(0,1),
 main = 'Best option hidden', names.arg=c('Children', 'Adults'), cex.axis=1.2, cex.lab=1.2)
stripchart(props ~ group, data=graph_data, vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue', 'green'))


#abline(0.25, 0, col='red')


#########################


results <- NULL

results$prop <- c(children_best_by_hidden$best_exposed, adults_best_by_hidden$best_exposed,
				  children_best_by_hidden$best_hidden, adults_best_by_hidden$best_hidden)

results$hidden <- factor(c(
				   rep(0, length(children_best_by_hidden$best_exposed)),
				   rep(0, length(adults_best_by_hidden$best_exposed)),
				   rep(1, length(children_best_by_hidden$best_hidden)),
				   rep(1, length(adults_best_by_hidden$best_hidden))
				   ))

			   
results$age <- factor(c(rep(0, length(children_best_by_hidden$best_exposed)),
				        rep(1, length(adults_best_by_hidden$best_exposed)),
				        rep(0, length(children_best_by_hidden$best_hidden)),
				        rep(1, length(adults_best_by_hidden$best_hidden))
				        ))
	   
results$subj <- factor(rep(seq(length(children_best_by_hidden$best_exposed)+length(adults_best_by_hidden$best_exposed)), 2))
		
		
		
				   
results <- as.data.frame(results)
#results

#x <- aov(acc~block*age + Error(subj/block), data=results)
#summary(x)

x <- aov(prop~hidden*age + Error(factor(subj)), data=results)
summary(x)


##################################################
#################################################3


#############################################################


adult_hidden_best_door_choices <- tapply(adult_hidden_best$door_chose, adult_hidden_best$subj, mean)
adult_hidden_not_best_door_choices <- tapply(adult_hidden_not_best$door_chose, adult_hidden_not_best$subj, mean)

children_hidden_best_door_choices <- tapply(children_hidden_best$door_chose, children_hidden_best$subj, mean)
children_hidden_not_best_door_choices <- tapply(children_hidden_not_best$door_chose, children_hidden_not_best$subj, mean)


###########################################################

#par(mfrow=c(1,2))
###########################

graph_data <- NULL
graph_data$props <- c(children_hidden_not_best_door_choices, adult_hidden_not_best_door_choices)
graph_data$group <- factor(c(rep("Children", length(children_hidden_not_best_door_choices)),
                             rep("z Adults", length(adult_hidden_not_best_door_choices))))
graph_data <- as.data.frame(graph_data)


boxplot(props ~ group, data=graph_data,
        ylab = 'Proportion of trials hidden option selected', col='white', ylim=c(0,1),
        main = 'Best option exposed', names.arg=c('Children', 'Adults'), cex.axis=1.2, cex.lab=1.2)
stripchart(props ~ group, data=graph_data, vertical = TRUE,  
           method = "jitter", add = TRUE, pch = 19, col = c('blue', 'green'))


#abline(0.25, 0, col='red')


graph_data <- NULL
graph_data$props <- c(children_hidden_best_door_choices, adult_hidden_best_door_choices)
graph_data$group <- factor(c(rep("Children", length(children_hidden_best_door_choices)),
                             rep("z Adults", length(adult_hidden_best_door_choices))))
graph_data <- as.data.frame(graph_data)


boxplot(props ~ group, data=graph_data,
        ylab = 'Proportion of trials hidden option selected', col='white', ylim=c(0,1),
        main = 'Best option hidden', names.arg=c('Children', 'Adults'), cex.axis=1.2, cex.lab=1.2)
stripchart(props ~ group, data=graph_data, vertical = TRUE,  
           method = "jitter", add = TRUE, pch = 19, col = c('blue', 'green'))


#abline(0.25, 0, col='red')


#########################




results <- NULL

results$prop <- c(children_hidden_best_door_choices, adult_hidden_best_door_choices,
                  children_hidden_not_best_door_choices, adult_hidden_not_best_door_choices)

results$best <- factor(c(
  rep(1, length(children_hidden_best_door_choices)),
  rep(1, length(adult_hidden_best_door_choices)),
  rep(0, length(children_hidden_not_best_door_choices)),
  rep(0, length(adult_hidden_not_best_door_choices))
))


results$age <- factor(c(
  rep(0, length(children_hidden_best_door_choices)),
  rep(1, length(adult_hidden_best_door_choices)),
  rep(0, length(children_hidden_not_best_door_choices)),
  rep(1, length(adult_hidden_not_best_door_choices))
))

results$subj <- factor(rep(seq(length(children_hidden_best_door_choices)+length(adult_hidden_best_door_choices)), 2))




results <- as.data.frame(results)
#results

#x <- aov(acc~block*age + Error(subj/block), data=results)
#summary(x)

x <- aov(prop~best*age + Error(factor(subj)), data=results)
summary(x)


t.test(children_hidden_best_door_choices, adult_hidden_best_door_choices, var.equal = T)

t.test(children_hidden_not_best_door_choices, adult_hidden_not_best_door_choices, var.equal = T)


d.t.unpaired<-function(t.val,n1,n2){
  d<-t.val*sqrt((n1+n2)/(n1*n2))
  names(d)<-"effect size d"
  return(d)
}

d.t.unpaired(4.8092, 36, 37)

d.t.unpaired(5.2876, 36, 37)


d.t.unpaired(13.108, 32, 34)