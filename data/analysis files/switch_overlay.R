




#child.alldata <- read.table('baseline_children.txt', header=F)
#adult.alldata <- read.table('BL_NV_combined_adults.txt', header=F)

#names(child.alldata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'r1', 'r2', 'r3', 'r4')
#names(adult.alldata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'r1', 'r2', 'r3', 'r4')


child.alldata <- read.table('door_data.txt', header=F)
adult.alldata <- read.table('door_data_adults.txt', header=F)

names(child.alldata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'door_loc', 'door_chose', 'r1', 'r2', 'r3', 'r4')
names(adult.alldata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'door_loc', 'door_chose', 'r1', 'r2', 'r3', 'r4')





child.maindata <- child.alldata[child.alldata$phase == 'main',]
adult.maindata <- adult.alldata[adult.alldata$phase == 'main',]

child.maindata$stay <- NULL
for (i in 1:length(child.maindata[,1])) {
    child.maindata$stay[i] <- as.numeric(child.maindata[i,]$res == child.maindata[i-1,]$res)
    
    if (child.maindata[i,]$trial == 1) { child.maindata$stay[i] <- 0 }
    
    }

child.stay_total <- tapply(child.maindata$stay, child.maindata$subj, sum)


adult.maindata$stay <- NULL
for (i in 1:length(adult.maindata[,1])) {
    adult.maindata$stay[i] <- as.numeric(adult.maindata[i,]$res == adult.maindata[i-1,]$res)
    
    if (adult.maindata[i,]$trial == 1) { adult.maindata$stay[i] <- 0 }
    
    }

adult.stay_total <- tapply(adult.maindata$stay, adult.maindata$subj, sum)


results <- NULL

results$stay <- c(child.stay_total, adult.stay_total)
results$group <- (c( rep(1, length(child.stay_total)),
					rep(2, length(adult.stay_total))))

results <- as.data.frame(results)

plot(results$group, 1-(results$stay)/100, type = 'p')


double_ups <- NULL

double_ups$stay <- c(0, 2, 2, 2, 3, 3, 8,
					76, 76, 77, 85, 86, 89, 91, 93, 93, 93, 95)

double_ups$group <- c(1.02, 1.02, 1.04, 1.06, 1.02, 1.04, 1.02,
						2.02, 2.04, 2.02, 2.02, 2.02, 2.02, 2.02, 2.02, 2.04, 2.06, 2.02)
						
double_ups <- as.data.frame(double_ups)			

rbind(results, double_ups)			

plot_list <- rbind(results, double_ups)
plot(plot_list$group, 1-(plot_list$stay)/100, type = 'p', pch= 1)



#################################################

###########################

graph_data <- NULL
graph_data$switches <- c(1-(child.stay_total)/100, 1-(adult.stay_total)/100)
graph_data$group <- factor(c(rep("Children", length(child.stay_total)),
							 rep("z Adults", length(adult.stay_total))))
graph_data <- as.data.frame(graph_data)

boxplot(switches ~ group, data=graph_data,
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,1),
 main = 'Response Switching', names.arg=c('Children', 'Adults'))
stripchart(switches ~ group, data=graph_data, vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue', 'green'))


abline(0.83, 0, col='red')
abline(0.65, 0, col='red')

#############################


child.alldata <- read.table('bland.txt', header=F)
names(child.alldata) <- c('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'r1', 'r2', 'r3', 'r4')

child.maindata <- child.alldata[child.alldata$phase == 'main',]
adult.maindata <- adult.alldata[adult.alldata$phase == 'main',]

child.maindata$stay <- NULL
for (i in 1:length(child.maindata[,1])) {
    child.maindata$stay[i] <- as.numeric(child.maindata[i,]$res == child.maindata[i-1,]$res)
    
    if (child.maindata[i,]$trial == 1) { child.maindata$stay[i] <- 0 }
    
    }

child.stay_total <- tapply(child.maindata$stay, child.maindata$subj, mean)


boxplot(1-child.stay_total, 
 ylab = 'Proportion of switch responses', col='white', ylim=c(0,1),
 xlab='Children')
stripchart(1-child.stay_total,  vertical = TRUE,  
    method = "jitter", add = TRUE, pch = 19, col = c('blue'))
abline(0.83, 0, col='red')
abline(0.65, 0, col='red')
