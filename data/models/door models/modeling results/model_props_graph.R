




door_prop <- 24/36
lag_prop <- 1/36
value_prop <- 10/36
random_prop <- 1/36

lag1_prop <- 18/32
value1_prop <- 14/32

props <- NULL

#props$door <- c( 0.0, door_prop )
#props$lag <- c( lag1_prop, lag_prop )
#props$value <- c( value1_prop, value_prop)

props$exp1 <- c(0.0, lag1_prop, value1_prop)
props$exp2 <- c(door_prop, lag_prop, value_prop)
props$exp3 <- c(0.0, 11/39, 28/39)

props <- as.data.frame(props)

counts <- c(0, 0, 1, 0,
			lag1_prop, 0.0, value1_prop, 0,
			lag_prop, door_prop,  value_prop, random_prop,
			8/39, 0.0,  21/39, 10/39, 
			0,0,0,0,0,0,0,0)
			
counts <- matrix(counts, 4)

cols= c('#009999', '#FFFF99', '#FF6600') # c("purple", "yellow", "orange")

cols= c('#009999', '#FFFF99', '#FF3366')

cols= c('#009999', '#FFFF99', '#CC6699')

cols= c('#009999', '#FFFF99', '#FF99FF')

cols= c('#009999', '#FFFF66', '#CC6699')

cols= c('#009966', '#FFFF66', '#CC6699')

densities <- c(30, 100, 100)

cols= c('#FFFF66', '#009966',  '#990066', '#FFFFFF')

densities <- c(100, 100, 30, 100)

barplot(counts, main="",
  		names.arg = c('Adults','Children', 'Children', 'Children', '', ''), col= cols,
 		legend = c('Lag', 'Uncertainty',  'Value', 'Random'), density = densities,
 		ylab='Proportion of Participants', cex.axis = 1.5, cex.lab = 1.5,
 		args.legend = c(cex=1.2), cex.names = 1.2)




barplot(counts, main="",
  		names.arg = c('Adults','Children', 'Exp. 2', 'Exp. 3', '', ''), col= cols,
 		legend = c('Uncertainty', 'Lag', 'Value'), density = densities,
 		ylab='Proportion of Participants', cex.axis = 1.5, cex.lab = 1.5,
 		args.legend = c(cex=1.3), cex.names = 1.2)

sum(model.data$RL_llh)
sum(model.data$Lag_llh)


par(mfrow=c(1,2))
hist(model.data$RL_llh, xlim=c(0,140))
hist(model.data$Lag_llh, xlim=c(0,140))



model.data$Lag_aic <- model.data$Lag_llh*2 + 2
model.data$random_better_than_lag <- model.data$Lag_aic > (138.62943611198907*2)
model.data$random_better_than_lag