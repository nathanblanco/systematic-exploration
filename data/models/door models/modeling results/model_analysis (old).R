


model.data  <- read.table('door_fits.txt', header=T)


#subj RL RL_auto switch switch_prob bias random lag

#hist(model.data$switch_prob)

#subj RL RL_auto switch switch_prob bias random lag Bev Blag



model.data$RL_best <- (model.data$RL_aic < model.data$RL_Lag_aic) & 
					  (model.data$RL_aic < model.data$RL_door_aic) & 
					  (model.data$RL_aic < model.data$RL_Lag_door_aic)
					  
model.data$Lag_best <- (model.data$RL_Lag_aic < model.data$RL_aic) & 
					  (model.data$RL_Lag_aic < model.data$RL_door_aic) & 
					  (model.data$RL_Lag_aic < model.data$RL_Lag_door_aic)
					  
model.data$door_best <- (model.data$RL_door_aic < model.data$RL_Lag_aic) & 
					  (model.data$RL_door_aic < model.data$RL_aic) & 
					  (model.data$RL_door_aic < model.data$RL_Lag_door_aic)
					  
model.data$Lag_door_best <- (model.data$RL_Lag_door_aic < model.data$RL_Lag_aic) & 
					  (model.data$RL_Lag_door_aic < model.data$RL_door_aic) & 
					  (model.data$RL_Lag_door_aic < model.data$RL_aic)

model.data$RL_over_Lag <- model.data$RL_aic < model.data$RL_Lag_aic

################################################################################

model.params <- read.table('door_fits2.txt', header=T)

median(model.params$beta)
median(model.params$phi)


################################################################################



barplot(model.data$Blag)
barplot(model.data$Bdoor)
barplot(model.data$Bdoor, ylim=c(-5,5))


door_data <- model.data[model.data$door_best==TRUE,]
non_door_data <- model.data[model.data$door_best==FALSE,]


means <- c( mean(door_data$Bev), mean(door_data$Blag), mean(door_data$Bdoor),
			mean(non_door_data$Bev), mean(non_door_data$Blag), mean(non_door_data$Bdoor))
			
			
door_data$ratio <- door_data$Bdoor/door_data$Bev



# unidimensional graph with best-fit proportions