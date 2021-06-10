model.data  <- read.table('separate_model_fits_door.txt', header=T)


#subj RL RL_auto switch switch_prob bias random lag

#hist(model.data$switch_prob)

#subj RL RL_auto switch switch_prob bias random lag Bev Blag



#model.data$RL_best <- (model.data$RL_llh < model.data$Lag_llh) & 
#					  (model.data$RL_llh < model.data$Door_llh) 
					  
#model.data$Lag_best <- (model.data$Lag_llh < model.data$RL_llh) & 
#					  (model.data$Lag_llh < model.data$Door_llh) 
					  
#model.data$door_best <- (model.data$Door_llh < model.data$Lag_llh) & 
#					  (model.data$Door_llh < model.data$RL_llh) 
					  

#sum(as.numeric(model.data$RL_best))
#sum(as.numeric(model.data$Lag_best))
#sum(as.numeric(model.data$door_best))





model.data$RL_aic <- (model.data$RL_llh * 2) + 4
model.data$Lag_aic <- (model.data$Lag_llh * 2) + 2
model.data$Door_aic <- (model.data$Door_llh * 2) + 2
model.data$random_aic <- -log(0.25)*100*2


model.data$RL_best <- (model.data$RL_aic < model.data$Lag_aic) & 
					  (model.data$RL_aic < model.data$Door_aic)  & 
					  (model.data$RL_aic < model.data$random_aic)
					  
model.data$Lag_best <- (model.data$Lag_aic < model.data$RL_aic) & 
					  (model.data$Lag_aic < model.data$Door_aic) & 
					  (model.data$Lag_aic < model.data$random_aic) 
					  
model.data$Door_best <- (model.data$Door_aic < model.data$RL_aic) & 
						(model.data$Door_aic < model.data$Lag_aic) & 
						(model.data$Door_aic < model.data$random_aic) 

model.data$random_best <- (model.data$random_aic < model.data$RL_aic) & 
						(model.data$random_aic < model.data$Lag_aic) & 
						(model.data$random_aic < model.data$Door_aic) 
						



sum(as.numeric(model.data$RL_best))
sum(as.numeric(model.data$Lag_best))
sum(as.numeric(model.data$Door_best))
sum(as.numeric(model.data$random_best))

unique(model.data[model.data$RL_best==TRUE,]$subj)
unique(model.data[model.data$Lag_best==TRUE,]$subj)
unique(model.data[model.data$Door_best==TRUE,]$subj)
unique(model.data[model.data$random_best==TRUE,]$subj)

