

setwd('/Users/nathanblanco/Dropbox/Work/current_projects/exploration studies/Baseline/data/models/modeling results')



child.ebm.data <- read.table('EBM_params_children.txt',header=T)
adult.ebm.data <- read.table('EBM_params_adults.txt',header=T)

child.RL.data <- read.table('RL_params_children.txt',header=T)
adult.RL.data <- read.table('RL_params_adults.txt',header=T)

child.pattern.data <- read.table('pattern_model_children.txt', header=T)
adult.pattern.data <- read.table('pattern_model_adults.txt', header=T)

# mean is dashed, median is solid

childAICS <- NULL
childAICS$RL <- child.RL.data$RL_aic
childAICS$ebm <- child.ebm.data$ebm_aic
childAICS$pattern <- child.pattern.data$Pattern_aic

childAICS <- as.data.frame(childAICS)

childAICS$ebm_best <- as.numeric((childAICS$ebm < childAICS$pattern) & (childAICS$ebm < childAICS$RL))
childAICS$RL_best <- as.numeric((childAICS$RL < childAICS$pattern) & (childAICS$RL < childAICS$ebm))
childAICS$pattern_best <- as.numeric((childAICS$pattern < childAICS$ebm) & (childAICS$pattern < childAICS$RL))

childAICS$pattern_over_ebm <- as.numeric((childAICS$pattern < childAICS$ebm))

colSums(childAICS)

 
adultAICS <- NULL
adultAICS$RL <- adult.RL.data$RL_aic
adultAICS$ebm <- adult.ebm.data$ebm_aic
adultAICS$pattern <- adult.pattern.data$Pattern_aic

adultAICS <- as.data.frame(adultAICS)

adultAICS$ebm_best <- as.numeric((adultAICS$ebm < adultAICS$pattern) & (adultAICS$ebm < adultAICS$RL))
adultAICS$RL_best <- as.numeric((adultAICS$RL < adultAICS$pattern) & (adultAICS$RL < adultAICS$ebm))
adultAICS$pattern_best <- as.numeric((adultAICS$pattern < adultAICS$ebm) & (adultAICS$pattern < adultAICS$RL))

adultAICS$pattern_over_ebm <- as.numeric((adultAICS$pattern < adultAICS$ebm))

colSums(adultAICS)