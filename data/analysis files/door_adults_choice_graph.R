
# means
line10_means <- c( mean(block1_choices[,4]), 
             mean(block2_choices[,4]),
             mean(block3_choices[,4]),
             mean(block4_choices[,2]),
             mean(block5_choices[,4]))
             
line3_means <- c( mean(block1_choices[,3]), 
             mean(block2_choices[,3]),
             mean(block3_choices[,3]),
             mean(block4_choices[,1]),
             mean(block5_choices[,3]))
             
line2_means <- c( mean(block1_choices[,2]), 
             mean(block2_choices[,2]), 
             mean(block3_choices[,2]), 
             0,
             mean(block5_choices[,2]))
             
line1_means <- c( mean(block1_choices[,1]), 
             mean(block2_choices[,1]),
             mean(block3_choices[,1]),
             0,
             mean(block5_choices[,1]))

# standard deviations
line10_sds <- c( sd(block1_choices[,4]), 
             sd(block2_choices[,4]),
             sd(block3_choices[,4]),
             sd(block4_choices[,2]),
             sd(block5_choices[,4]))
             
line3_sds <- c( sd(block1_choices[,3]), 
             sd(block2_choices[,3]),
             sd(block3_choices[,3]),
             sd(block4_choices[,1]),
             sd(block5_choices[,3]))
             
line2_sds <- c( sd(block1_choices[,2]), 
             sd(block2_choices[,2]), 
             sd(block3_choices[,2]), 
             0,
             sd(block5_choices[,2]))
             
line1_sds <- c( sd(block1_choices[,1]), 
             sd(block2_choices[,1]),
             sd(block3_choices[,1]),
             0,
             sd(block5_choices[,1]))

# Ns
line10_lengths <- c( length(block1_choices[,1]), 
             length(block2_choices[,1]),
             length(block3_choices[,1]),
             length(block4_choices[,1]),
             length(block5_choices[,1]))
             
line3_lengths <- c( length(block1_choices[,1]), 
             length(block2_choices[,1]),
             length(block3_choices[,1]),
             length(block4_choices[,1]),
             length(block5_choices[,1]))
             
line2_lengths <- c( length(block1_choices[,1]), 
             length(block2_choices[,1]),
             length(block3_choices[,1]),
             length(block4_choices[,1]),
             length(block5_choices[,1]))
             
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

