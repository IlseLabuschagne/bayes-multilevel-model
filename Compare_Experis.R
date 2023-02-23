cat("\014") 
rm(list=ls())

install.packages("brms")
library(brms)

install.packages("lme4")
library(lme4)


install.packages("HDInterval")
library(HDInterval)

install.packages("BEST")
library(BEST)

library(sjstats)

#Exp 1 data.
constant_stim_Exp1 <- c(2.675535869,	3.335000582,	3.667829272,	3.002171892,	3.335000582,	3.667829272,	3.667829272,	1.245851446, #N1, 130 Hz, 70 dBA, B1
                   1.027119883,	0.506694206,	3.335000582,	1.194423351,	1.464546957,	2.611857563,	3.335000582,	0.684074402, #N2, 130 Hz, 70 dBA, B1
                   0.410893934,	0.925272967,	1.247738843,	0.925272967,	1.094968337,	0.925272967,	1.245851446,	0, #N3, 130 Hz, 70 dBA, B1
                   -0.21869551,	-1.053653023,	0.347075565,	0.192198424,	0,	          0.487845893,	2.611857563,	0.539273989, #N4, 130 Hz, 70 dBA, B1
                   1.73369734,	  3.335000582,	3.667829272,	2.456840359,	3.667829272,	3.667829272,	3.335000582,	1.464546957, #N1, 130 Hz, 70 dBA, B2
                   1.58473768,	  1.541498916,	3.667829272,	1.750262902,	3.002171892,	3.335000582,	2.221543233,	2.34270718, #N2, 130 Hz, 70 dBA, B2
                   0.00000000,	  0.177380196,	2.264641935,	0,	          0.857424514,	2.34270718,	  0.487845893,	-0.347075565, #N3, 130 Hz, 70 dBA, B2
                   0.487845893,	0.684074402,	0.58827413,	  0.487845893,	0.861454599,	1.683242467,	0.269150383,	0.177380196, #N4, 130 Hz, 70 dBA, B2
                   2.611857563,	1.931813245,	3.667829272,	0.861454599,	2.611857563,	3.667829272,	2.944686252,	0.706577457, #N1, 130 Hz, 50 dBA, B3
                   2.611857563,	2.087261739,	3.667829272,	0.514379033,	1.95239285,	  3.667829272,	3.335000582,	0.539273989, #N2, 130 Hz, 50 dBA, B3
                   1.417434212,	0.192198424,	0.7579695,	  0.000000000,	1.245851446,	2.34270718,	  1.754433049,	0.684074402, #N3, 130 Hz, 50 dBA, B3
                   0.000000000,	0.410893934,	0.00000000,	  -0.514379033,	0.347075565,	0.7579695,	  1.247738843,	1.053653023, #N4, 130 Hz, 50 dBA, B3
                   
                   2.675535869,	-0.876272826,	3.667829272,	0,          	3.002171892,	3.667829272,	3.335000582,	0.167303468, #N1, 210 Hz, 70 dBA, B1
                   2.087261739,	-0.336998837,	3.667829272,	-0.336998837,	3.667829272,	2.944686252,	1.91756637,	  0.7579695, #N2, 210 Hz, 70 dBA, B1
                   1.750262902,	0,           	2.611857563,	1.754433049,	1.194423351,	1.194423351,	0.992293402,	0.876272826, #N3, 210 Hz, 70 dBA, B1
                   0.992293402,	3.335000582,	2.221543233,	1.683242467,	0.514379033,	1.750262902,	0.723143019,	0.169695369, #N4, 210 Hz, 70 dBA, B1
                   2.456840359,	0.514379033,	3.667829272,	0.706577457,	3.667829272,	2.944686252,	2.456840359,	1.36411872, #N1, 210 Hz, 70 dBA, B2
                   1.754433049,	0,           	3.335000582,	0.925272967,	3.002171892,	2.675535869,	1.580567533,	0.706577457, #N2, 210 Hz, 70 dBA, B2
                   1.417434212,	0.347075565,	2.944686252,	2.087261739,	2.34270718,	  2.675535869,	0.992293402,	0.684074402, #N3, 210 Hz, 70 dBA, B2
                   1.247738843,	0.7579695,	  1.95239285,	  2.611857563,	  0.336998837,  2.087261739,	0.390314329,	0.925272967, #N4, 210 Hz, 70 dBA, B2
                   2.221543233,	0,           	3.667829272,	-0.177380196,	3.335000582,	3.667829272,	3.335000582,	0.506694206, #N1, 210 Hz, 50 dBA, B3
                   2.34270718,	  1.027119883,	2.944686252,	0.169695369,	3.002171892,	3.667829272,	2.264641935,	0.336998837, #N2, 210 Hz, 50 dBA, B3
                   1.36411872,	  0.36957862,	  1.95239285,	 -0.680044317,	1.053653023,	2.611857563,	0.857424514,	0, #N3, 210 Hz, 50 dBA, B3
                   0.7579695,	  0.58827413,	  1.053653023,	-0.177380196,	1.541498916,	3.335000582,	1.58473768,	 -0.706577457) #N4, 210 Hz, 50 dBA, B3
#Exp 2 data
constant_stim_Exp2 <- c( 2.124011669,	1.36411872,	  2.264641935,	3.002171892,	1.417434212,	3.667829272,	2.087261739,	2.675535869,
                   0.878160223,	0.514379033,	2.221543233,	1.931813245,	0.992293402,	2.456840359,	0.925272967,	2.675535869,
                   0.347075565,	0.514379033,	0.514379033,	-0.167303468,	1.070358647,	1.931813245,	0.347075565,	-0.347075565,
                   0.659464712,	0.539273989,	0.925272967,	0.861454599,	0.659464712,	1.58473768,	  0.21869551,	  0.36957862,
                   
                   1.36411872,	2.944686252,	2.944686252,	3.002171892,	2.611857563,	3.667829272,	3.667829272,	1.931813245,
                   1.36411872,	1.094968337,	3.667829272,	0.7579695,  	0.925272967,	2.611857563,	1.094968337,	0.7579695,
                   1.73369734,	1.464546957,	1.053653023,	1.027119883,	1.247738843,	2.34270718,	  0.684074402,	0.410893934,
                   1.194423351,	0.169695369,	0.177380196,	0,           	1.58473768,	  1.272348533,	0.36957862,	  0.7579695,
                   
                   2.124011669,	0.876272826,	0.876272826,	0.514379033,	0.336998837,	2.944686252,	1.58473768,	  1.194423351,
                   0.514379033,	1.210988913,	0.347075565,	0.861454599,	0.169695369,	1.403187337,	0.659464712,	-0.177380196,
                   0.706577457,	0.192198424,	0.347075565,	-0.336998837,	0.177380196,	0.857424514,	0.269150383,	0,
                   0.487845893,	0.336998837,	0.514379033,	0,         	  -0.347075565,	0,	          1.403187337,	-1.053653023,
                   
                   1.931813245,	2.675535869,	2.611857563,	1.73369734,	  2.087261739,	3.335000582,	3.002171892,	2.124011669,
                   1.541498916,	-1.580567533,	0.861454599,	2.221543233,	1.403187337,	1.272348533,	1.541498916,	1.541498916,
                   1.194423351,	0.925272967,  1.027119883,  1.094968337, 	1.247738843,	0.706577457,	1.750262902,	1.053653023,
                   0,           0.336998837,	0.706577457,	-0.514379033,	-0.21869551,	0.169695369,	0,          	0.169695369,
                   
                   2.944686252,	2.611857563,	3.667829272,	1.247738843,	2.264641935,	3.667829272,	2.944686252,	1.58473768,
                   1.210988913,	0.336998837,	2.124011669,	1.027119883,	0.390314329,	3.667829272,	3.335000582,	0.410893934,
                   0.878160223,	1.272348533,	1.053653023,	0.269150383,	0.269150383,	1.73369734,	  0.684074402,	0.539273989,
                   0.21869551,	0.684074402,	0.876272826,	0.21869551,	  0.21869551,	  1.36411872,	  0.336998837,	-0.169695369,
                   
                   1.245851446,	1.95239285,	  1.272348533,	0.684074402,	1.73369734,	  3.335000582,	2.264641935,	0.192198424,
                   0.514379033,	-0.539273989, -0.192198424,	0,    	      0.336998837,	1.027119883,	1.754433049,	-0.680044317,
                   0,         	0.36957862,	  0, 	          -0.514379033,	-1.053653023,	0,      	    1.027119883,	0.347075565,
                   0.169695369,	0.347075565,	0.36957862,	  -0.167303468,	-1.094968337,	0.21869551,	  1.580567533,	-0.21869551)

constant_stim <- c(constant_stim_Exp1, constant_stim_Exp2)

listener = c(rep(c(1,2,3,4,5,6,8,9), 8*3), rep(c(1,2,3,4,5,6,10,11), 8*3))
noise_levs = rep((rep(c(rep(6, 8), rep(8, 8), rep(9, 8), rep(9.5, 8)), 6)), 2)
F0 = rep((c(rep(130, 4*8*3), rep(210, 4*8*3))), 2)
band = rep((rep(c(rep(1, 4*8), rep(2, 4*8), rep(3, 4*8)), 2)), 2)
experiment = c(c(rep(1, 8*3*2*4)), c(rep(2, 8*3*2*4)))

mydata = data.frame(constant_stim=constant_stim, noise_levs = factor(noise_levs), 
                    listener= factor(listener), F0 = factor(F0), band  = factor(band), 
                    experiment = factor(experiment))

mydata$noise_levs <- relevel(mydata$noise_levs, ref = "8") #8,6,9,9.5
mydata$noise_levs <- relevel(mydata$noise_levs, ref = "9") #9,8,6,9.5
mydata$noise_levs <- relevel(mydata$noise_levs, ref = "9.5")

mydata <- mydata[order(-noise_levs),]

priors <- list(muM = 0, muSD = 1)

Data_mat_for_BEST = matrix(
  c(mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 9.5 & mydata$F0 == 130 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 9 & mydata$F0 == 130 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 8 & mydata$F0 == 130 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 6 & mydata$F0 == 130 & mydata$experiment == 1],
    
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 9.5 & mydata$F0 == 130 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 9 & mydata$F0 == 130 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 8 & mydata$F0 == 130 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 6 & mydata$F0 == 130 & mydata$experiment == 1],
    
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 9.5 & mydata$F0 == 130 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 9 & mydata$F0 == 130 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 8 & mydata$F0 == 130 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 6 & mydata$F0 == 130 & mydata$experiment == 1],
    
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 9.5 & mydata$F0 == 210 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 9 & mydata$F0 == 210 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 8 & mydata$F0 == 210 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 6 & mydata$F0 == 210 & mydata$experiment == 1],
    
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 9.5 & mydata$F0 == 210 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 9 & mydata$F0 == 210 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 8 & mydata$F0 == 210 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 6 & mydata$F0 == 210 & mydata$experiment == 1],
    
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 9.5 & mydata$F0 == 210 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 9 & mydata$F0 == 210 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 8 & mydata$F0 == 210 & mydata$experiment == 1],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 6 & mydata$F0 == 210 & mydata$experiment == 1],
    
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 9.5 & mydata$F0 == 130 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 9 & mydata$F0 == 130 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 8 & mydata$F0 == 130 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 6 & mydata$F0 == 130 & mydata$experiment == 2],
    
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 9.5 & mydata$F0 == 130 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 9 & mydata$F0 == 130 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 8 & mydata$F0 == 130 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 6 & mydata$F0 == 130 & mydata$experiment == 2],
    
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 9.5 & mydata$F0 == 130 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 9 & mydata$F0 == 130 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 8 & mydata$F0 == 130 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 6 & mydata$F0 == 130 & mydata$experiment == 2],
    
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 9.5 & mydata$F0 == 210 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 9 & mydata$F0 == 210 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 8 & mydata$F0 == 210 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '1' & mydata$noise_levs == 6 & mydata$F0 == 210 & mydata$experiment == 2],
    
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 9.5 & mydata$F0 == 210 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 9 & mydata$F0 == 210 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 8 & mydata$F0 == 210 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '2' & mydata$noise_levs == 6 & mydata$F0 == 210 & mydata$experiment == 2],
    
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 9.5 & mydata$F0 == 210 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 9 & mydata$F0 == 210 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 8 & mydata$F0 == 210 & mydata$experiment == 2],
    mydata$constant_stim[mydata$band == '3' & mydata$noise_levs == 6 & mydata$F0 == 210 & mydata$experiment == 2]), # the data elements 
  nrow=48,              # number of rows 
  ncol=8,              # number of columns 
  byrow = TRUE)


#Experiment 1 minus 2 for each band
mean(c(mean(Data_mat_for_BEST[1:4,])-mean(Data_mat_for_BEST[25:28,]), mean(Data_mat_for_BEST[13:16,])-mean(Data_mat_for_BEST[37:40,])))
mean(c(mean(Data_mat_for_BEST[5:8,])-mean(Data_mat_for_BEST[29:32,]), mean(Data_mat_for_BEST[17:20,])-mean(Data_mat_for_BEST[41:44,])))
mean(c(mean(Data_mat_for_BEST[9:12,])-mean(Data_mat_for_BEST[33:36,]), mean(Data_mat_for_BEST[21:24,])-mean(Data_mat_for_BEST[45:48,])))

#Experiment 1 minus 2 for each band and F0, B1, B2, B3 for 130H, then B1, B2, B3 for 210Hz, 
mean(Data_mat_for_BEST[1:4,])-mean(Data_mat_for_BEST[25:28,])
mean(Data_mat_for_BEST[5:8,])-mean(Data_mat_for_BEST[29:32,]) 
mean(Data_mat_for_BEST[9:12,])-mean(Data_mat_for_BEST[33:36,])

mean(Data_mat_for_BEST[13:16,])-mean(Data_mat_for_BEST[37:40,])
mean(Data_mat_for_BEST[17:20,])-mean(Data_mat_for_BEST[41:44,])
mean(Data_mat_for_BEST[21:24,])-mean(Data_mat_for_BEST[45:48,])

BEST_OUTPUT_EXPERIS <- matrix(0, nrow = 24, ncol = 9);
for(i in 1:24) {
  BESTout <- BESTmcmc((Data_mat_for_BEST[i,]-Data_mat_for_BEST[i+24,]), priors=priors, parallel=FALSE)
  BEST_OUTPUT_EXPERIS[i,1] = median(BESTout$mu)
  BEST_OUTPUT_EXPERIS[i,2] = HDInterval::hdi(BESTout$mu, credMass = 0.9)[[1]]
  BEST_OUTPUT_EXPERIS[i,3] = HDInterval::hdi(BESTout$mu, credMass = 0.9)[[2]]
  BEST_OUTPUT_EXPERIS[i,4] = median(BESTout$sigma)
  BEST_OUTPUT_EXPERIS[i,5] = HDInterval::hdi(BESTout$sigma, credMass = 0.9)[[1]]
  BEST_OUTPUT_EXPERIS[i,6] = HDInterval::hdi(BESTout$sigma, credMass = 0.9)[[2]]
  beta <- BESTout$mu/BESTout$sigma
  BEST_OUTPUT_EXPERIS[i,7] = median(beta)
  BEST_OUTPUT_EXPERIS[i,8] = HDInterval::hdi(beta, credMass = 0.9)[[1]]
  BEST_OUTPUT_EXPERIS[i,9] = HDInterval::hdi(beta, credMass = 0.9)[[2]]
}

mydata2 = mydata[mydata$listener != 8,]
mydata2 = mydata2[mydata2$listener != 9,]
mydata2 = mydata2[mydata2$listener != 10,]
mydata2 = mydata2[mydata2$listener != 11,]

Data_mat_for_BEST2 = matrix(
  c(mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 9.5 & mydata2$F0 == 130 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 9 & mydata2$F0 == 130 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 8 & mydata2$F0 == 130 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 6 & mydata2$F0 == 130 & mydata2$experiment == 1],
    
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 9.5 & mydata2$F0 == 130 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 9 & mydata2$F0 == 130 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 8 & mydata2$F0 == 130 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 6 & mydata2$F0 == 130 & mydata2$experiment == 1],
    
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 9.5 & mydata2$F0 == 130 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 9 & mydata2$F0 == 130 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 8 & mydata2$F0 == 130 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 6 & mydata2$F0 == 130 & mydata2$experiment == 1],
    
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 9.5 & mydata2$F0 == 210 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 9 & mydata2$F0 == 210 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 8 & mydata2$F0 == 210 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 6 & mydata2$F0 == 210 & mydata2$experiment == 1],
    
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 9.5 & mydata2$F0 == 210 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 9 & mydata2$F0 == 210 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 8 & mydata2$F0 == 210 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 6 & mydata2$F0 == 210 & mydata2$experiment == 1],
    
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 9.5 & mydata2$F0 == 210 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 9 & mydata2$F0 == 210 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 8 & mydata2$F0 == 210 & mydata2$experiment == 1],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 6 & mydata2$F0 == 210 & mydata2$experiment == 1],
    
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 9.5 & mydata2$F0 == 130 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 9 & mydata2$F0 == 130 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 8 & mydata2$F0 == 130 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 6 & mydata2$F0 == 130 & mydata2$experiment == 2],
    
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 9.5 & mydata2$F0 == 130 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 9 & mydata2$F0 == 130 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 8 & mydata2$F0 == 130 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 6 & mydata2$F0 == 130 & mydata2$experiment == 2],
    
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 9.5 & mydata2$F0 == 130 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 9 & mydata2$F0 == 130 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 8 & mydata2$F0 == 130 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 6 & mydata2$F0 == 130 & mydata2$experiment == 2],
    
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 9.5 & mydata2$F0 == 210 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 9 & mydata2$F0 == 210 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 8 & mydata2$F0 == 210 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '1' & mydata2$noise_levs == 6 & mydata2$F0 == 210 & mydata2$experiment == 2],
    
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 9.5 & mydata2$F0 == 210 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 9 & mydata2$F0 == 210 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 8 & mydata2$F0 == 210 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '2' & mydata2$noise_levs == 6 & mydata2$F0 == 210 & mydata2$experiment == 2],
    
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 9.5 & mydata2$F0 == 210 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 9 & mydata2$F0 == 210 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 8 & mydata2$F0 == 210 & mydata2$experiment == 2],
    mydata2$constant_stim[mydata2$band == '3' & mydata2$noise_levs == 6 & mydata2$F0 == 210 & mydata2$experiment == 2]), # the data elements 
  nrow=48,              # number of rows 
  ncol=6,              # number of columns 
  byrow = TRUE)

BEST_OUTPUT_EXPERIS2 <- matrix(0, nrow = 24, ncol = 9);
#Compare 1 to 7, 2 to 8 etc through to 6 to 12
for(i in 1:24) {
  BESTout <- BESTmcmc((Data_mat_for_BEST2[i,]-Data_mat_for_BEST2[i+24,]), priors=priors, parallel=FALSE)
  BEST_OUTPUT_EXPERIS2[i,1] = median(BESTout$mu)
  BEST_OUTPUT_EXPERIS2[i,2] = HDInterval::hdi(BESTout$mu, credMass = 0.9)[[1]]
  BEST_OUTPUT_EXPERIS2[i,3] = HDInterval::hdi(BESTout$mu, credMass = 0.9)[[2]]
  BEST_OUTPUT_EXPERIS2[i,4] = median(BESTout$sigma)
  BEST_OUTPUT_EXPERIS2[i,5] = HDInterval::hdi(BESTout$sigma, credMass = 0.9)[[1]]
  BEST_OUTPUT_EXPERIS2[i,6] = HDInterval::hdi(BESTout$sigma, credMass = 0.9)[[2]]
  beta <- BESTout$mu/BESTout$sigma
  BEST_OUTPUT_EXPERIS2[i,7] = median(beta)
  BEST_OUTPUT_EXPERIS2[i,8] = HDInterval::hdi(beta, credMass = 0.9)[[1]]
  BEST_OUTPUT_EXPERIS2[i,9] = HDInterval::hdi(beta, credMass = 0.9)[[2]]
}

#-----------------------------------------------------------

iters = 20000;
model0B.model <- brm(constant_stim ~ (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept")),
                     warmup = floor(iters*0.2))
model0B.model.loo <- loo(model0B.model)

model1B.model <- brm(constant_stim ~ noise_levs + (1|listener), data=mydata, 
                    iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                    prior = c(set_prior("normal(0,1)", class = "Intercept"),
                              set_prior("normal(0,1)", class = "b")),
                    warmup = floor(iters*0.2))
model1B.model.loo <- loo(model1B.model)
loo_compare(model0B.model.loo, model1B.model.loo)
#elpd_diff se_diff
#model1B.model   0.0       0.0  
#model0B.model -84.2      12.7  

model2B.model <- brm(constant_stim ~ noise_levs + experiment + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept"),
                               set_prior("normal(0,1)", class = "b")),
                     warmup = floor(iters*0.2))
model2B.model.loo <- loo(model2B.model)
loo_compare(model1B.model.loo, model2B.model.loo)
#elpd_diff se_diff
#model2B.model   0.0       0.0  
#model1B.model -17.1       6.0  

model3B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept"),
                               set_prior("normal(0,1)", class = "b")),
                     warmup = floor(iters*0.2))
model3B.model.loo <- loo(model3B.model)
loo_compare(model2B.model.loo, model3B.model.loo)
#elpd_diff se_diff
#model3B.model  0.0       0.0   
#model2B.model -0.8       3.0   

model4B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept"),
                               set_prior("normal(0,1)", class = "b")),
                     warmup = floor(iters*0.2))
model4B.model.loo <- loo(model4B.model)
loo_compare(model3B.model.loo, model4B.model.loo)
#elpd_diff se_diff
#model4B.model   0.0       0.0  
#model3B.model -13.8       5.2 

model5B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:noise_levs + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept"),
                               set_prior("normal(0,1)", class = "b")),
                     warmup = floor(iters*0.2))
model5B.model.loo <- loo(model5B.model)
loo_compare(model4B.model.loo, model5B.model.loo)
#elpd_diff se_diff
#model4B.model  0.0       0.0   
#model5B.model -4.6       2.0  

model6B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept"),
                               set_prior("normal(0,1)", class = "b")),
                     warmup = floor(iters*0.2))
model6B.model.loo <- loo(model6B.model)
loo_compare(model4B.model.loo, model6B.model.loo)
#elpd_diff se_diff
#model6B.model  0.0       0.0   
#model4B.model -6.1       3.9  

model7B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0 + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept"),
                               set_prior("normal(0,1)", class = "b")),
                     warmup = floor(iters*0.2))
model7B.model.loo <- loo(model7B.model)
loo_compare(model6B.model.loo, model7B.model.loo)
#elpd_diff se_diff
#model6B.model  0.0       0.0   
#model7B.model -0.9       0.5 

model8B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0:noise_levs +  (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept"),
                               set_prior("normal(0,1)", class = "b")),
                     warmup = floor(iters*0.2))
model8B.model.loo <- loo(model8B.model)
loo_compare(model6B.model.loo, model8B.model.loo)
#elpd_diff se_diff
#model8B.model  0.0       0.0   
#model6B.model -1.8       3.4   

model9B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0:noise_levs + F0:experiment + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept"),
                               set_prior("normal(0,1)", class = "b")),
                     warmup = floor(iters*0.2))
model9B.model.loo <- loo(model9B.model)
loo_compare(model8B.model.loo, model9B.model.loo)
#elpd_diff se_diff
#model8B.model  0.0       0.0   
#model9B.model -0.5       1.1 

model10B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0:noise_levs + F0:experiment + F0:band + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept"),
                               set_prior("normal(0,1)", class = "b")),
                     warmup = floor(iters*0.2))
model10B.model.loo <- loo(model10B.model)
loo_compare(model9B.model.loo, model10B.model.loo)
#elpd_diff se_diff
#model9B.model   0.0       0.0   
#model10B.model -2.1       0.4   

model11B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0:noise_levs + F0:experiment + noise_levs:experiment:band + (1|listener), data=mydata, 
                      iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                      prior = c(set_prior("normal(0,1)", class = "Intercept"),
                                set_prior("normal(0,1)", class = "b")),
                      warmup = floor(iters*0.2))
model11B.model.loo <- loo(model11B.model)
loo_compare(model9B.model.loo, model11B.model.loo)
#elpd_diff se_diff
#model9B.model   0.0       0.0   
#model11B.model -6.0       2.8   

model12B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0:noise_levs + F0:experiment + noise_levs:experiment:F0 + (1|listener), data=mydata, 
                      iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                      prior = c(set_prior("normal(0,1)", class = "Intercept"),
                                set_prior("normal(0,1)", class = "b")),
                      warmup = floor(iters*0.2))
model12B.model.loo <- loo(model12B.model)
loo_compare(model9B.model.loo, model12B.model.loo)
#elpd_diff se_diff
#model12B.model  0.0       0.0   
#model9B.model  -6.8       3.0

model13B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0:noise_levs + F0:experiment + noise_levs:experiment:F0 + band:experiment:F0 + (1|listener), data=mydata, 
                      iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                      prior = c(set_prior("normal(0,1)", class = "Intercept"),
                                set_prior("normal(0,1)", class = "b")),
                      warmup = floor(iters*0.2))
model13B.model.loo <- loo(model13B.model)
loo_compare(model12B.model.loo, model13B.model.loo)
#elpd_diff se_diff
#model12B.model  0.0       0.0   
#model13B.model -3.6       0.6   

model14B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0:noise_levs + F0:experiment + noise_levs:experiment:F0 + noise_levs:experiment:F0:band + (1|listener), data=mydata, 
                      iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                      prior = c(set_prior("normal(0,1)", class = "Intercept"),
                                set_prior("normal(0,1)", class = "b")),
                      warmup = floor(iters*0.2))
model14B.model.loo <- loo(model14B.model)
loo_compare(model12B.model.loo, model14B.model.loo)
#elpd_diff se_diff
#model12B.model   0.0       0.0  
#model14B.model -14.2       4.0  

model15B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0:noise_levs + F0:experiment + noise_levs:experiment:F0 + (noise_levs|listener), data=mydata, 
                      iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                      prior = c(set_prior("normal(0,1)", class = "Intercept"),
                                set_prior("normal(0,1)", class = "b"),
                                set_prior("lkj(2)", class = "cor")),
                      warmup = floor(iters*0.2))
model15B.model.loo <- loo(model15B.model)
loo_compare(model12B.model.loo, model15B.model.loo)
#elpd_diff se_diff
#model15B.model  0.0       0.0   
#model12B.model -5.5       4.1   

model16B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0:noise_levs + F0:experiment + noise_levs:experiment:F0 + (noise_levs + experiment|listener), data=mydata, 
                      iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                      prior = c(set_prior("normal(0,1)", class = "Intercept"),
                                set_prior("normal(0,1)", class = "b"),
                                set_prior("lkj(2)", class = "cor")),
                      warmup = floor(iters*0.2))
model16B.model.loo <- loo(model16B.model)
loo_compare(model15B.model.loo, model16B.model.loo)
#elpd_diff se_diff
#model16B.model   0.0       0.0  
#model15B.model -14.7       5.8 

model17B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0:noise_levs + F0:experiment + noise_levs:experiment:F0 + (noise_levs + experiment + band|listener), data=mydata, 
                      iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                      prior = c(set_prior("normal(0,1)", class = "Intercept"),
                                set_prior("normal(0,1)", class = "b"),
                                set_prior("lkj(2)", class = "cor")),
                      warmup = floor(iters*0.2))
model17B.model.loo <- loo(model17B.model)
loo_compare(model16B.model.loo, model17B.model.loo)
#elpd_diff se_diff
#model17B.model  0.0       0.0   
#model16B.model -5.6       4.2  

model18B.model <- brm(constant_stim ~ noise_levs + experiment + noise_levs:experiment + band + band:experiment + F0:noise_levs + F0:experiment + noise_levs:experiment:F0 + (noise_levs + experiment + band + F0|listener), data=mydata, 
                      iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                      prior = c(set_prior("normal(0,1)", class = "Intercept"),
                                set_prior("normal(0,1)", class = "b"),
                                set_prior("lkj(2)", class = "cor")),
                      warmup = floor(iters*0.2))
model18B.model.loo <- loo(model18B.model)
loo_compare(model17B.model.loo, model18B.model.loo)
#elpd_diff se_diff
#model18B.model  0.0       0.0   
#model17B.model -3.8       3.5   

tidy_stan_results18 = parameters::model_parameters(model18B.model, effects = "all", component = "all",
                                                   centrality = "median", dispersion = "TRUE", ci = 0.90, ci_method = "hdi", 
                                                   diagnostic = "all")

thedrawsIntercept = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_Intercept"]][((iters*0.2+1):iters)],
                      model18B.model[["fit"]]@sim[["samples"]][[2]][["b_Intercept"]][((iters*0.2+1):iters)],
                      model18B.model[["fit"]]@sim[["samples"]][[3]][["b_Intercept"]][((iters*0.2+1):iters)],
                      model18B.model[["fit"]]@sim[["samples"]][[4]][["b_Intercept"]][((iters*0.2+1):iters)])
thedrawsN1 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs9"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs9"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs9"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs9"]][((iters*0.2+1):iters)])
thedrawsN2 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs8"]][((iters*0.2+1):iters)],
              model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs8"]][((iters*0.2+1):iters)],
              model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs8"]][((iters*0.2+1):iters)],
              model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs8"]][((iters*0.2+1):iters)])
thedrawsN4 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs6"]][((iters*0.2+1):iters)],
              model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs6"]][((iters*0.2+1):iters)],
              model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs6"]][((iters*0.2+1):iters)],
              model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs6"]][((iters*0.2+1):iters)])
thedrawsexperiment = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_experiment2"]][((iters*0.2+1):iters)],
                      model18B.model[["fit"]]@sim[["samples"]][[2]][["b_experiment2"]][((iters*0.2+1):iters)],
                      model18B.model[["fit"]]@sim[["samples"]][[3]][["b_experiment2"]][((iters*0.2+1):iters)],
                      model18B.model[["fit"]]@sim[["samples"]][[4]][["b_experiment2"]][((iters*0.2+1):iters)])
thedrawsband2 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_band2"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[2]][["b_band2"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[3]][["b_band2"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[4]][["b_band2"]][((iters*0.2+1):iters)])
thedrawsband3 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_band3"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[2]][["b_band3"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[3]][["b_band3"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[4]][["b_band3"]][((iters*0.2+1):iters)])
thedrawsN1experiment = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs9:experiment2"]][((iters*0.2+1):iters)],
               model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs9:experiment2"]][((iters*0.2+1):iters)],
               model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs9:experiment2"]][((iters*0.2+1):iters)],
               model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs9:experiment2"]][((iters*0.2+1):iters)])
thedrawsN2experiment = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs8:experiment2"]][((iters*0.2+1):iters)],
               model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs8:experiment2"]][((iters*0.2+1):iters)],
               model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs8:experiment2"]][((iters*0.2+1):iters)],
               model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs8:experiment2"]][((iters*0.2+1):iters)])
thedrawsN4experiment = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs6:experiment2"]][((iters*0.2+1):iters)],
               model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs6:experiment2"]][((iters*0.2+1):iters)],
               model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs6:experiment2"]][((iters*0.2+1):iters)],
               model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs6:experiment2"]][((iters*0.2+1):iters)])
thedrawsband2experiment = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_experiment2:band2"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[2]][["b_experiment2:band2"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[3]][["b_experiment2:band2"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[4]][["b_experiment2:band2"]][((iters*0.2+1):iters)])
thedrawsband3experiment = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_experiment2:band3"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[2]][["b_experiment2:band3"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[3]][["b_experiment2:band3"]][((iters*0.2+1):iters)],
                  model18B.model[["fit"]]@sim[["samples"]][[4]][["b_experiment2:band3"]][((iters*0.2+1):iters)])
thedrawsN05F0210 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs9.5:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs9.5:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs9.5:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs9.5:F0210"]][((iters*0.2+1):iters)])
thedrawsN1F0210 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs9:F0210"]][((iters*0.2+1):iters)],
                         model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs9:F0210"]][((iters*0.2+1):iters)],
                         model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs9:F0210"]][((iters*0.2+1):iters)],
                         model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs9:F0210"]][((iters*0.2+1):iters)])
thedrawsN2F0210 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs8:F0210"]][((iters*0.2+1):iters)],
                         model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs8:F0210"]][((iters*0.2+1):iters)],
                         model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs8:F0210"]][((iters*0.2+1):iters)],
                         model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs8:F0210"]][((iters*0.2+1):iters)])
thedrawsN4F0210 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs6:F0210"]][((iters*0.2+1):iters)],
                         model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs6:F0210"]][((iters*0.2+1):iters)],
                         model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs6:F0210"]][((iters*0.2+1):iters)],
                         model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs6:F0210"]][((iters*0.2+1):iters)])
thedrawsF0210experiment = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[2]][["b_experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[3]][["b_experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[4]][["b_experiment2:F0210"]][((iters*0.2+1):iters)])
thedrawsN1F0210 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs9:experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs9:experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs9:experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs9:experiment2:F0210"]][((iters*0.2+1):iters)])
thedrawsN2F0210 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs8:experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs8:experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs8:experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs8:experiment2:F0210"]][((iters*0.2+1):iters)])
thedrawsN4F0210 = c(model18B.model[["fit"]]@sim[["samples"]][[1]][["b_noise_levs6:experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[2]][["b_noise_levs6:experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[3]][["b_noise_levs6:experiment2:F0210"]][((iters*0.2+1):iters)],
                    model18B.model[["fit"]]@sim[["samples"]][[4]][["b_noise_levs6:experiment2:F0210"]][((iters*0.2+1):iters)])

#Below not complete.
B1_N05_F0130_exp1_draws = thedrawsIntercept
B1_N1_F0130_exp1_draws = thedrawsIntercept + thedrawsNL1
B1_N2_F0130_exp1_draws = thedrawsIntercept + thedrawsNL2
B1_N4_F0130_exp1_draws = thedrawsIntercept + thedrawsNL3

B2_N05_F0130_exp1_draws = thedrawsIntercept + thedrawsband2
B2_N1_F0130_exp1_draws = thedrawsIntercept + thedrawsband2 + thedrawsNL1
B2_N2_F0130_exp1_draws = thedrawsIntercept + thedrawsband2 + thedrawsNL2
B2_N4_F0130_exp1_draws = thedrawsIntercept + thedrawsband2 + thedrawsNL3

B3_N05_F0130_exp1_draws = thedrawsIntercept + thedrawsband3
B3_N1_F0130_exp1_draws = thedrawsIntercept + thedrawsband3 + thedrawsNL1
B3_N2_F0130_exp1_draws = thedrawsIntercept + thedrawsband3 + thedrawsNL2
B3_N4_F0130_exp1_draws = thedrawsIntercept + thedrawsband3 + thedrawsNL3

B1_N05_F0210_exp1_draws = thedrawsIntercept + thedrawsN05F0210
B1_N1_F0210_exp1_draws = thedrawsIntercept + thedrawsNL1 + thedrawsN1F0210
B1_N2_F0210_exp1_draws = thedrawsIntercept + thedrawsNL2 + thedrawsN2F0210
B1_N4_F0210_exp1_draws = thedrawsIntercept + thedrawsNL3 + thedrawsN4F0210

B2_N05_F0210_exp1_draws = thedrawsIntercept + thedrawsband2 + thedrawsN05F0210
B2_N1_F0210_exp1_draws = thedrawsIntercept + thedrawsband2 + thedrawsNL1 + thedrawsN1F0210
B2_N2_F0210_exp1_draws = thedrawsIntercept + thedrawsband2 + thedrawsNL2 + thedrawsN2F0210
B2_N4_F0210_exp1_draws = thedrawsIntercept + thedrawsband2 + thedrawsNL3 + thedrawsN4F0210

B3_N05_F0210_exp1_draws = thedrawsIntercept + thedrawsband3 + thedrawsN05F0210
B3_N1_F0210_exp1_draws = thedrawsIntercept + thedrawsband3 + thedrawsNL1 + thedrawsN1F0210
B3_N2_F0210_exp1_draws = thedrawsIntercept + thedrawsband3 + thedrawsNL2 + thedrawsN2F0210
B3_N4_F0210_exp1_draws = thedrawsIntercept + thedrawsband3 + thedrawsNL3 + thedrawsN4F0210



B1_N05_F0130_exp2_draws = thedrawsIntercept + thedrawsexperiment 
B1_N1_F0130_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN1experiment + thedrawsNL1
B1_N2_F0130_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN2experiment + thedrawsNL2
B1_N4_F0130_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN4experiment + thedrawsNL3

B2_N05_F0130_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsband2
B2_N1_F0130_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN1experiment + thedrawsband2 + thedrawsNL1
B2_N2_F0130_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN2experiment + thedrawsband2 + thedrawsNL2
B2_N4_F0130_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN4experiment + thedrawsband2 + thedrawsNL3

B3_N05_F0130_exp12_draws = thedrawsIntercept + thedrawsexperiment + thedrawsband3
B3_N1_F0130_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN1experiment + thedrawsband3 + thedrawsNL1
B3_N2_F0130_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN2experiment + thedrawsband3 + thedrawsNL2
B3_N4_F0130_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN4experiment + thedrawsband3 + thedrawsNL3

B1_N05_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN05F0210
B1_N1_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN1experiment + thedrawsNL1 + thedrawsN1F0210
B1_N2_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN2experiment + thedrawsNL2 + thedrawsN2F0210
B1_N4_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN4experiment + thedrawsNL3 + thedrawsN4F0210

B2_N05_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsband2 + thedrawsN05F0210
B2_N1_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN1experiment + thedrawsband2 + thedrawsNL1 + thedrawsN1F0210
B2_N2_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN2experiment + thedrawsband2 + thedrawsNL2 + thedrawsN2F0210
B2_N4_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN4experiment + thedrawsband2 + thedrawsNL3 + thedrawsN4F0210

B3_N05_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsband3 + thedrawsN05F0210
B3_N1_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN1experiment + thedrawsband3 + thedrawsNL1 + thedrawsN1F0210
B3_N2_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN2experiment + thedrawsband3 + thedrawsNL2 + thedrawsN2F0210
B3_N4_F0210_exp2_draws = thedrawsIntercept + thedrawsexperiment + thedrawsN4experiment + thedrawsband3 + thedrawsNL3 + thedrawsN4F0210


#Only those who participate in both experiments

mydata2 = mydata[mydata$listener != 8,]
mydata2 = mydata2[mydata2$listener != 9,]
mydata2 = mydata2[mydata2$listener != 10,]
mydata2 = mydata2[mydata2$listener != 11,]

#-----------------------------------------------------------

iters = 20000;
model0B.model.subset <- brm(constant_stim ~ (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept")),
                     warmup = floor(iters*0.2))
model0B.model.loo <- loo(model0B.model)

model1B.model.subset <- brm(constant_stim ~ experiment + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept")),
                     warmup = floor(iters*0.2))
model1B.model.loo <- loo(model1B.model)
loo_compare(model0B.model.loo, model1B.model.loo)
#elpd_diff se_diff
#model1B.model   0.0       0.0  
#model0B.model -10.2      15.0  

model2B.model.subset <- brm(constant_stim ~ experiment + band + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept")),
                     warmup = floor(iters*0.2))
model2B.model.loo <- loo(model2B.model)
loo_compare(model1B.model.loo, model2B.model.loo)
#elpd_diff se_diff
#model2B.model  0.0       0.0   
#model1B.model -7.4       4.1   

model3B.model.subset <- brm(constant_stim ~ experiment + band + experiment:band + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept")),
                     warmup = floor(iters*0.2))
model3B.model.loo <- loo(model3B.model)
loo_compare(model2B.model.loo, model3B.model.loo)
#elpd_diff se_diff
#model3B.model  0.0       0.0   
#model2B.model -2.8       2.9  

model4B.model.subset <- brm(constant_stim ~ experiment + band + experiment:band + F0 + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept")),
                     warmup = floor(iters*0.2))
model4B.model.loo <- loo(model4B.model)
loo_compare(model3B.model.loo, model4B.model.loo)
#elpd_diff se_diff
#model3B.model  0.0       0.0   
#model4B.model -1.0       0.4   

model5B.model.subset <- brm(constant_stim ~ experiment + band + experiment:band + F0:experiment + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept")),
                     warmup = floor(iters*0.2))
model5B.model.loo <- loo(model5B.model)
loo_compare(model3B.model.loo, model5B.model.loo)
#elpd_diff se_diff
#model3B.model  0.0       0.0   
#model5B.model -1.6       0.8 

model6B.model.subset <- brm(constant_stim ~ experiment + band + experiment:band + F0:band + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept")),
                     warmup = floor(iters*0.2))
model6B.model.loo <- loo(model6B.model)
loo_compare(model3B.model.loo, model6B.model.loo)
#elpd_diff se_diff
#model3B.model  0.0       0.0   
#model6B.model -3.0       0.5   

model7B.model.subset <- brm(constant_stim ~ experiment + band + experiment:band + F0:band:experiment + (1|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept")),
                     warmup = floor(iters*0.2))
model7B.model.loo <- loo(model7B.model)
loo_compare(model3B.model.loo, model7B.model.loo)

model8B.model.subset <- brm(constant_stim ~ experiment + band + experiment:band + (band|listener), data=mydata, 
                     iter = iters, control = list(adapt_delta = 0.99), init = 0, 
                     prior = c(set_prior("normal(0,1)", class = "Intercept")),
                     warmup = floor(iters*0.2))
model8B.model.loo <- loo(model7B.model)
loo_compare(model7B.model.loo, model8B.model.loo)

tidy_stan_results19 = parameters::model_parameters(model19B.model, effects = "all", component = "all",
                                                   centrality = "median", dispersion = "TRUE", ci = 0.90, ci_method = "hdi", 
                                                   diagnostic = "all")



mean(thresholds[mydata$band == 1 & mydata$experi == 1 & mydata$F0 == 130])
