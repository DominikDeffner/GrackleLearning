library(readr)
d <- read_csv("~/GitHub/Grackles_Reinforcement/corina_logan.23.1.csv")



setwd("C:/Users/dominik_deffner/Documents/GitHub/Grackles_Reinforcement")

d$id <- sapply(1:nrow(d), function (i) which(unique(d$Bird) == d$Bird[i]) )
d <- d[-which(d$Experiment=="Refresher"),]


d <- d[with(d, order(d$id)), ]
d$Choice <- NA
for (i in 1: nrow(d)) {
  if (d$Experiment[i] == "Initial"){
    if (d$Correct[i] == 1){
      d$Choice[i] <- 1
    } else {
      d$Choice[i] <- 2
    } 
  } else {
    if (d$Correct[i] == 1){
      d$Choice[i] <- 2
    } else {
      d$Choice[i] <- 1
    } 
  }
}

d[d=="Initial"] <- 0
d[d=="Reverse"] <- 1
d$Experiment <- as.integer(d$Experiment)


d <- subset(d, select = -c(Date,Bird, Batch, `Preference? Notes`, `NonoverlappingWindow4-trialbins`,Criterion, ColorOnLeft,CorrectChoice))

dat <- as.list(d)
dat$N <- nrow(d)
dat$N_id <- length(unique(d$id))


# Plot learning curves
Prop_correct_initial <- c()
Prop_correct_reversal <- c()

for (i in 1:100) {
  Prop_correct_initial[i] <- sum(d$Correct[d$Trial==i & d$Experiment == 0]) / length(d$Correct[d$Trial==i& d$Experiment == 0 ]) 
  Prop_correct_reversal[i] <- sum(d$Correct[d$Trial==i & d$Experiment == 1]) / length(d$Correct[d$Trial==i& d$Experiment == 1]) 
}

#par(mfrow= c(1,2))
#plot(c(Prop_correct_initial[1:50], Prop_correct_reversal[1:50]), type = "b", ylim = c(0,1))
#plot(Prop_correct_reversal, type = "b", ylim = c(0,1))


m1 <- stan( file="Grackle_reinforcement.stan" , data=dat_initial  ,iter = 5000, cores = 4, chains=4, control = list(adapt_delta=0.99, max_treedepth = 12))  #now with varying effects  #Individual learning only
m2 <- stan( file="Grackle_multilevel_reinforcement.stan" , data=dat ,iter = 5000, cores = 4, chains=4, control = list(adapt_delta=0.99, max_treedepth = 12))  #now with varying effects
m3 <- stan( file="Grackle_multilevel_centered_reinforcement.stan" , data=dat ,iter = 10000, cores = 4, chains=4, control = list(adapt_delta=0.99, max_treedepth = 12))  #now with varying effects
m4 <- stan( file="Grackle_multilevel_contrast_reinforcement.stan" , data=dat ,iter = 5000, cores = 4, chains=4, control = list(adapt_delta=0.9, max_treedepth = 12))  #now with varying effects
m5 <- stan( file="Grackle_multilevel_fixed_L_reinforcement.stan" , data=dat ,iter = 2500, cores = 4, chains=4, control = list(adapt_delta=0.9, max_treedepth = 12))  #now with varying effects

s <- extract.samples(m4)

Ind_Parameters <- matrix(0, dat$N_id, 4)
for (i in 1 : dat$N_id) {
  Ind_Parameters[i , 1] <- exp( mean(s$b0_L) + mean(s$v_ID[, i, 1]) )
  Ind_Parameters[i , 2] <- exp( (mean(s$b0_L) + mean(s$v_ID[, i, 1])) + (mean(s$b1_L) + mean(s$v_ID[, i, 2])))
  
  Ind_Parameters[i , 3] <- inv_logit( mean(s$b0_phi) + mean(s$v_ID[, i, 3]) )
  Ind_Parameters[i , 4] <- inv_logit( (mean(s$b0_phi) + mean(s$v_ID[, i, 3])) + (mean(s$b1_phi) + mean(s$v_ID[, i, 4])))
}

