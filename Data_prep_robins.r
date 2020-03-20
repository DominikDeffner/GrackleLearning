library(readr)

d <- read_csv("~/GitHub/Grackles_Reinforcement/data_NZrobinReversal.csv")


setwd("C:/Users/dominik_deffner/Documents/GitHub/Grackles_Reinforcement")

d$id <- sapply(1:nrow(d), function (i) which(unique(d$BirdID) == d$BirdID[i]) )

d <- d[-which(is.na(d$Trial)),]

d <- d[-which(d$Trial=="Probe"),]

d <- d[-which(d$Trial=="Ignore"),]
d <- d[-which(d$Correct=="?"),]


d <- d[with(d, order(d$id)), ]
d$Choice <- NA
for (i in 1: nrow(d)) {
  if (d$Experiment[i] == "Colour assoc."){
    if (d$Correct[i] == "Y"){
      d$Choice[i] <- 1
    } else {
      d$Choice[i] <- 2
    } 
  } else {
    if (d$Correct[i] == "Y"){
      d$Choice[i] <- 2
    } else {
      d$Choice[i] <- 1
    } 
  }
}


d[d=="Colour assoc."] <- 0
d[d=="Colour reversal"] <- 1

d[d=="N"] <- 0
d[d=="Y"] <- 1

d$Correct <- as.integer(d$Correct)
d$Experiment <- as.integer(d$Experiment)
d$Trial <- as.integer(d$Trial)


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


m1 <- stan( file="Grackle_reinforcement.stan" , data=dat_initial  ,iter = 5000, cores = 4, chains=4, control = list(adapt_delta=0.99, max_treedepth = 12))  #now with varying effects  #Individual learning only
m2 <- stan( file="Grackle_multilevel_reinforcement.stan" , data=dat ,iter = 5000, cores = 4, chains=4, control = list(adapt_delta=0.99, max_treedepth = 12))  #now with varying effects
m3 <- stan( file="Grackle_multilevel_centered_reinforcement.stan" , data=dat ,iter = 10000, cores = 4, chains=4, control = list(adapt_delta=0.99, max_treedepth = 12))  #now with varying effects
m4 <- stan( file="Grackle_multilevel_contrast_reinforcement.stan" , data=dat ,iter = 1000, cores = 4, chains=4, control = list(adapt_delta=0.9, max_treedepth = 12))  #now with varying effects
m5 <- stan( file="Grackle_multilevel_fixed_L_reinforcement.stan" , data=dat ,iter = 2500, cores = 4, chains=4, control = list(adapt_delta=0.9, max_treedepth = 12))  #now with varying effects



