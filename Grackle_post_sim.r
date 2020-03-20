

# Post-hoc Simulation for Grackle reversal learning




s <- extract.samples(m4)

Ind_Parameters <- matrix(0, dat$N_id, 4)
for (i in 1 : dat$N_id) {
  Ind_Parameters[i , 1] <- exp( mean(s$b0_L) + mean(s$v_ID[, i, 1]) )
  Ind_Parameters[i , 2] <- exp( (mean(s$b0_L) + mean(s$v_ID[, i, 1])) + (mean(s$b1_L) + mean(s$v_ID[, i, 2])))
  
  Ind_Parameters[i , 3] <- inv_logit( mean(s$b0_phi) + mean(s$v_ID[, i, 3]) )
  Ind_Parameters[i , 4] <- inv_logit( (mean(s$b0_phi) + mean(s$v_ID[, i, 3])) + (mean(s$b1_phi) + mean(s$v_ID[, i, 4])))
}


#Simulation function
Sim_fct <- function(Tmax = 100,  #Number of trials, payoffs switch after 50 rounds
                    N_id = 8){   #Updating/learning rate  
  
  #Create output object
  
  d_Overall <- c()
  
  #Loop over birds
  
  for (ind in 1:N_id) {
    
    #Set initial attractions to 0
    A <- c(0,0)
    
    #Create output matrix to record choices and payoffs
    
    d <- data.frame(id=ind, trial = 1:Tmax, Choice=NA, Payoff=NA, Experiment = NA)
    
    d$Experiment[1:50] <- 0
    d$Experiment[51:100] <- 1
    
    
    # Start simulation loop
    for (i in 1:Tmax) {
      
      if ( i <= 50) {
        lambda <- Ind_Parameters[ind, 1]
        phi    <- Ind_Parameters[ind, 3]
      } else {
        lambda <- Ind_Parameters[ind, 2]
        phi    <- Ind_Parameters[ind, 4]
      }
      
      Prob <- c()
      for (xx in 1:2) {
        Prob[xx] <- exp(lambda*A[xx]) / sum(exp(lambda*A))
      }
      
      
      #Make choice proportional to attraction scores
      d$Choice[which(d$id==ind & d$trial==i)] <- sample(c(1:2), size = 1, prob = Prob)
      
      pay <- c(0,0)
      
      if (d$Experiment[i] == 0){
        if (d$Choice[i] == 1){
          d$Payoff[i] <- 1
          pay[1] <- 1
        } else {
          d$Payoff[i] <- 0
        } 
      } else {
        if (d$Choice[i] == 1){
          d$Payoff[i] <- 0
        } else {
          d$Payoff[i] <- 1
          pay[2] <- 1
        } 
      }
      
      
      #Update Attractions
      for (xx in 1:2) {
        A[xx] <- (1-phi) * A[xx] + phi * pay[xx]
      }
      
      
    }#time i
    
    d_Overall <- rbind(d_Overall, d)
    
  }#ind
  return(d_Overall)
}


dat <- Sim_fct() 

# Plot learning curves
Prop_correct<- c()

for (i in 1:100) {
  Prop_correct[i]  <- sum(dat$Payoff[dat$trial==i]) / length(dat$Payoff[dat$trial==i]) 
}



par(mfrow= c(1,2))
plot(c(Prop_correct_initial[1:50], Prop_correct_reversal[1:50]),xlab = "Trial", ylab = "Proportion Correct", type = "b", ylim = c(0,1), main = "Bird Data")
abline(v = 51, lty =2)
plot(Prop_correct, type = "b", ylim = c(0,1), ylab = "",xlab = "Trial", main = "Simulated Data")
abline(v = 51, lty =2)