data <- data.frame(time = 1:5,
                   number_infected = c(3,8,26,76,225))
par(mfrow=c(1,1))
plot(data)

initial_state_values <- c(S = 762,  
                          I = 1,       
                          R = 0)

parameters <- c(beta = 1.2,      # the infection rate, which acts on susceptibles
                gamma = 0.04)     # the rate of recovery, which acts on those infected


times <- seq(from = 0, to = 5, by = 0.1)

sir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {   # tell R to unpack variable names from the state and parameters inputs    
    
    # Calculating the total population size N (the sum of the number of people in each compartment)
    N <- S+I+R
    
    # Defining lambda as a function of beta and I:
    lambda <- beta * I/N
    
    # The differential equations
    dS <- -lambda * S               # people move out of (-) the S compartment at a rate lambda (force of infection)
    dI <- lambda * S - gamma * I    # people move into (+) the I compartment from S at a rate lambda, 
    # and move out of (-) the I compartment at a rate gamma (recovery)
    dR <- gamma * I                 # people move into (+) the R compartment from I at a rate gamma
    
    # Return the number of people in the S, I and R compartments at each timestep 
    # (in the same order as the input state variables)
    return(list(c(dS, dI, dR))) 
  })
  
}
output <- ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters)

output_long <- melt(as.data.frame(output), id = "time")   

ggplot(data = output_long,                                               # specify object containing data to plot
       aes(x = time, y = value, colour = variable, group = variable)) +  # assign columns to axes and groups
  geom_line() +                                                          # represent data as lines
  xlab("Time (days)")+                                                   # add label for x axis
  ylab("Proportion of the population") +                                 # add label for y axis
  labs(colour = "Compartment",                                           # add legend title  
       title = "Proportion susceptible, infected and recovered over time") +                                                               
  theme(legend.position = "bottom")   

