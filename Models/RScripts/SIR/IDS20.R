
# INPUT
# Initial state values for a naive population (everyone is susceptible except for 1 index case),
# where the total population size N is (approximately) 1 million, 20% of this are children and 15% are elderly
initial_state_values <- c(S1 = 200000,   # 20% of the population are children - all susceptible
                          I1 = 1,        # the outbreak starts with 1 infected person (can be of either age)  
                          R1 = 0,
                          S2 = 650000,   # 100%-20%-15% of the population are adults - all susceptible
                          I2 = 0,
                          R2 = 0,
                          S3 = 150000,   # 15% of the population are elderly - all susceptible
                          I3 = 0,
                          R3 = 0)

# Parameters
parameters <- c(b = 0.05,     # the probability of infection per contact is 5%
                c_11 = 7,     # daily number of contacts that children make with each other
                c_12 = 5,     # daily number of contacts that children make with adults
                c_13 = 1,     # daily number of contacts that children make with the elderly
                c_21 = 2,     # daily number of contacts that adults make with children
                c_22 = 9,     # daily number of contacts that adults make with each other
                c_23 = 1,     # daily number of contacts that adults make with the elderly
                c_31 = 1,     # daily number of contacts that elderly people make with children
                c_32 = 3,     # daily number of contacts that elderly people make with adults
                c_33 = 2,     # daily number of contacts that elderly people make with each other
                gamma = 1/5)  # the rate of recovery is 1/5 per day

# Run simulation for 3 months
times <- seq(from = 0, to = 90, by = 0.1)

# MODEL FUNCTION
sir_age_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {
    
    N1 <- S1+I1+R1  # the total number of children in the population
    N2 <- S2+I2+R2  # the total number of adults in the population
    N3 <- S3+I3+R3  # the total number of elderly people in the population  
    
    # Defining the force of infection
    
    # For each age group, need to add the infection rate due to contact with the elderly, 
    # and need to define add an equation for the force of infection experienced by the elderly
    
    # Force of infection acting on susceptible children:
    lambda_1 <- b * c_11 * I1/N1 + b * c_12 * I2/N2 + b * c_13 * I3/N3
    # Force of infection acting on susceptible adults:
    lambda_2 <- b * c_21 * I1/N1 + b * c_22 * I2/N2 + b * c_23 * I3/N3  
    # Force of infection acting on susceptible elderly people:
    lambda_3 <- b * c_31 * I1/N1 + b * c_32 * I2/N2 + b * c_33 * I3/N3  
    
    # The differential equations
    # Rate of change in children:
    dS1 <- -lambda_1 * S1               
    dI1 <- lambda_1 * S1 - gamma * I1
    dR1 <- gamma * I1
    # Rate of change in adults:
    dS2 <- -lambda_2 * S2            
    dI2 <- lambda_2 * S2 - gamma * I2
    dR2 <- gamma * I2   
    # Rate of change in the elderly:
    dS3 <- -lambda_3 * S3            
    dI3 <- lambda_3 * S3 - gamma * I3
    dR3 <- gamma * I3 
    
    # Output
    return(list(c(dS1, dI1, dR1, dS2, dI2, dR2, dS3, dI3, dR3))) 
  })
}


# MODEL OUTPUT

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_age_model,
                            parms = parameters))

# Turn output into long format
output_long <- melt(as.data.frame(output), id = "time") 

# Plot number of people in all compartments over time
ggplot(data = output_long,                                               
       aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Number of people") +                                
  labs(colour = "Compartment") 
