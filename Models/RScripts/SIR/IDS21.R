# INPUT
# Initial state values for a naive population (everyone is susceptible except for 1 index case),
# where the total population size N is (approximately) 1 million, 20% of this are children and 15% are elderly
initial_state_values <- c(S1 = 200000,   # 20% of the population are children - all susceptible
                          S2 = 650000,   # 100%-20%-15% of the population are adults - all susceptible
                          S3 = 150000,   # 15% of the population are elderly - all susceptible
                          I1 = 1,        # the outbreak starts with 1 infected person (can be of either age) 
                          I2 = 0,
                          I3 = 0,
                          R1 = 0,
                          R2 = 0,   
                          R3 = 0)

# Set up an empty contact matrix with rows for each age group and columns for each age group
contact_matrix <- matrix(0,nrow=3,ncol=3)
# Fill in the contract matrix
contact_matrix[1,1] = 7     # daily number of contacts that children make with each other
contact_matrix[1,2] = 5     # daily number of contacts that children make with adults
contact_matrix[1,3] = 1     # daily number of contacts that children make with the elderly
contact_matrix[2,1] = 2     # daily number of contacts that adults make with children
contact_matrix[2,2] = 9     # daily number of contacts that adults make with each other
contact_matrix[2,3] = 1     # daily number of contacts that adults make with the elderly
contact_matrix[3,1] = 1     # daily number of contacts that elderly people make with children
contact_matrix[3,2] = 3     # daily number of contacts that elderly people make with adults
contact_matrix[3,3] = 2     # daily number of contacts that elderly people make with each other
# The contact_matrix now looks exactly like the one in the etivity instructions. We add this matrix as a parameter below.

# Parameters
parameters <- c(b = 0.05,     # the probability of infection per contact is 5%
                contact_matrix = contact_matrix,   # the age-specific average number of daily contacts (defined above)
                gamma = 1/5)  # the rate of recovery is 1/5 per day

# Run simulation for 3 months
times <- seq(from = 0, to = 90, by = 0.1)

# MODEL FUNCTION
sir_age_model <- function(time, state, parameters) {  
  
  with(as.list(parameters), {
    
    n_agegroups <- 3                                 # number of age groups
    S <- state[1:n_agegroups]                        # assign to S the first 3 numbers in the initial_state_values vector
    I <- state[(n_agegroups+1):(2*n_agegroups)]      # assign to I numbers 4 to 6 in the initial_state_values vector
    R <- state[(2*n_agegroups+1):(3*n_agegroups)]    # assign to R numbers 7 to 9 in the initial_state_values vector
    
    N <- S+I+R     # people in S, I and R are added separately by age group, so N is also a vector of length 3
    
    # Defining the force of infection
    
    # Force of infection acting on susceptible children
    lambda <- b * contact_matrix %*% as.matrix(I/N) 
    # %*% is used to multiply matrices in R
    # the lambda vector contains the forces of infection for children, adults and the elderly (length 3)
    
    # The differential equations
    # Rate of change in children:
    dS <- -lambda * S             
    dI <- lambda * S - gamma * I
    dR <- gamma * I
    
    # Output
    return(list(c(dS, dI, dR))) 
  })
}


# MODEL OUTPUT

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_age_model,
                            parms = parameters))
# the output column names are adopted from the names we assigned in the initial_state_values vector

# Turn output into long format
output_long <- melt(as.data.frame(output), id = "time") 

# Plot number of people in all compartments over time
ggplot(data = output_long,                                               
       aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Number of people") +                                
  labs(colour = "Compartment") 