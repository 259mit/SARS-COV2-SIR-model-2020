# MODEL INPUTS:

# Initial number of people in each compartment
initial_state_values <- c(S = 300000,         
                          I = 1,           
                          R = 0,          
                          T = 0)   # treatment compartment: no one is on 
# treatment at the beginning of the simulation


# Parameters describing the transition rates in units of days^-1
parameters <- c(beta = 0.6,       # the infection rate
                gamma = 0.2,      # the natural (untreated) rate of recovery
                h = 0.25,         # the rate of treatment initiation
                gamma_t = 0.8)    # the rate of recovery after treatment

# TIMESTEPS:

# Sequence of timesteps to solve the model at
times <- seq(from = 0, to = 80, by = 1) # from 0 to 80 days in daily intervals

# MODEL FUNCTION: 

treatment_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {    
    
    # Calculating the total population size N 
    N <- S+I+R+T                # need to add the treated compartment here
    
    # Defining lambda
    lambda <- beta * (I+T)/N    # force of infection depends on the proportion 
    # in the I and T compartment  
    
    # The differential equations
    dS <- -lambda * S            
    dI <- lambda * S - gamma * I - h * I  # infected people initiate 
    # treatment at a rate h  
    dT <- h * I - gamma_t * T             # people enter the treated 
    # compartment at rate h and 
    # recover at rate gamma_t
    dR <- gamma * I + gamma_t * T         # movement into the recovered compartment 
    # is from infected and treated compartment
    
    # Return the number of people in each compartment at each timestep 
    # (in the same order as the input state variables)
    return(list(c(dS, dI, dR, dT)))         # need to add the rate of change in 
    # the treated compartment T here
  })
  
}

# MODEL OUTPUT:

# Solving the differential equations using the ode integration algorithm
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = treatment_model,
                            parms = parameters))

# Plot the number infected
ggplot(data = output,                                               
       aes(x = time, y = I+T)) +     # infected people include those 
  # untreated (I) and treated (T)
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Prevalence of infection") +                                      
  labs(title = paste("Epidemic with treatment initiation rate of", 
                     parameters["h"], "per day"))
