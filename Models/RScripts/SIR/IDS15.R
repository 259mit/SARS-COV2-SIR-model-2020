data<-dataset1
plot(data$time, data$number_infected)
initial_state_values <- c(S = 499,        # the total population size is 500
                          I = 1,       
                          R = 0)

times <- seq(from = 0, to = 200, by = 1)  # the outbreak lasts 200 days

# SIR MODEL FUNCTION
sir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {
    
    N <- S+I+R
    
    lambda <- beta * I/N
    
    # The differential equations
    dS <- -lambda * S               
    dI <- lambda * S - gamma * I
    dR <- gamma * I             
    
    # Output
    return(list(c(dS, dI, dR))) 
  })
}

SIR_SSQ <- function(parameters, dat) {  # takes as inputs the parameter values and dataset
  
  beta <- parameters[1]    # extract and save the 1st value in the "parameters" 
  # input argument as beta
  gamma <- parameters[2]   # and 2nd value in the "parameters" input argument as gamma
  
  # Simulate the model with initial conditions and timesteps defined above,
  # and parameter values from function call
  output <- as.data.frame(ode(y = initial_state_values, 
                              times = times, 
                              func = sir_model,
                              parms = c(beta = beta,      # ode() takes the values for
                                        # beta and gamma extracted from
                                        gamma = gamma)))  # the "parameters" input argument
  # of the SIR_SSQ() function
  
  # Calculate the sum of squares by comparing the model output with the matching
  # datapoints: This involves, for each timepoint with available data, 
  # calculating the difference between the number of infections
  # predicted by the model and the observed number of infections, squaring all these 
  # differences,and taking the sum of all squared differences
  SSQ <- sum((output$I[output$time %in% dat$time]-dat$number_infected)^2)
  
  return(SSQ)
  
}

optimum<-optim(par = c(0.1, 0.1),         # chose sensible starting values for beta and gamma
               fn = SIR_SSQ,              # the distance function to optimise
               dat = data)                # the dataset to fit to 
# ("dat" argument is passed to the 
# function specified in fn)

optimum$par

parameters <- c(beta = 0.16,
                gamma = 0.05)

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))

# PLOT OF THE MODEL FIT

ggplot() +
  geom_line(data = output, aes(x = time, y = I)) +                              
  geom_point(data = data, aes(x = time, y = number_infected), col = "red") +  
  xlab("Time (days)")+                                              
  ylab("Prevalence of infection") +                                 
  labs(title = paste("Model fit to the epidemic curve with beta =", parameters["beta"], 
                     "and gamma =", parameters["gamma"]))
