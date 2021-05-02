data <-  data.frame(time = 1:14,
                    number_infected = c(3,8,26,76,225,298,258,233,189,128,68,29,14,4))
plot(data, type = "line")


# INPUT
initial_state_values <- c(S = 762,  
                          I = 1,       
                          R = 0)

# Adding the parameters vector
parameters <- c(beta = 1.7,
                gamma = 0.45)

times <- seq(from = 0, to = 14, by = 0.1)

# MODEL FUNCTION
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


# MODEL OUTPUT

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))

# PLOT OF THE MODEL FIT

ggplot() +
  geom_line(data = output, aes(x = time, y = I)) +       # plot the model prediction of
  # the number infected as a line
  geom_point(data = data, aes(x = time, y = number_infected), 
             colour = "red") +  # overlay the data as red dots
  xlab("Time (days)")+                                              
  ylab("Number of infected people") +                                 
  labs(title = paste("Model fit to the epidemic curve with beta =", 
                     parameters["beta"], 
                     "and gamma =", parameters["gamma"]))

