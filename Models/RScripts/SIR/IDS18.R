initial_state_values <- c(S = 1000,  
                          I = 1,       
                          R = 0)

parameters <- c(beta = 0.4,
                gamma = 0.2)

times <- seq(from = 0, to = 100, by = 1)

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

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))

paste("Number susceptible at time 0:", output$S[output$time == 0])
paste("Number susceptible at time 100:", output$S[output$time == 100])
paste("Difference:", output$S[output$time == 0]-output$S[output$time == 100])


result<-as.data.frame(output)
cum_cases<-1000-result$S
plot(cum_cases)








