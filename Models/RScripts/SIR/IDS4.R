N <- 1000000
initial_number_suscepted <- 1000000-1
initial_number_infected <- 1       
initial_number_recovered <- 0 
daily_infection_rate <- 1
recovery_rate <- 0.1          
follow_up_duration <- 60


initial_state_values <- c(S = initial_number_suscepted,
                          I = initial_number_infected, 
                          R = initial_number_recovered)
  
parameters <- c(beta = daily_infection_rate ,gamma = recovery_rate)  

times <- seq(from = 0, to = follow_up_duration, by = 1) 

initial_state_values
parameters
times  

sir_model <- function(time, state, parameters) { 
  
  with(as.list(c(state, parameters)), {
    lambda <- beta * I/N
    dS <- -lambda*S
    dI <- lambda*S-gamma*I
    dR <- gamma*I
    
    return(list(c(dS ,dI, dR)))
  })
  
}

run_sir_model <- 
  function(beta, gamma, S0 = 999999, I0 = 1, R0 = 0, duration = 60) {
    
    initial_state_values <- c(S = S0,  # nearly the whole population we are 
                              # modelling is susceptible to infection
                              I = I0,  # the epidemic starts with a single
                              # infected person
                              R = R0)  # there is no prior immunity in the
    # population
    
    parameters <- c(beta = beta, # the rate of infection, 
                    
                    gamma = gamma)  # the rate of recovery, 
    # which acts on those infected
    
    times <- seq(from = 0, to = duration, by = 1) 
    
    output <- as.data.frame(ode(y = initial_state_values, 
                                times = times, 
                                func = sir_model,
                                parms = parameters))

    # Plotting the output
    plot(x = output$time,             # time on the x axis
         y = output$S,                # the number of susceptible people at
         # each timestep on the y axis
         type = "l",                  # type = "l" tells R we want lines 
         # rather than points
         ylim = c(0,(S0+I0+R0)),      # the limits of the y axis
         # (from 0 to the total number of 
         # people)
         
         xlab = "Time (days)", 
         ylab = "Number of people")    # add axis labels
    
    lines(x = output$time,            # add the number of
          y = output$I,                 # infected people at each
          col = "red")                  # timestep on the y axis
    
    
    lines(x = output$time,            # number of recovered
          y = output$R,                 # people at each
          col = "blue")                 # timestep on the y axis
    
    legend(x = "right",                     # add a legend on the right-hand
           # side of the plot
           legend = c("S", "I", "R"),       # labels S, I and R for black, 
           col = c("black", "red", "blue"),   # red, blue lines respectively
           lty = c(1,1))                    # both lines are
    # solid linetype (lty = 1)
    title(main = paste("beta =", beta,      # main title
                       ", gamma =", gamma))
  }

par(mfrow=c(1,1))
run_sir_model(beta = 1, gamma = 0.1)



