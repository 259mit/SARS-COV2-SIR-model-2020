initial_state_values <- c(S = 1000000,  
                          I = 1,       
                          R = 0)

parameters <- c(beta = 0.3,
                gamma = 0.4)

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

# PLOT OF THE MODEL FIT

ggplot() +
  geom_line(data = output, aes(x = time, y = I)) +                              
  xlab("Time (days)")+                                              
  ylab("Prevalence of infection") +                                 
  labs(title = paste("Deterministic model output for R0 =",parameters["beta"]/parameters["gamma"])) +
  ylim(c(0,150))

# Defining the model and input
a <- c("beta*S*I/1000000","gamma*I")
nu <- matrix(c(-1,+1,0,0,-1,+1),
             nrow=3,
             ncol=2,
             byrow=FALSE)
tf <- 100

# Simulation
sir_out <- ssa(initial_state_values, a, nu, parameters, tf=tf, simName="SIR")

while(sir_out$stats$nSteps==1){
  sir_out <- ssa(initial_state_values,a,nu, parameters,tf=tf,simName="SIR")
}

# Record number of simulations
n_sims <- 1

# Plot
stoch_plot <- ggplot(as.data.frame(sir_out$data)) +
  geom_line(aes(x = t, y = I)) +
  xlab("Time (days)")+                                              
  ylab("Prevalence of infection") +                                 
  labs(title = paste("Stochastic model output for R0 =",parameters["beta"]/parameters["gamma"]),
       subtitle = paste(n_sims, "simulations")) +
  ylim(c(0,150)) +
  xlim(c(0,100))

stoch_plot

sir_out <- ssa(initial_state_values,a,nu,parameters,tf=tf,simName="SIR")

while(sir_out$stats$nSteps==1){
  sir_out <- ssa(initial_state_values,a,nu,parameters,tf=tf,simName="SIR")
}

n_sims <- n_sims+1

stoch_plot <- stoch_plot + 
  geom_line(data = as.data.frame(sir_out$data), aes(x = t, y = I), col = sample(rainbow(20),1)) +
  labs(title = paste("Stochastic model output for R0 =",parameters["beta"]/parameters["gamma"]),
       subtitle = paste(n_sims, "simulations"))

stoch_plot


initial_state_values <- c(S = 1000000,  
                          I = 1,       
                          R = 0,
                          cum_inc = 0)
stoch_output1 <- simulate_stoch_model(beta = 0.3, gamma = 0.4, n_sims = 10, plot = "prevalence")
stoch_output1 <- simulate_stoch_model(beta = 0.3, gamma = 0.4, n_sims = 10, plot = "cumulative_incidence")
stoch_output1

