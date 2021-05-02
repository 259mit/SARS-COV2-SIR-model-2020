LL <- sum(dpois(x = reported_data$number_reported, lambda = 0.6 * output$I[output$time %in% reported_data$time], log = TRUE))

initial_state_values <- c(S = 800,  
                          I = 1,       
                          R = 0)

times <- seq(from = 0, to = 10, by = 0.1)

# SIR MODEL FUNCTION

loglik_function <- function(parameters, dat) {   # takes as inputs the parameter values and dataset
  
  beta <- parameters[1]    # extract and save the first value in the "parameters" input argument as beta
  gamma <- parameters[2]   # extract and save the second value in the "parameters" input argument as gamma
  
  # Simulate the model with initial conditions and timesteps defined above, and parameter values from function call
  output <- as.data.frame(ode(y = initial_state_values, 
                              times = times, 
                              func = sir_model,
                              parms = c(beta = beta,       # ode() takes the values for beta and gamma extracted from
                                        gamma = gamma)))   # the "parameters" input argument of the loglik_function()
  
  # Calculate log-likelihood using code block 4 from the previous etivity, accounting for the reporting rate of 60%:
  LL <- sum(dpois(x = dat$number_reported, lambda = 0.6 * output$I[output$time %in% dat$time], log = TRUE))
  
  return(LL) 
}

optim(par = c(1.7, 0.111),           # starting values for beta and gamma - you should get the same result no matter 
      # which values you choose here
      fn = loglik_function,        # the distance function to optimise
      dat = reported_data,         # the dataset to fit to ("dat" argument is passed to the function specified in fn)
      control = list(fnscale=-1))