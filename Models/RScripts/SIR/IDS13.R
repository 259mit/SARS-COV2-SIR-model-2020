# SIR function: write and run this first
# this function will be an argument in ode()

SIR_fn <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    N  <- S+I+R
    
    dS <- -beta*S*I/N
    dI <- beta*S*I/N-gamma*I
    dR <- gamma*I
    
    return(list(c(dS, dI, dR)))
    
  })
  
}


SIR_SSQ <- function(parameters, dat) {  # 'parameters': parameters for SIR_fn
  #  supplied as a vector with named 
  #  elements  
  # 'dat' should be a dataframe or 
  # list containing a vector element I
  
  # calculate model output using your SIR function with ode()
  ### you will need to have defined your SIR function
  ### ode() will need to take parameters - arguments beta and gamma 
  ### from the input to SIR_SSQ
  
  result <- as.data.frame(ode(  y = initial_state_values # named vector 
                                , times = times                # vector of times
                                ,  func = SIR_fn               # your SIR function
                                , parms = parameters)   # beta and gamma, which will 
                          # be read from the parameters 
                          # argument entered 
                          # with SIR_SSQ()
  )
  
  # SSQ calculation: needs the dat argument 
  # (the observed data you are fitting to)
  ### You need to filter the model output to those timepoints 
  ### in the observed data
  ### Calculate SSQ by squaring each delta - the difference
  ### between model output and observed data point
  ### and taking the sum of these squared deltas
  
  # assumes the data you are fitting to has a column "I"
  dat <- na.omit(dat) # within the function, 
  #select complete cases only from dat
  
  #select from result$I where the times match the times in the data  
  deltas2 <- (result$I[result$time %in% dat$time] - dat$I)^2   
  
  SSQ   <- sum(deltas2)
  
  return(SSQ)
  
}

flu_dat <- data.frame(time = c(1:14), 
                      I = c(3, 8, 26, 76, 225, 298, 258, 233, 189, 128, 68, 29, 14, 4))
initial_state_values <- c(S = 762, I = 1, R = 0)

# test your function, with a beta and gamma in a vector, parms
parms <- c(beta = 1.15, gamma = 0.02)

# use many timepoints (dense timesteps)
times <- seq(from = 0, to = 14, by = 0.1) 

require(deSolve)
ssq1 <- SIR_SSQ(parameters = parms, 
                dat = flu_dat)
print(paste0("ssq = ", ssq1, ", where beta = ", parms['beta'], ", gamma = ", parms['gamma']))

# or entered directly
ssq2 <- SIR_SSQ(parameters = c(beta = 1.7, gamma = 0.45), 
                dat = flu_dat)
print(paste0("ssq = ", ssq2, ", where beta = 1.7, gamma = 0.45"))


result <- as.data.frame(ode(y = initial_state_values, # vector of initial state values
                            # with named elements
                            times = times,            # vector of times
                            func = SIR_fn,            # your predefined SIR function
                            parms = parameters)       # the parameters which
                        # SIR_fn needs
)

# plot your data and model output if you want, to check it

require(ggplot2)
res_plot <- ggplot() # initialise, but don't assign data

res_plot <- res_plot + geom_point(aes(x = time, y = I)
                                  , colour = "red"
                                  , shape  = "x" 
                                  , data = flu_dat)
res_plot <- res_plot +  geom_line(aes(x = time, y = I)
                                  , colour = "blue"
                                  , data = result)

res_plot


deltas2  <- (result$I[result$time %in% flu_dat$time] - flu_dat$I)^2
SSQ      <- sum(deltas2)
SSQ










