N <- 114200
initial_state_values <- c(S = N-1,  
                          E = 100,
                          I = 0,       
                          R = 0)
parameters <- c(beta = 1.7,
                gamma = 0.15,
                mu = 0.3,
                a = 0.8 )

times <- seq(from = 0, to = 100, by = 1)

seir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {
    
    N <- S+E+I+R
    b <- mu
    lambda <- beta * I/N
    
    # The differential equations
    dS <- b*N - (lambda+mu)*S 
    dE <- lambda*S - (mu+a)*E
    dI <- a*E - (gamma+mu)*I
    dR <- gamma*I - mu*R         
    
    # Output
    return(list(c(dS,dE, dI, dR))) 
  })
}

SEIR_SSQ <- function(parameters, dat) {  # 'parameters': parameters for SIR_fn
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
                                ,  func = SEIR_fn               # your SIR function
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


output <- ode(y = initial_state_values, 
              times = times, 
              func = seir_model,
              parms = parameters)
resultf <- as.data.frame(output)

output_long <- melt(as.data.frame(output), id = "time")   

optimised <- optim(par = c(beta = beta_start
                           , gamma = gamma_start)      # these are the starting beta 
                   # and gamma that will be fed 
                   # first, into SSQ_fn
                   , fn = SEIR_SSQ
                   , dat = mdata  # this argument comes under "..." 
                   # "Further arguments to be passed to fn and gr"
)

optimised #have a look at the model output

optimised$par

opt_mod <- as.data.frame(ode(y = initial_state_values  # named vector of initial
                             # state values
                             , times = times            # vector of times
                             ,  func = SIR_fn           # your predefined SIR function
                             , parms = optimised$par))
## plot your optimised model output, with the epidemic data using ggplot ##
require(ggplot2)

opt_plot <- ggplot()
opt_plot <- opt_plot + geom_point(aes(x = time, y = I)
                                  , colour = "red"
                                  , shape  = "x" 
                                  , data = flu_dat)
opt_plot <- opt_plot + geom_line(aes(x = time, y = I)
                                 , colour = "blue"
                                 , data   = opt_mod)
opt_plot


