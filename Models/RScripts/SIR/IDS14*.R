flu_data <- data.frame(time = 1:14, 
                       number_infected = c(3,8,26,76,225,298,258,233,189,128,68,29,14,4))
initial_state_values <- c(S = 762, I = 1, R = 0)


SIR_fn <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    N  <- S+I+R
    
    dS <- -beta*S*I/N
    dI <- beta*S*I/N-gamma*I
    dR <- gamma*I
    
    return(list(c(dS, dI, dR)))
    
  })
  
}

SIR_SSQ <- function(parameters, dat) {  # parameters must contain beta & gamma
  
  # calculate model output using your SIR function with ode()
  
  result <- as.data.frame(ode(y = initial_state_values  # vector of initial state 
                              # values, with named elements
                              , times = times             # vector of times
                              , func = SIR_fn             # your predefined SIR function
                              , parms = parameters)       # the parameters argument
                          # entered with SIR_SSQ()
  )
  
  # SSQ calculation: needs the dat argument (the observed data you are fitting to)
  # assumes the data you are fitting to has a column "I"
  
  # select only complete cases, i.e. rows with no NAs, from the dataframe
  dat <- na.omit(dat)  
  
  # select elements where results$time is in dat$time
  deltas2 <- (result$I[result$time %in% dat$time]  
              - dat$I)^2                             
  SSQ   <- sum(deltas2)
  
  return(SSQ)
  
}

# choose values to start your optimisation
beta_start  <- 1
gamma_start <- 0.5

# times - dense timesteps for a more detailed solution
times <- seq(from = 0, to = 14, by = 0.1) 

# optim
# you will need to run the cells to assign your functions first

optimised <- optim(par = c(beta = beta_start
                           , gamma = gamma_start)      # these are the starting beta 
                   # and gamma that will be fed 
                   # first, into SSQ_fn
                   , fn = SIR_SSQ
                   , dat = flu_dat  # this argument comes under "..." 
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

