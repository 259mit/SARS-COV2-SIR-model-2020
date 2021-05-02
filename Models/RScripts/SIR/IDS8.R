initial_number_suscepted <- 0
initial_number_infected <- 1000       
initial_number_recovered <- 0 
daily_infection_rate <- 0
initial_number_died <- 0
recovery_rate <- 0.2
mortality_rate<- 0.05
follow_up_duration <- 365

initial_state_values <- c(S = initial_number_suscepted,
                          I = initial_number_infected, 
                          R = initial_number_recovered,
                          M = initial_number_died)

parameters <- c(beta = daily_infection_rate ,gamma = recovery_rate, mu = mortality_rate)  

times <- seq(from = 0, to = follow_up_duration, by = 1) 

initial_state_values
parameters
times  

sir_model <- function(time, state, parameters) { 
  
  with(as.list(c(state, parameters)), {
    N <- S+I+R
    mu <- mortality_rate
    lambda <- beta * I/N
    dS <- -lambda*S - mu*S
    dI <- lambda*S-gamma*I - mu*I
    dR <- gamma*I - mu*R
    dM <- mu*R
    
    return(list(c(dS ,dI, dR, dM)))
  })
  
}

result <- ode(y = initial_state_values         # contains initial N
              , times = times        # the vector of timepoints 
              , func = sir_model    # the exponential equation, written as a function
              , parms = parameters)       # parameters for the exponential equation: here, just alpha
result
output<-result
plot(result)

output_long <- melt(as.data.frame(output), id = "time")     

ggplot(data = output_long,   # specify object containing data to plot
       aes(x = time, 
           y = value, 
           colour = variable, 
           group = variable)) +       # assign columns to axes and groups
  geom_line() +                       # represent data as lines
  xlab("Time (days)")+                # add label for x axis
  ylab("Number of people") +          # add label for y axis
  labs(colour = "Compartment")

output_long$proportion <- output_long$value/sum(initial_state_values)

ggplot(data = output_long,                                               # specify object containing data to plot
       aes(x = time, y = proportion, colour = variable, group = variable)) +  # assign columns to axes and groups
  geom_line() +                                                          # represent data as lines
  xlab("Time (years)")+                                                  # add label for x axis
  ylab("Prevalence (proportion)") +                                      # add label for y axis
  labs(colour = "Compartment",                                           # add legend title  
       title = "Prevalence of susceptible, infected and recovered people over time")



