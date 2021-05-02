initial_number_infected <- 1000000       
initial_number_recovered <- 0  
initial_number_died<-0
recovery_rate <- 0.1
mortality_rate <- 0.2
follow_up_duration <- 28

initial_state_values <- c(I = initial_number_infected, 
                          R = initial_number_recovered,
                          M = initial_number_died)
parameters <- c(gamma = recovery_rate, mu = mortality_rate)  

times <- seq(from = 0, to = follow_up_duration, by = 1) 

initial_state_values
parameters
times  

cohort_model <- function(time, state, parameters) { 
  
  with(as.list(c(state, parameters)), {
    
    dI <- -(gamma+mu)*I
    dR <- gamma*I
    dM <- mu*I
    
    return(list(c(dI, dR, dM)))
 })
}

result <- ode(y = initial_state_values         # contains initial N
              , times = times        # the vector of timepoints 
              , func = cohort_model    # the exponential equation, written as a function
              , parms = parameters)       # parameters for the exponential equation: here, just alpha
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
  labs(colour = "Compartment")        # add legend title












