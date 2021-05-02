library(deSolve)   # package to solve the model
library(reshape2)  # package to change the shape of the model output
library(ggplot2)   # package for plotting

initial_number_infected <- 1000000       
initial_number_recovered <- 0  
recovery_rate <- 1/10          
follow_up_duration <- 28

initial_state_values <- c(I = initial_number_infected, 
                          R = initial_number_recovered)
parameters <- c(gamma = recovery_rate)  

times <- seq(from = 0, to = follow_up_duration, by = 1) 

initial_state_values
parameters
times  

cohort_model <- function(time, state, parameters) { 
  
  with(as.list(c(state, parameters)), {
    
    dI <- -gamma*I
    dR <- gamma*I
      
      return(list(c(dI, dR)))
  })
  
}


result <- ode(y = initial_state_values         # contains initial N
              , times = times        # the vector of timepoints 
              , func = cohort_model    # the exponential equation, written as a function
              , parms = parameters)       # parameters for the exponential equation: here, just alpha
result
plot(result)

output<-result

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











  