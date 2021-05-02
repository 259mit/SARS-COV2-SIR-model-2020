# SEIR TRIAL NO SOCIAL DISTANCING
library(deSolve)   # package to solve the model
library(reshape2)  # package to change the shape of the model output
library(ggplot2)   # package for plotting



N <- 114200000
initial_state_values <- c(S = N-2,  
                          E = 2,
                          I = 0,       
                          R = 0)
parameters <- c(beta = 1.1,
                gamma = 1/7,
                a = 0.056)

times <- seq(from = 0, to = 71, by = 1)

seir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {
    
    N <- S+E+I+R
    lambda <- beta * I/N
    
    # The differential equations
    dS <-  - (lambda)*S 
    dE <- lambda*S - (a)*E
    dI <- a*E - (gamma)*I
    dR <- gamma*I         
    
    # Output
    return(list(c(dS, dE, dI, dR))) 
  })
}
output <- ode(y = initial_state_values, 
              times = times, 
              func = seir_model,
              parms = parameters)
result <- as.data.frame(output)

output_long <- melt(as.data.frame(output), id = "time")   

#ggplot(data = output_long,                                               
#       aes(x = time, y = value, colour = variable, group = variable)) +  
#  geom_line() +  
#  geom_point(data = data, aes(x = time, y = number_infected), 
#             colour = "red") +
 # xlab("Time (days)")+                                                   
 # ylab("Number of people") +                                
#  labs(colour = "Compartment")   

ggplot()+
geom_line(data = result, aes(x = time, y = I ))+
  geom_point(data = mdata, aes(x=srno, y=diff, colour="red"))+
  xlab("Time (days)")+                                              
  ylab("Number of infected people")


sum(result$I)


