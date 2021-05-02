library(deSolve)
library(reshape2)
N <- 1300000000
initial_state_values <- c(S = 0.05*(N)-32100-12919,
                          I = 32100,       
                          R = 12919)
p=5
parameters <- c(beta = p*0.183,
                gamma = 0.1)

times <- seq(from = 0, to = 15, by = 1)

SIR_fn <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {
    
    lambda <- beta * I/N
    
    # The differential equations
    dS <- -lambda*S 
    dI <- lambda*S- gamma*I 
    dR <- gamma*I          
    
    # Output
    return(list(c(dS, dI, dR))) 
  })
}  

output <- ode(y = initial_state_values, 
              times = times, 
              func = SIR_fn,
              parms = parameters)
result <- as.data.frame(output)

output_long <- melt(as.data.frame(output), id = "time")   
library(ggplot2)
result$S
cc<- 0.05*(N)-result$S


ggplot()+
  geom_line(data = output_long, aes(x = time, y = value, colour=variable, group=variable))+
  geom_line(data = cc, aes(x=times,y=cc, colour="Cum"))+
  xlab("Time (days)")+                                              
  ylab("Number ofpeople")+
  labs(colour="Compartment")+
  ylim(0,100000)


