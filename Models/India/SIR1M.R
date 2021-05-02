library(deSolve)
library(reshape2)
library(ggplot2)
initial_state_values <- c(S = 1300000000,   
                          I = 53520,           
                          R = 37129)           


parameters <- c(beta = 1.36/14, 
                gamma = 1/14)   


times <- seq(from = 0, to = 15, by = 1)  



sir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), { 
    
    N <- S+I+R
    
    lambda <- beta * I/N
    
    dS <- -lambda * S              
    dI <- lambda * S - gamma * I    
    dR <- gamma * I                
    
    return(list(c(dS, dI, dR))) 
  })
  
}


output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))
output_long <- melt(as.data.frame(output), id = "time")                 
output_long$proportion <- output_long$value/sum(initial_state_values)

pp<-output$I/sum(initial_state_values)
pp
cp<-sum(initial_state_values)-output$S
cp
dcp<-diff(cp)
dcp
cc<-data.frame(cp,times)

ggplot()+
  geom_line(data = output_long, aes(x = time, y = value, colour=variable, group=variable))+
  geom_line(data = cc, aes(x=times,y=cp, colour="Cum"))+
  xlab("Time (days)")+                                              
  ylab("Number ofpeople")+
  labs(colour="Compartment")+
  ylim(0,150000)


paste((output$R/cp)*100,"%")
cp
message("     Error Percentage");message("May 4 : ",30600/46437,"%");message("May 5 : ",49200/49400,"%");message("May 6 : ",13600/52987,"%");message("May 7 : ",11200/56325,"%")



