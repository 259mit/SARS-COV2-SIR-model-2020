initial_state_values <- c(S = 1300000000,   
                          I = 32000,           
                          R = 13250,
                          D = 1463)           


parameters <- c(beta = 1.36/14, 
                gamma = 1/14,
                mu = 0.1 )   


times <- seq(from = 0, to = 15, by = 1)  



sird_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), { 
    
    N <- S+I+R+D
    
    lambda <- beta * I/N
    
    dS <- -lambda * S              
    dI <- lambda * S - gamma * I - mu * I  
    dR <- gamma * I        
    dD <- mu * I
    
    return(list(c(dS, dI, dR, dD))) 
  })
  
}


output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sird_model,
                            parms = parameters))
output_long <- melt(as.data.frame(output), id = "time")                 
output_long$proportion <- output_long$value/sum(initial_state_values)

cp<-sum(initial_state_values)-output$S
cc<-data.frame(cp,times)

ggplot()+
  geom_line(data = output_long, aes(x = time, y = value, colour=variable, group=variable))+
  geom_line(data = cc, aes(x=times,y=cp, colour="Cum"))+
  xlab("Time (days)")+                                              
  ylab("Number ofpeople")+
  labs(colour="Compartment")+
  ylim(0,150000)

cp
output$D
message("     Error Percentage");message("May 4 : ",30600/46437,"%");message("May 5 : ",49200/49400,"%");message("May 6 : ",13600/52987,"%");message("May 7 : ",11200/56325,"%")

