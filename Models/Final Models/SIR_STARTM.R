initial_state_values <- c(S = 350000,   
                          I = 3,           
                          R = 0
)
parameters <- c(beta = 0.395, 
                gamma = 0.256)   
times <- seq(from = 0, to = 120, by = 1)  

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
  ylim(0,300000)

paste((output$R/cp)*100,"%")
cp[69]
output$I

