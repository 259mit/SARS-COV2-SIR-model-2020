simulate_stoch_model<-function (beta, gamma, n_sims, plot = "prevalence", final_time = 100, 
          initial_conditions = initial_state_values) 
{
  cum_inc <- rep(0, n_sims)
  parms <- c(beta = beta, gamma = gamma)
  a <- c("beta*S*I/(S+I+R)", "gamma*I")
  nu <- matrix(c(-1, +1, 0, +1, 0, -1, +1, 0), nrow = 4, ncol = 2, 
               byrow = FALSE)
  tf <- final_time
  sir_out <- ssa(initial_conditions, a, nu, parms, tf = tf, 
                 simName = "SIR")
  cum_inc[1] <- sir_out$data[nrow(sir_out$data), "cum_inc"]
  while (sir_out$stats$nSteps == 1) {
    sir_out <- ssa(initial_conditions, a, nu, parms, tf = tf, 
                   simName = "SIR")
  }
  if (plot == "prevalence") {
    myplot <- ggplot(as.data.frame(sir_out$data)) + geom_line(aes(x = t, 
                                                                  y = I), na.rm = TRUE) + xlab("Time (days)") + ylab("Prevalence of infection") + 
      labs(title = paste("Stochastic model output for R0 =", 
                         parms["beta"]/parms["gamma"]), subtitle = paste(n_sims, 
                                                                         "simulations")) + xlim(c(0, final_time))
  }
  else if (plot == "cumulative_incidence") {
    myplot <- ggplot(as.data.frame(sir_out$data)) + geom_line(aes(x = t, 
                                                                  y = cum_inc), na.rm = TRUE) + xlab("Time (days)") + 
      ylab("Cumulative incidence of infection") + labs(title = paste("Stochastic model output for R0 =", 
                                                                     parms["beta"]/parms["gamma"]), subtitle = paste(n_sims, 
                                                                                                                     "simulations")) + xlim(c(0, final_time))
  }
  else {
    print("plot can be either prevalence or cumulative_incidence")
  }
  for (i in 2:n_sims) {
    sir_out <- ssa(initial_conditions, a, nu, parms, tf = tf, 
                   simName = "SIR")
    while (sir_out$stats$nSteps == 1) {
      sir_out <- ssa(initial_conditions, a, nu, parms, 
                     tf = tf, simName = "SIR")
    }
    cum_inc[i] <- sir_out$data[nrow(sir_out$data), "cum_inc"]
    if (plot == "prevalence") {
      myplot <- myplot + geom_line(data = as.data.frame(sir_out$data), 
                                   aes(x = t, y = I), col = sample(rainbow(20), 
                                                                   1), na.rm = TRUE) + labs(title = paste("Stochastic model output for R0 =", 
                                                                                                          parms["beta"]/parms["gamma"]), subtitle = paste(n_sims, 
                                                                                                                                                          "simulations"))
    }
    else if (plot == "cumulative_incidence") {
      myplot <- myplot + geom_line(data = as.data.frame(sir_out$data), 
                                   aes(x = t, y = cum_inc), col = sample(rainbow(20), 
                                                                         1), na.rm = TRUE) + labs(title = paste("Stochastic model output for R0 =", 
                                                                                                                parms["beta"]/parms["gamma"]), subtitle = paste(n_sims, 
                                                                                                                                                                "simulations"))
    }
    else {
      print("plot can be either prevalence or cumulative_incidence")
    }
    i <- i + 1
  }
  print(myplot)
  return(cum_inc)
}
