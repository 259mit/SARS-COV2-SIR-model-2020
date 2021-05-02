library(shiny)
library(shinySIR)
run_shiny(model = "SIR", 
          neweqns = sir_model,
          ics = c(S = 1000, I = 1, R = 0),
          parm0 = c(beta = 1.36/14, gamma = 1/14),
          parm_names = c("Transmission rate", "Recovery rate"),
          parm_min = c(beta = 1/14, gamma = 1/16),
          parm_max = c(beta = 4/14, gamma = 1/9)
)
