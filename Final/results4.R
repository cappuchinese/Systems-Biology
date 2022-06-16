## ---------------------------
##
## Name: results3.R
##
## Author: Lisa Hu
##
## Purpose: Script creates the results for different days of water integretion
##
## Email: l.j.b.hu@st.hanze.nl
##
## ---------------------------

## ODE values
parameters <- c(q = 0.5, r = 0.043, N = 3000, B = 0.06, g = 0.001, o = 0.00001, time = 8)
state <- c(W = 0.6, C = 1, I = 0)
times <- seq(0, 120, by = 1)

## Determine what the root is
root <- function(t, y, parms){
  return(y["W"] - 4e-3)
}

## When root found, execute event
eventfun <- function (t, y, parms){
  y["I"] <- 1
  y["W"] <- y["W"] + y["I"]
  return(y)
}

## Run the simulation with events
sim.data <- ode(times = times, y = state, parms = parameters,
                func = water_model, rootfunc = root,
                events = list(func = eventfun, root = TRUE, terminalroot = 2))
roottimes <- attributes(sim.data)$troot  # Timesteps where root was found
sim.data <- as.data.frame(sim.data)

## Create plot
ggplot(sim.data, mapping = aes(x = time)) +
       # The different lines
       geom_line(mapping = aes(y = W, color = "Water")) +
       geom_line(mapping = aes(y = C, color = "Biomass")) +
       # Vertical lines where root was found
       unlist( mapply( function(x){
         geom_vline(xintercept = x, linetype = "dashed")
                                   }, roottimes) ) +
       # Labels
       labs(x = "Time", y = "W(t), C(t)") +
       # Line colours
       scale_colour_manual(values = c("blue", "red"),
                           limits = c("Water", "Biomass")) +
       # Theme
       theme(legend.position = "bottom")
