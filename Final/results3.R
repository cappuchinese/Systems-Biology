## ---------------------------
##
## Name: results3.R
##
## Author: Lisa Hu
##
## Purpose: Script creates the results for different days of water irrigation
##
## Email: l.j.b.hu@st.hanze.nl
##
## ---------------------------

## ODE values
parameters <- c(q = 0.5, r = 0.043, N = 3000, I = 1,
                B = 0.06, g = 0.001, o = 0.00001, time = 8) # time = 8 for reference
state <- c(W = 0.6, C = 1)
times <- seq(0, 120, by = 1)

## Run the simulations
ref.data <- as.data.frame(ode(times = times, y = state,
                              parms = parameters, func = model, method = "euler"))

time.values <- list("Every 5 days" = 5,
                    "Every 10 days" = 10,
                    "Every 12 days" = 12)

for(i in seq_along(time.values)){
  parameters$time <- time.values[[i]]  # Set the time value
  # Run the simulation and store in time.values
  time.values[[i]] <- as.data.frame(ode(times = times, y = state,
                              parms = parameters, func = model, method = "euler"))
}

## Create the plots
plts <- lapply(c("W", "C"), create.plots, ref.data, time.values)

## Add the figure annotations
plot.tags <- c("(a)", "(b)")
for(i in seq_along(plts)){
  plts[[i]] <- plts[[i]] + labs(tag = plot.tags[i])
}

## Arrange the plots
my.grid <- ggarrange(plotlist = plts, ncol = 1, nrow = 2,
                     common.legend = FALSE, legend = "bottom")
## Print the plots with a title
print( annotate_figure(my.grid,
                       top = text_grob("Water irrigation on different days") ) )
