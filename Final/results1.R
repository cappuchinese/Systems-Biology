## ---------------------------
##
## Name: results1.R
##
## Author: Lisa Hu
##
## Purpose: Script creates the first results for the final report
##
## Email: l.j.b.hu@st.hanze.nl
##
## ---------------------------

## ODE values
parameters <- c(q = 0.5, r = 0.043, N = 3000, I = 1,
                B = 0.06, g = 0.001, o = 0.00001, time = 8)
state <- c(W = 0.6, C = 1)
times <- seq(0, 120, by = 1)

## Run the simulations
ref.data <- as.data.frame(ode(times = times, y = state,
                              parms = parameters, func = model, method = "euler"))

## Determine the different q values
q.values <- list("q = 0.1" = 0.1,
                 "q = 1" = 1)

for(i in seq_along(q.values)){
  parameters$q <- q.values[[i]]  # Set new q value
  # Run the simulation and store in q.values
  q.values[[i]] <- as.data.frame(ode(times = times, y = state,
                                     parms = parameters, func = model, method = "euler"))
}

## Simulation for delayed water irrigation (every 16 days)
parameters <- c(q = 0.5, r = 0.043, N = 3000, I = 1,
                B = 0.06, g = 0.001, o = 0.00001, time = 16)

delay.data <- as.data.frame(ode(times = times, y = state, parms = parameters,
                                func = model, method = "euler"))
delay.data <- list("Delayed water irrigation" = delay.data)


## Create the plots
# The model simulation
plt1 <- ggplot(ref.data, mapping = aes(x = time)) +
          # The different lines
          geom_line(mapping = aes(y = W, color = "Water")) +
          geom_line(mapping = aes(y = C, color = "Biomass"), linetype = "dashed") +
          # Labels
          labs(x = "Time", y = "W(t), C(t)") +
          # Line colours
          scale_colour_manual(values = c("blue", "red"),
                              limits = c("Water", "Biomass")) +
          # Make the line of the Biomass a dashed line in the legend
          guides(color = guide_legend(title = "",
                                      override.aes = list(linetype = c(1, 2))))

# Different q values
plt2 <- lapply("C", create.plots, ref.data, q.values)

# Delayed water model
plt3 <- lapply(c("W", "C"), create.plots, ref.data, delay.data)

## Add figure annotation
plot.list <- append(list(plt1), c(plt2, plt3))
plot.tags <- c("(a)", "(b)", "(c)", "(d)")

for(i in seq_along(plot.list)){
  plot.list[[i]] <- plot.list[[i]] + labs(tag = plot.tags[i])
}

## Arrange plots
my.grid <- ggarrange(plotlist = plot.list, ncol = 2, nrow = 2,
                     common.legend = FALSE, legend = "bottom")
## Print plots
print( annotate_figure(my.grid) )
