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

data.values <- data.frame(name = c("W", "C"),
                          ylabel = c("W(t)", "C(t)"),
                          title = c("Soil water amount", "Growth of fruit tree biomass"))
rownames(data.values) <- data.values$name


## Run the simulations
ref.data <- as.data.frame(ode(times = times, y = state,
                              parms = parameters, func = model, method = "euler"))

q.values <- list("q = 0.1" = 0.1,
                 "q = 1" = 1)

for(i in seq_along(q.values)){
  parameters$q <- q.values[[i]]
  q.values[[i]] <- as.data.frame(ode(times = times, y = state,
                                     parms = parameters, func = model, method = "euler"))
}

parameters <- c(q = 0.5, r = 0.043, N = 3000, I = 1,
                B = 0.06, g = 0.001, o = 0.00001, time = 16)

delay.data <- as.data.frame(ode(times = times, y = state, parms = parameters,
                                func = model, method = "euler"))
delay.data <- list("Delayed water irrigation" = delay.data)


## plot the data
plt1 <- ggplot(ref.data, mapping = aes(x = time)) +
          geom_line(mapping = aes(y = W, color = "Water")) +
          geom_line(mapping = aes(y = C, color = "Biomass"), linetype = "dashed") +
          labs(x = "Time", y = "W(t), C(t)") +
          scale_colour_manual(values = c("blue", "red"),
                              limits = c("Water", "Biomass")) +
          guides(color = guide_legend(title = "", override.aes = list(linetype = c(1, 2))))

plt2 <- lapply("C", create.plots, ref.data, q.values)
plt3 <- lapply(c("W", "C"), create.plots, ref.data, delay.data)

plot.list <- append(list(plt1), c(plt2, plt3))

plot.tags <- c("(a)", "(b)", "(c)", "(d)")

for(i in seq_along(plot.list)){
  plot.list[[i]] <- plot.list[[i]] + labs(tag = plot.tags[i])
}

my.grid <- ggarrange(plotlist = plot.list, ncol = 2, nrow = 2,
                     common.legend = FALSE, legend = "bottom")
print( annotate_figure(my.grid, top = text_grob("Plots of the paper") ) )
