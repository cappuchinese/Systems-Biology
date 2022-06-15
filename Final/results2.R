## ---------------------------
##
## Name: results2.R
##
## Author: Lisa Hu
##
## Purpose: Script creates the day/night results for the final report
##
## Email: l.j.b.hu@st.hanze.nl
##
## ---------------------------

## ODE values
time.step <- 1/24
parameters <- c(q = 0.5, r = 0.043, N = 3000, I = 1, B = 0.06, g = 0.001, o = 0.00001)
state <- c(W = 0.6, C = 1)
times <- seq(0, 120, by = time.step)

data.values <- data.frame(name = c("W", "C"),
                          ylabel = c("W(t)", "C(t)"),
                          title = c("Soil water amount", "Growth of fruit tree biomass"))
rownames(data.values) <- data.values$name

## Run the simulations
d.n_data <- as.data.frame(ode(times = times, y = state, parms = parameters,
                              func = day.night_model, method = "euler"))

ggplot(d.n_data, mapping = aes(x = time)) +
       geom_line(mapping = aes(y = W, color = "Water")) +
       geom_line(mapping = aes(y = C, color = "Biomass"), linetype = "dashed") +
       labs(x = "Time", y = "W(t), C(t)") +
       scale_colour_manual(values = c("blue", "red"),
                           limits = c("Water", "Biomass")) +
       guides(color = guide_legend(title = "", override.aes = list(linetype = c(1, 2))))
