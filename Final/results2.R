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
parameters <- c(q = 0.5, r = 0.043, N = 3000, I = 1,
                B = 0.06, g = 0.001, o = 0.00001, time = 8)
state <- c(W = 0.6, C = 1)
times <- seq(0, 120, by = 1/24)

## Run the simulations
d.n_data <- as.data.frame(ode(times = times, y = state, parms = parameters,
                              func = day.night_model, method = "euler"))

## Create the plot
ggplot(d.n_data, mapping = aes(x = time)) +
       # The different lines
       geom_line(mapping = aes(y = W, color = "Water")) +
       geom_line(mapping = aes(y = C, color = "Biomass day/night")) + # day/night
       geom_line(mapping = aes(y = C, color = "Reference biomass"),
                 data = ref.data, linetype = "dashed") + # Default model
       # Labels
       labs(x = "Time", y = "W(t), C(t)") +
       # Line colours
       scale_colour_manual(values = c("blue", "red", "black"),
                           limits = c("Water", "Biomass day/night",
                                      "Reference biomass")) +
       guides(color = guide_legend(title = "",
                                   override.aes = list(linetype = c(1, 1, 2)))) +
       # Theme
       theme(legend.position = "bottom")
