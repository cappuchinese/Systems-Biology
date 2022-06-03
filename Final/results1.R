## Set the values for the simulation
parameters <- c(
  q = 0.5, # Accumulated energy constant
  r = 0.043, # Fruit trees intrinsic growth rate
  N = 3000, # Fruit carrying capacity
  I = 1, # Water toevoer
  B = 0.06, # Evapotranspiration rate
  g = 0.001, # Photosynthetic contribution rate
  o = 0.00001 # Mortality rate of fruit trees,
)

state <- c(W = 0.6, C = 1)
times <- seq(0, 120, by = 1)

## Run the simulation
results <- as.data.frame(ode(times = times, y = state,
                 parms = parameters, func = model, method = "euler"))

ggplot(results, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = W), color = "blue") +
  geom_line(mapping = aes(y = C), color = "red", linetype = "dashed")+
  geom_vline(xintercept = 10) +
  labs(x = "Time", y = "W(t), C(t)")
