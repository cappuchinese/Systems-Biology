## Model function
model <- function(t, y, parms){
  if(t %% 9 == 0 && t < 80 && t > 0){
    with(as.list(c(parms, y)), {
      dW <- I
      dC <- 0
      return( list( c(dW, dC) ) )
    })
  }
  else{
    with(as.list(c(parms, y)),{
      dW <- (-B * q * W) - (r * C * (1 - ( C/N ) ) * W)
      dC <- (r * C * (1 - ( C/N ) ) * W) + ( (g*q*C*W)/(C+1)*(W+1) ) - o * C
      return( list( c(dW, dC) ) )
    })
  }
}

## Function to create plots
create.plots <- function(plot.values, ref.data, change.data){
  data.names <- names(change.data)
  # Create colours for the different lines (except the reference data)
  colours <- hue_pal()(length(change.data))
  # y.val inserts the plot.value for the corresponding row of data.values
  y.val <- data.values[plot.values,]
  # The plot
  plt <- ggplot(data = ref.data, mapping = aes(x = time, y = !!sym(y.val$name) ) ) +
    # Lines (Reference data stays black)
    geom_line(aes(color = "Ref")) +
    unlist( mapply(function(single.data, data.name)
                        geom_line(data = single.data, aes(color = data.name) ),
                   change.data, data.names ) ) +
    # Labels
    labs(x = "Time", y = y.val$ylabel, title = y.val$title) +
    theme(legend.position = "bottom") +
    # Line colours
    scale_colour_manual(values = c("black", colours),
                        limits = c("Ref", names(change.data) ) ) +
    # Legend correction
    guides(color = guide_legend(title = ""))
  return(plt)
}

## Function that arranges plots
arrange.plots <- function(plots, title){
  my.grid <- ggarrange(plotlist = plots, ncol = 2, nrow = length(plots)/2,
                       common.legend = TRUE, legend = "bottom")
  print( annotate_figure(my.grid, top = text_grob(title) ) )
}
