## ---------------------------
##
## Name: functions.R
##
## Author: Lisa Hu
##
## Purpose: Script contains functions used in the result scripts
##
## Email: l.j.b.hu@st.hanze.nl
##
## ---------------------------

## ---- basic-model
#' Basic model
model <- function(t, y, parms){
  # Add water every 8 days, until day 80
  if(t %% 8 == 0 && t < 80 && t > 0){
    with(as.list(c(parms, y)), {
      dW <- I # I is the water irrigation
      dC <- 0 # There is no growth on those days
      return( list( c(dW, dC) ) )
    })
  }
  # Else the model runs with the differentials
  else{
    with(as.list(c(parms, y)),{
      dW <- (-B * q * W) - (r * C * (1 - ( C/N ) ) * W)
      dC <- (r * C * (1 - ( C/N ) ) * W) + ( (g*q*C*W)/(C+1)*(W+1) ) - o * C
      return( list( c(dW, dC) ) )
    })
  }
}

## ---- delayWater-model
#' Model for delayed water irrigation
delay.model <- function(t, y, parms){
  # Add water every 16 days, until day 80
    if(t %% 16 == 0 && t < 80 && t > 0){
    with(as.list(c(parms, y)), {
      dW <- I # I is the water irrigation
      dC <- 0 # There is no growth on those days
      return( list( c(dW, dC) ) )
    })
  }
  # Else the model runs with the differentials
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
  #' plot.values = The column name of the datas
  #' ref.data = The reference data
  #' change.data = The data that contains changed values
  data.names <- names(change.data)
  # Create colours for the different lines (except the reference data)
  colours <- hue_pal()(length(change.data))
  # y.val inserts the plot.value for the corresponding row of data.values
  y.val <- data.values[plot.values,]
  # The plot
  plt <- ggplot(data = ref.data, mapping = aes(x = time, y = !!sym(y.val$name) ) ) +
    # Lines (Reference data stays black)
    geom_line(aes(color = "Reference")) +
    unlist( mapply(function(single.data, data.name)
                        geom_line(data = single.data, aes(color = data.name) ),
                   change.data, data.names ) ) +
    # Labels
    labs(x = "Time", y = y.val$ylabel, title = y.val$title) +
    theme(legend.position = "bottom") +
    # Line colours
    scale_colour_manual(values = c("black", colours),
                        limits = c("Reference", names(change.data) ) ) +
    # Legend correction
    guides(color = guide_legend(title = ""))
  print(plt)
  return(plt)
}

## Function that arranges plots
arrange.plots <- function(plots, title){
  my.grid <- ggarrange(plotlist = plots, ncol = 2, nrow = length(plots)/2,
                       common.legend = FALSE, legend = "bottom")
  print( annotate_figure(my.grid, top = text_grob(title) ) )
}
