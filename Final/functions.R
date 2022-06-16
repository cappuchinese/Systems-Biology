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
model <- function(t, y, parms){
  # Add water every given days, until day 80
  if(t %% as.numeric(parms["time"]) == 0 && t < 80 && t > 0){
    with(as.list(c(parms, y)), {
      dW <- I # I is the water irrigation
      dC <- 0 # There is no growth on those days
      return( list( c(dW, dC) ) )
    })
  }
  # Else the model runs with the equations
  else{
    with(as.list(c(parms, y)),{
      dW <- (-B * q * W) - (r * C * (1 - ( C/N ) ) * W)
      dC <- (r * C * (1 - ( C/N ) ) * W) + ( (g*q*C*W)/(C+1)*(W+1) ) - o * C
      return( list( c(dW, dC) ) )
    })
  }
}

## ---- Day-night model
day.night_model <- function(t, y, parms){
  # Add water every given days, until day 80
  if(t %% as.numeric(parms["time"]) == 0 && t < 80 && t > 0){
    with( as.list( c(parms, y)), {
      dW <- I * 24 # I is the water irrigation
      # NOTE : x24 because the water amount should not change
      dC <- 0 # There is no growth on those days
      return( list( c(dW, dC) ) )
    })
  }
  # Else the model runs with the equations
  else{
    if(t %% 1 <= 0.25 | t %% 1 >= 0.83){
      # During the night (no sun)
      with( as.list (c(parms, y)), {
        dW <- ((-B * q * W) - (r * C * (1 - ( C/N ) ) * W)) # Normal water drop
        dC <- 0 # No growth
        return( list( c(dW, dC) ) )
        })
    }
    else{
      # During the day (sun)
      with( as.list( c(parms, y)),{
      dW <- ((-B * q * W) - (r * C * (1 - ( C/N ) ) * W))
      dC <- ((r * C * (1 - ( C/N ) ) * W) + ( (g*q*C*W)/(C+1)*(W+1) ) - o * C)
      return( list( c(dW, dC) ) )
      })
    }
  }
}

## ---- water-model
water_model <- function(t, y, parms){
  # Add water every given days, until day 80
  with(as.list(c(parms, y)),{
    dW <- (-B * q * W) - (r * C * (1 - ( C/N ) ) * W)
    dC <- (r * C * (1 - ( C/N ) ) * W) + ( (g*q*C*W)/(C+1)*(W+1) ) - o * C
    dI <- 0
    return( list( c(dW, dC, dI) ) )
  })
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
    labs(x = "Time", y = y.val$ylabel) +
    theme(legend.position = "bottom") +
    # Line colours
    scale_colour_manual(values = c("black", colours),
                        limits = c("Reference", names(change.data) ) ) +
    # Legend correction
    guides(color = guide_legend(title = ""))
  return(plt)
}

## Labels and titles for according value
data.values <- data.frame(name = c("W", "C"),
                          ylabel = c("W(t)", "C(t)"))
rownames(data.values) <- data.values$name
