library(animate)

sigma <- 10
beta <- 8 / 3
rho <- 28
dt <- 0.015

set.seed(1)
num_points = 10
# init_point = c(20, 3, 60)
init_point = c(1,1,1)
xyz <-  rep(init_point, times = num_points) + rnorm(num_points*3, 0, 0.001)
xyz <- matrix(xyz, num_points*3, 1)

trajectories = xyz

# Initialisation takes ~0.5s
device <- animate$new(width = 600, height = 400)  
attach(device)

for (i in 1:800) {
  
  trajectories <- cbind(trajectories, rep(NA, nrow(trajectories)))
  
  for(j in 1:num_points){
    
    pnt_ref = j*3 - 3
    x <- trajectories[pnt_ref + 1, i]
    y <- trajectories[pnt_ref + 2, i]
    z <- trajectories[pnt_ref + 3, i]
    
    # Euler's method
    dx <- sigma * (y - x) * dt
    dy <- (x * (rho - z) - y) * dt
    dz <- (x * y - beta * z) * dt
    
    #Update trajectories
    trajectories[pnt_ref + 1, i + 1] <- x + dx
    trajectories[pnt_ref + 2, i + 1] <- y + dy
    trajectories[pnt_ref + 3, i + 1] <- z + dz
    
    # Plot observed trajectory (XY plane)
    if(j == 1){
      xs <- trajectories[1, ]
      zs <- trajectories[3, ]
      lines(xs, zs, id = "ObsTraj", xlim = c(-30, 30), ylim = c(-20, 70))
      
      # x <- trajectories[pnt_ref + 1, i + 1]
      # z <- trajectories[pnt_ref + 3, i + 1]
      # plot(x, z, id = "ObsPoint", xlim = c(-30, 30), ylim = c(-20, 70), col = "blue") #<<
    }
    
    # Add ensemble predictions
    # Plot the x-y solution plane
    # if(i%%3 == 0){
      x <- trajectories[pnt_ref + 1, i + 1]
      z <- trajectories[pnt_ref + 3, i + 1]
      point_id = paste("ID", j, sep = "-")
      plot(x, z, id = point_id, xlim = c(-30, 30), ylim = c(-20, 70), col = "blue") #<<
    # }
    
  }
  
  # Sys.sleep(0.025)
  Sys.sleep(0.05)
    
  }
  

clear()           # Clear a plot
off()             # Switch off the device when you are done


library(tidyverse)  
x_ind = (1:num_points)*3 - 2
z_ind = (1:num_points)*3
t_ind = seq(475, 500, by = 5)

lorenz_plot <- ggplot(data = NULL) + 
  geom_path(aes(x = trajectories[1, ], y = trajectories[3,]), col = "grey")  +
    geom_point(aes(x = trajectories[x_ind, t_ind], 
                   y = trajectories[z_ind, t_ind]), col = "red") + 
  theme_bw()
lorenz_plot
# 
# # Define UI ----
# ui <- fluidPage(
#   plotOutput("plot1"),
#   sliderInput("time", "Time", min = 250, max = 500, value = 1)
# )
# 
# # Define server logic ----
# server <- function(input, output) {
#   output$plot1 <- renderPlot({
#     
#     ggplot(data = NULL) + 
#       geom_path(aes(x = trajectories[1, ], y = trajectories[3,]), col = "grey") + 
#       geom_point(aes(x = trajectories[x_ind, input$time], y = trajectories[z_ind, input$time]), col = "red") + 
#       theme_bw()
#    
#   })
#   
# }
# 
# # Run the app ----
# shinyApp(ui = ui, server = server)
# 
# # # Change to x-z solution plane
# # plot(x, z, id = "ID-1", xlim = c(-30, 30), ylim = range(zs), transition = TRUE) #<<
# # lines(xs, zs, id = "lines-1", xlim = c(-30, 30), ylim = range(zs), transition = TRUE) #<<
