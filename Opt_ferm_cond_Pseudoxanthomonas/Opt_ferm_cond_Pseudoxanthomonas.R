#Ref: Optimization of fermentation conditions of bacterium Pseudoxanthomonas indica H32 

data1 <- data.frame("Temp" = (c(25, 32.5, 25, 25, 32.5, 32.5, 40, 25, 33, 40, 25, 40, 25, 32.5, 40, 33, 33)),
                   "pH" = (c(7, 8, 8, 6, 6, 7, 6, 9, 8, 7, 9, 8, 9, 9, 9, 8, 8)),
                   "DCW" = c(2.918, 5.232, 5.736, 1.012, 2.878, 5.467, 2.278, 0, 5.105, 4.472, 0, 6.075, 0, 0, 0, 4.169, 5.323),
                   "u" = c(0.064, 0.446, 0.172, 0.060, 0.074, 0.194, 0.115, 0.000, 0.445, 0.320, 0.000, 0.158, 0.000, 0, 0.000, 0.478, 0.470)
  
                   
)

data2 <- data.frame("Qg/V" = (c(0.5,1.5,1,1,0.5,1.5,0.5,1.5,1.5,0.5,1,1,1,1.5,1,1,0.5,1,1.5,0.5,1.5,0.5,1,0.5,1)),
                    "Ni" = (c(7.5, 14.7, 4.17, 7.5, 5.83, 14.17, 10.83, 10.83, 12.5, 14.17, 7.5, 9.17, 10.83, 4.17, 5.83, 5.83, 12.5, 12.5, 9.17, 14.17, 5.83, 9.17, 4.17, 4.17, 7.5)),
                    "Kla" = c(0.0592, 0.1240, 0.0263, 0.0661, 0.0430, 0.0811, 0.0657, 0.0976, 0.0984, 0.0380, 0.0694, 0.1036, 0.0667, 0.0287, 0.0562, 0.0472, 0.0438, 0.0880, 0.0859, 0.0351, 0.0606, 0.0682, 0.0288, 0.0170, 0.0615))


# data exploration
summary(data1)
summary(data2)

boxplot(data1$DCW ~ data1$Temp ,data = data1)
boxplot(data1$DCW ~ data1$pH ,data = data1)

boxplot(data1$u ~ data1$Temp ,data = data1)
boxplot(data1$u ~ data1$pH ,data = data1)

library(plotly)

fig <- plot_ly(data1, x = data1$pH, y = data1$Temp, z = data1$DCW,
               marker = list(color = ~data1$DCW, colorscale = c('#FFE1A1', '#683531')))

fig <- fig %>% layout(scene = list(xaxis = list(title = 'pH'),
                                   yaxis = list(title = 'Temperature (C)'),
                                   zaxis = list(title = 'DCW(g/L)')))

fig

# linear regression 
linear.mod <- lm(data1$DCW ~ data1$pH + data1$Temp)
summary(linear.mod) #non significant parameters
pred.val <- predict(linear.mod)
pred.val

# Sample data
x <- seq(6, 9, length.out = 100)
y <- seq(25, 40, length.out = 100)
z <- outer(x, y, function(x, y) 4.1073 - 0.5920*x + 0.1094*y)

# Create a plot_ly plot
fig <- fig %>% add_surface(x = x, y= y, z =z,
    opacity = 0.7,
    contours = list(
      z = list(
      highlightcolor = "#42f4bc",
      project = list(z = TRUE)
      )
    ),
    colorbar = list()  # Remove color bar
  )

fig

library(rgl)
library(gdata)
library(plotrix)
library(viridisLite)

col=color.scale(z,
                c(0,0.2,0.9,0.95,0.95),
                c(0.7,0.8,0.9,0.7,0.95),
                c(0.1,0,0,0.35,0.95))

# Make the plot with the data points
plot3d(x = data1$pH, y = data1$Temp, z = data1$DCW, type = "s", size = 1, lit = FALSE,
       col = "red", xlab = "pH", ylab = "Temp", zlab = "DCW")

# Add line segments showing the error
segments3d(interleave(data1$pH,   data1$pH),
           interleave(data1$Temp, data1$Temp),
           interleave(data1$DCW,  pred.val),
           col = "black")

# Add the mesh of predicted values
surface3d(x, y, z,
         color  = col, front = "lines", back = "lines")


# Fitting the model with forward selection process

y = data1$DCW
A = data1$Temp
B =  data1$pH
A2 = A**2
B2 = B**2
base.model <- lm(y ~ 1)
model.DCW <- lm(y ~ A + B + A*B + A2 + B2)

model.DCW
anova(model.DCW)
summary(model.DCW)

pred.val <- predict(model.DCW)
max.val <- max(pred.val)
# Predicted values
y <- seq(25, 40, length.out = 100)
x <- seq(6, 9, length.out = 100)
z <- outer(x, y, function(x, y)  -1.189e+02 + 8.579e-01*y + 2.979e+01*x -8.718e-03*y**2   -1.951e+00*x**2   -3.165e-02*x*y)

# Make the plot with the data points
plot3d(x = data1$pH, y = data1$Temp, z = data1$DCW, type = "s", size = 1, lit = FALSE,
       col = "red", xlab = "pH", ylab = "Temp", zlab = "DCW")

# Add line segments showing the error
segments3d(interleave(data1$pH,   data1$pH),
           interleave(data1$Temp, data1$Temp),
           interleave(data1$DCW,  pred.val),
           col = "black")

# Add the mesh of predicted values
surface3d(x, y, z,
          color  = col, front = "lines", back = "lines")


# Optimal condition
fig <- plot_ly()
fig <- fig %>% add_surface(x = x, y= y, z =z,
                           opacity = 0.7,
                           contours = list(
                             z = list(
                               show=TRUE,
                               usecolormap=TRUE,
                               highlightcolor = "#42f4bc",
                               project = list(z = TRUE)
                             )
                           ),
                           colorbar = list()  # Remove color bar
)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'pH'),
                                   yaxis = list(title = 'Temperature (C)'),
                                   zaxis = list(title = 'DCW(g/L)')))

fig <- fig %>% add_markers(x = 8.1818,y= 31.8181, z= 5.869, col = "red")
fig <- fig %>% add_text(x = 8.1818,y= 31.8181, z= 5.869,text = "Maximum value = 5.869")


fig

# Fit u
y = data1$u
model.u <- lm(y ~ A + B + A*B + A2 + B2)

model.u
anova(model.u)
summary(model.u)

pred.val <- predict(model.u)
max.val <- max(pred.val)
# Predicted values
y <- seq(25, 40, length.out = 100)
x <- seq(6, 9, length.out = 100)
z <- outer(x, y, function(x, y)  -9.565810 +  0.188868*y + 1.823005*x -0.002460 *y**2   -0.114996 *x**2  -0.003208*x*y)

# Make the plot with the data points
plot3d(x = data1$pH, y = data1$Temp, z = data1$u, type = "s", size = 1, lit = FALSE,
       col = "red", xlab = "pH", ylab = "Temp", zlab = "u")

# Add line segments showing the error
segments3d(interleave(data1$pH,   data1$pH),
           interleave(data1$Temp, data1$Temp),
           interleave(data1$u,  pred.val),
           col = "black")

# Add the mesh of predicted values
surface3d(x, y, z,
          color  = col, front = "lines", back = "lines")


# Optimal condition
fig <- plot_ly()
fig <- fig %>% add_surface(x = x, y= y, z =z,
                           opacity = 0.7,
                           contours = list(
                             z = list(
                               show=TRUE,
                               usecolormap=TRUE,
                               highlightcolor = "#42f4bc",
                               project = list(z = TRUE)
                             )
                           ),
                           colorbar = list()  # Remove color bar
)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'pH'),
                                   yaxis = list(title = 'Temperature (C)'),
                                   zaxis = list(title = 'u(h-1)')))

fig <- fig %>% add_markers(x = 7.6969,y= 32.27, z= 0.398, col = "red")
fig <- fig %>% add_text(x = 7.6969,y= 32.27, z= 0.398,text = "Maximum value = 0.398")

fig
# Multiple layers plot (Stacked plot)
# Predicted values u
y <- seq(25, 40, length.out = 100)
x <- seq(6, 9, length.out = 100)
z <- outer(x, y, function(x, y)  -9.565810 +  0.188868*y + 1.823005*x -0.002460 *y**2   -0.114996 *x**2  -0.003208*x*y)

z <- z + 4

fig <- plot_ly()
fig <- fig %>% add_surface(x = x, y= y, z =z,
                           opacity = 0.7,
                           contours = list(
                             z = list(
                               show=TRUE,
                               usecolormap=TRUE,
                               highlightcolor = "#42f4bc",
                               project = list(z = TRUE)
                             )
                           ),
                           colorbar = list()  # Remove color bar
)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'pH'),
                                   yaxis = list(title = 'Temperature (C)'),
                                   zaxis = list(title = 'Response DCW&u')))

# Predicted values DCW
y <- seq(25, 40, length.out = 100)
x <- seq(6, 9, length.out = 100)
z <- outer(x, y, function(x, y)  -1.189e+02 + 8.579e-01*y + 2.979e+01*x -8.718e-03*y**2   -1.951e+00*x**2   -3.165e-02*x*y)

fig <- fig %>% add_surface(x = x, y= y, z =z,
                           opacity = 0.7,
                           contours = list(
                             z = list(
                               show=TRUE,
                               usecolormap=TRUE,
                               highlightcolor = "#ADDD8E",
                               project = list(z = TRUE)
                             )
                           ),
                           colorbar = list()  # Remove color bar
)
fig <- fig %>% add_markers(x = 8.1818,y= 31.8181, z= 5.869, col = "black")
fig <- fig %>% add_markers(x = 7.6969,y= 32.27, z= 0.398+4, col = "red")
fig

# Fit kLa
y = data2$Kla
A = data2$Qg.V
B =  data2$Ni
A2 = A**2
B2 = B**2
model.kLa <- lm(y ~ A + B + A*B + A2 + B2)

model.kLa
anova(model.kLa)
summary(model.kLa)

pred.val <- predict(model.kLa)
max.val <- max(pred.val)
max.val
