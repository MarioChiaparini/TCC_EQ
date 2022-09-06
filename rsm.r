# Regression Analysis for a 2^2 Design with central points

#load data file

# loading Response Surface Methodology package
library(rsm)
library("readxl")

my_data <- read_excel("/home/ABTLUS/mario.neto/Desktop/basededados.xlsx")

yield_data <- coded.data(
  my_data,                 # Experimental data
  formulas = list(            # List of coding formulas for each factor
    x1 ~ (A - 210)/30, 
    x2 ~ (B - 80)/10,
    x3 ~ (C - 30)/15,
    x4 ~ (D - 30)/10
  ))

head(yield_data)

yield_data.rsm <- rsm(Yield~SO(x1, x2, x3, x4), data = yield_data, subset=(Block="B1"))

par(mfrow = c(2,3))       # 2 x 3 pictures on one plot
contour(
  yield_model,            # Our model
  ~ x1 + x2 + x3 + x4,    # A formula to obtain the 6 possible graphs 
  image = TRUE,           # If image = TRUE, apply color to each contour
)

RSM2 <- as.coded.data(my_data, 
              x1 ~ (Time-85)/5,
              x2 ~ (Temp-175)/5)
str(RSM2)

# regression model with coded variables
model <- rsm(Y ~ FO(x1,x2) + TWI(x1,x2), data = RSM2)
summary(model)

# residuals plot

plot(RSM2$Y, model$residuals) +
  abline(h=0, col = "gray75")
plot(model$fitted.values, model$residuals) +
  abline(h=0, col = "gray75")
plot(RSM2$x1, model$residuals) +
  abline(h=0, col = "gray75")
plot(RSM2$x2, model$residuals) +
  abline(h=0, col = "gray75")

bb_design_1 <- bbd(
  k  = 4,            # Number of factors,
  n0 = 1,            # Number of center points,
  block = FALSE,     # Consider blocks or not in the design 
  randomize = FALSE  # Randomize or not the order experiments
)