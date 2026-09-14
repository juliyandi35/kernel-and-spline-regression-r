# SPLINE REGRESSION
# Load required libraries
library(mgcv)
library(splines)
library(readxl)
library(locfit)

# Generate sample data
set.seed(123)
data <- read_excel('DATA SKRIPSI FIRSAN.xlsx')
data <- data[,-1]

# X1 = IPM
x1 <- data$IPM
y <- data$`Persentase Kemiskinan`

# Define the range of possible knots
knot_range <- seq(0.5, 9.5, by = 1)
knot_range
# Empty vector to store GCV scores
gcv_scores_spline1 <- vector()

# Iterate over possible knot positions
for (knot in knot_range) {
  # Build the spline regression model with current knot
  spline_reg1 <- gam(y ~ bs(x1, knots = knot))
  # Calculate GCV score
  gcv_score_spline1 <- gam.check(spline_reg1)$gcv.ubre
  # Append GCV score to vector
  gcv_scores_spline1 <- c(gcv_scores_spline1, gcv_score_spline1)
}

gcv_scores_spline1
min(gcv_scores_spline1)
which.min(gcv_scores_spline1)

# Determine the optimal knot position
optimal_knot1 <- knot_range[which.min(gcv_scores_spline1)]
optimal_knot1
# Build the spline regression model using the optimal knot position
spline_reg_model1 <- gam(y ~ bs(x1, knots = optimal_knot1))
spline_reg_summary1 <- summary(spline_reg_model1)
spline_reg_model1
spline_reg_summary1
# Get the predicted values from the model
predicted_spline1 <- predict(spline_reg_model1)

# Create a scatter plot of the data
par(mfrow=c(1,1))
plot(x1, y, pch = 16, col = "blue", xlab = "IPM", ylab = "Persentase Kemiskinan", main = "Spline Regression")

# Add the spline regression line to the plot
lines(x1, predicted_spline1, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1, 2), bty = "n")

# Calculate the residuals
residuals_spline1 <- y - predicted_spline1

# Calculate MSE
mse_spline1 <- mean(residuals_spline1^2)

# Calculate RMSE
rmse_spline1 <- sqrt(mse_spline1)

# Calculate MAPE
mape_spline1 <- mean(abs(residuals_spline1/y)) * 100

# Print the results
print(paste("Spline Model IPM MSE:", mse_spline1))
print(paste("Spline Model IPM RMSE:", rmse_spline1))
print(paste("Spline Model IPM MAPE:", mape_spline1))
print(paste("Spline Model IPM Adjusted R Squared:",spline_reg_summary1$r.sq))

# Load required library
library(boot)

# Define the GCV function
gcv <- function(y, yhat) {
  mean((y - yhat) ^ 2) / (1 - mean(yhat)^2)
}

# Initialize variables
window_widths <- c(1.2, 1.2, 1.1, 1, 1)  # Range of window widths to consider
gcv_values_kernel1 <- numeric(length(window_widths))

# Calculate GCV for different window widths
for (i in 1:length(window_widths)) {
  kernel_density1 <- density(y, bw = window_widths[i], kernel = "gaussian")
  kernel_reg1 <- glm(kernel_density1$x ~ kernel_density1$y, family = gaussian())
  gcv_values_kernel1[i] <- cv.glm(data = data.frame(x1 = kernel_density1$x, y = kernel_density1$y), glmfit = kernel_reg1, cost = gcv)$delta[1]
}

gcv_values_kernel1
min(gcv_values_kernel1)
which.min(gcv_values_kernel1)

# Determine the optimal window width based on the minimum GCV value
optimal_window_width1 <- window_widths[which.min(gcv_values_kernel1)]
optimal_window_width1
# Build the kernel regression model with the optimal window width
kernel_density1 <- density(y, bw = optimal_window_width1, kernel = "gaussian")

# Load required libraries
library(np)

# Perform kernel regression
kernel_reg1 <- npreg(kernel_density1$y ~ kernel_density1$x, regtype = "ll", bwmethod = "cv.ls")
kernel_reg1

# Plot kernel density and fitted values
par(mfrow=c(1,1))
plot(kernel_density1, main = "Kernel Regression - X1 (IPM)")
lines(kernel_density1$x, fitted(kernel_reg1), col = "red", lwd = 2)
legend("topright", legend = c("Kernel Density", "Fitted Values"), col = c("black", "red"), lwd = c(1, 2), bty = "n")

# Calculate the predicted values
y_pred_kernel1 <- fitted(kernel_reg1)

# Calculate the residuals
residuals_kernel1 <- kernel_density1$y - y_pred_kernel1

# Calculate the total sum of squares
sst1 <- sum((kernel_density1$y - mean(kernel_density1$y))^2)

# Calculate the residual sum of squares
ssr1 <- sum(residuals_kernel1^2)

# Calculate the R-squared
kernel1_rsquared <- 1 - (ssr1 / sst1)

# Calculate the number of predictors
num_predictors1 <- length(coefficients(kernel_reg1)) - 1

# Calculate the number of observations
num_observations <- length(y)

# Calculate the degrees of freedom
df1 <- num_observations - num_predictors1 - 1

# Calculate the adjusted R-squared
kernel1_adj_rsquared <- 1 - (ssr1 / sst1) * ((num_observations - 1) / df1)

# Print the R-squared and adjusted R-squared
print(paste("Kernel Model IPM R-squared:", kernel1_rsquared))
print(paste("Kernel Model IPM Adjusted R-squared:", kernel1_adj_rsquared))

# Calculate MSE
mse_kernel1 <- mean((kernel_density1$y - y_pred_kernel1)^2)

# Calculate RMSE
rmse_kernel1 <- sqrt(mse_kernel1)

# Calculate MAPE
mape_kernel1 <- mean(abs((kernel_density1$y - y_pred_kernel1) / kernel_density1$y)) * 100

# Print the results
print(paste("Kernel Model IPM MSE:", mse_kernel1))
print(paste("Kernel Model IPM RMSE:", rmse_kernel1))
print(paste("Kernel Model IPM MAPE:", mape_kernel1))

# Comparing the model
print(paste("Spline Model IPM Adjusted R Squared:",spline_reg_summary1$r.sq))
print(paste("Kernel Model IPM Adjusted R-squared:", kernel1_adj_rsquared))

# Compare the plot
par(mfrow=c(1,2))
plot(kernel_density1, main = "Kernel Regression - X1 (IPM)")
lines(kernel_density1$x, fitted(kernel_reg1), col = "green", lwd = 2)
legend("topright", legend = c("Data","Kernel Fitted Values"), col = c("black","green"), lwd = c(1,1),cex = 0.65, bty = "n")
plot(x1, y, col = "blue", xlab = "IPM", ylab = "Persentase Kemiskinan", main = "Spline Regression - X1 (IPM)")
lines(x1, predicted_spline1, col = "red", lwd = 2)
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1,1),cex = 0.65, bty = "n")


# X2 = ADHB
x2 <- data$ADHB
y <- data$`Persentase Kemiskinan`

# Define the range of possible knots
knot_range <- seq(0.5, 9.5, by = 1)
knot_range
# Empty vector to store GCV scores
gcv_scores_spline2 <- vector()

# Iterate over possible knot positions
for (knot in knot_range) {
  # Build the spline regression model with current knot
  spline_reg2 <- gam(y ~ bs(x2, knots = knot))
  # Calculate GCV score
  gcv_score_spline2 <- gam.check(spline_reg2)$gcv.ubre
  # Append GCV score to vector
  gcv_scores_spline2 <- c(gcv_scores_spline2, gcv_score_spline2)
}

gcv_scores_spline2
min(gcv_scores_spline2)
which.min(gcv_scores_spline2)

# Determine the optimal knot position
optimal_knot2 <- knot_range[which.min(gcv_scores_spline2)]
optimal_knot2
# Build the spline regression model using the optimal knot position
spline_reg_model2 <- gam(y ~ bs(x2, knots = optimal_knot2))
spline_reg_summary2 <- summary(spline_reg_model2)
spline_reg_model2
spline_reg_summary2
# Get the predicted values from the model
predicted_spline2 <- predict(spline_reg_model2)

# Create a scatter plot of the data
par(mfrow=c(1,1))
plot(x2, y, pch = 16, col = "blue", xlab = "ADHB", ylab = "Persentase Kemiskinan", main = "Spline Regression")

# Add the spline regression line to the plot
lines(x2, predicted_spline2, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1, 2), bty = "n")

# Calculate the residuals
residuals_spline2 <- y - predicted_spline2

# Calculate MSE
mse_spline2 <- mean(residuals_spline2^2)

# Calculate RMSE
rmse_spline2 <- sqrt(mse_spline2)

# Calculate MAPE
mape_spline2 <- mean(abs(residuals_spline2/y)) * 100

# Print the results
print(paste("Spline Model ADHB MSE:", mse_spline2))
print(paste("Spline Model ADHB RMSE:", rmse_spline2))
print(paste("Spline Model ADHB MAPE:", mape_spline2))
print(paste("Spline Model ADHB Adjusted R Squared:",spline_reg_summary2$r.sq))

# Load required library
library(boot)

# Define the GCV function
gcv <- function(y, yhat) {
  mean((y - yhat) ^ 2) / (1 - mean(yhat)^2)
}

# Initialize variables
window_widths <- c(1.2, 1.2, 1.1, 1, 1)  # Range of window widths to consider
gcv_values_kernel2 <- numeric(length(window_widths))

# Calculate GCV for different window widths
for (i in 1:length(window_widths)) {
  kernel_density2 <- density(y, bw = window_widths[i], kernel = "gaussian")
  kernel_reg2 <- glm(kernel_density2$x ~ kernel_density2$y, family = gaussian())
  gcv_values_kernel2[i] <- cv.glm(data = data.frame(x2 = kernel_density2$x, y = kernel_density2$y), glmfit = kernel_reg2, cost = gcv)$delta[1]
}

gcv_values_kernel2
min(gcv_values_kernel2)
which.min(gcv_values_kernel2)

# Determine the optimal window width based on the minimum GCV value
optimal_window_width2 <- window_widths[which.min(gcv_values_kernel2)]
optimal_window_width2
# Build the kernel regression model with the optimal window width
kernel_density2 <- density(y, bw = optimal_window_width2, kernel = "gaussian")

# Load required libraries
library(np)

# Perform kernel regression
kernel_reg2 <- npreg(kernel_density2$y ~ kernel_density2$x, regtype = "ll", bwmethod = "cv.ls")
kernel_reg2

# Plot kernel density and fitted values
par(mfrow=c(1,1))
plot(kernel_density2, main = "Kernel Regression - X2 (ADHB)")
lines(kernel_density2$x, fitted(kernel_reg2), col = "red", lwd = 2)
legend("topright", legend = c("Kernel Density", "Fitted Values"), col = c("black", "red"), lwd = c(1, 2), bty = "n")

# Calculate the predicted values
y_pred_kernel2 <- fitted(kernel_reg2)

# Calculate the residuals
residuals_kernel2 <- kernel_density2$y - y_pred_kernel2

# Calculate the total sum of squares
sst2 <- sum((kernel_density2$y - mean(kernel_density2$y))^2)

# Calculate the residual sum of squares
ssr2 <- sum(residuals_kernel2^2)

# Calculate the R-squared
kernel2_rsquared <- 1 - (ssr2 / sst2)

# Calculate the number of predictors
num_predictors2 <- length(coefficients(kernel_reg2)) - 1

# Calculate the number of observations
num_observations <- length(y)

# Calculate the degrees of freedom
df2 <- num_observations - num_predictors2 - 1

# Calculate the adjusted R-squared
kernel2_adj_rsquared <- 1 - (ssr2 / sst2) * ((num_observations - 1) / df2)

# Print the R-squared and adjusted R-squared
print(paste("Kernel Model ADHB R-squared:", kernel2_rsquared))
print(paste("Kernel Model ADHB Adjusted R-squared:", kernel2_adj_rsquared))

# Calculate MSE
mse_kernel2 <- mean((kernel_density2$y - y_pred_kernel2)^2)

# Calculate RMSE
rmse_kernel2 <- sqrt(mse_kernel2)

# Calculate MAPE
mape_kernel2 <- mean(abs((kernel_density2$y - y_pred_kernel2) / kernel_density2$y)) * 100

# Print the results
print(paste("Kernel Model ADHB MSE:", mse_kernel2))
print(paste("Kernel Model ADHB RMSE:", rmse_kernel2))
print(paste("Kernel Model ADHB MAPE:", mape_kernel2))

# Comparing the model
print(paste("Spline Model ADHB Adjusted R Squared:",spline_reg_summary2$r.sq))
print(paste("Kernel Model ADHB Adjusted R-squared:", kernel2_adj_rsquared))

# Compare the plot
par(mfrow=c(1,2))
plot(kernel_density2, main = "Kernel Regression - X2 (ADHB)")
lines(kernel_density2$x, fitted(kernel_reg2), col = "green", lwd = 2)
legend("topright", legend = c("Data","Kernel Fitted Values"), col = c("black","green"), lwd = c(1,1),cex = 0.65, bty = "n")
plot(x2, y, col = "blue", xlab = "ADHB", ylab = "Persentase Kemiskinan", main = "Spline Regression - X2 (ADHB)")
lines(x2, predicted_spline2, col = "red", lwd = 2)
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1,1),cex = 0.65, bty = "n")

# X3 = ADHK
x3 <- data$ADHK
y <- data$`Persentase Kemiskinan`

# Define the range of possible knots
knot_range <- seq(0.5, 9.5, by = 1)
knot_range
# Empty vector to store GCV scores
gcv_scores_spline3 <- vector()

# Iterate over possible knot positions
for (knot in knot_range) {
  # Build the spline regression model with current knot
  spline_reg3 <- gam(y ~ bs(x3, knots = knot))
  # Calculate GCV score
  gcv_score_spline3 <- gam.check(spline_reg3)$gcv.ubre
  # Append GCV score to vector
  gcv_scores_spline3 <- c(gcv_scores_spline3, gcv_score_spline3)
}

gcv_scores_spline3
min(gcv_scores_spline3)
which.min(gcv_scores_spline3)

# Determine the optimal knot position
optimal_knot3 <- knot_range[which.min(gcv_scores_spline3)]
optimal_knot3
# Build the spline regression model using the optimal knot position
spline_reg_model3 <- gam(y ~ bs(x3, knots = optimal_knot3))
spline_reg_summary3 <- summary(spline_reg_model3)
spline_reg_model3
spline_reg_summary3
# Get the predicted values from the model
predicted_spline3 <- predict(spline_reg_model3)

# Create a scatter plot of the data
par(mfrow=c(1,1))
plot(x3, y, pch = 16, col = "blue", xlab = "ADHK", ylab = "Persentase Kemiskinan", main = "Spline Regression")

# Add the spline regression line to the plot
lines(x3, predicted_spline3, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1, 2), bty = "n")

# Calculate the residuals
residuals_spline3 <- y - predicted_spline3

# Calculate MSE
mse_spline3 <- mean(residuals_spline3^2)

# Calculate RMSE
rmse_spline3 <- sqrt(mse_spline3)

# Calculate MAPE
mape_spline3 <- mean(abs(residuals_spline3/y)) * 100

# Print the results
print(paste("Spline Model ADHK MSE:", mse_spline3))
print(paste("Spline Model ADHK RMSE:", rmse_spline3))
print(paste("Spline Model ADHK MAPE:", mape_spline3))
print(paste("Spline Model ADHK Adjusted R Squared:",spline_reg_summary3$r.sq))

# Load required library
library(boot)

# Define the GCV function
gcv <- function(y, yhat) {
  mean((y - yhat) ^ 2) / (1 - mean(yhat)^2)
}

# Initialize variables
window_widths <- c(1.2, 1.2, 1.1, 1, 1)  # Range of window widths to consider
gcv_values_kernel3 <- numeric(length(window_widths))

# Calculate GCV for different window widths
for (i in 1:length(window_widths)) {
  kernel_density3 <- density(y, bw = window_widths[i], kernel = "gaussian")
  kernel_reg3 <- glm(kernel_density3$x ~ kernel_density3$y, family = gaussian())
  gcv_values_kernel3[i] <- cv.glm(data = data.frame(x3 = kernel_density3$x, y = kernel_density3$y), glmfit = kernel_reg3, cost = gcv)$delta[1]
}

gcv_values_kernel3
min(gcv_values_kernel3)
which.min(gcv_values_kernel3)

# Determine the optimal window width based on the minimum GCV value
optimal_window_width3 <- window_widths[which.min(gcv_values_kernel3)]
optimal_window_width3
# Build the kernel regression model with the optimal window width
kernel_density3 <- density(y, bw = optimal_window_width3, kernel = "gaussian")

# Load required libraries
library(np)

# Perform kernel regression
kernel_reg3 <- npreg(kernel_density3$y ~ kernel_density3$x, regtype = "ll", bwmethod = "cv.ls")
kernel_reg3

# Plot kernel density and fitted values
par(mfrow=c(1,1))
plot(kernel_density3, main = "Kernel Regression - X3 (ADHK)")
lines(kernel_density3$x, fitted(kernel_reg3), col = "red", lwd = 2)
legend("topright", legend = c("Kernel Density", "Fitted Values"), col = c("black", "red"), lwd = c(1, 2), bty = "n")

# Calculate the predicted values
y_pred_kernel3 <- fitted(kernel_reg3)

# Calculate the residuals
residuals_kernel3 <- kernel_density3$y - y_pred_kernel3

# Calculate the total sum of squares
sst3 <- sum((kernel_density3$y - mean(kernel_density3$y))^2)

# Calculate the residual sum of squares
ssr3 <- sum(residuals_kernel3^2)

# Calculate the R-squared
kernel3_rsquared <- 1 - (ssr3 / sst3)

# Calculate the number of predictors
num_predictors3 <- length(coefficients(kernel_reg3)) - 1

# Calculate the number of observations
num_observations <- length(y)

# Calculate the degrees of freedom
df3 <- num_observations - num_predictors3 - 1

# Calculate the adjusted R-squared
kernel3_adj_rsquared <- 1 - (ssr3 / sst3) * ((num_observations - 1) / df3)

# Print the R-squared and adjusted R-squared
print(paste("Kernel Model ADHK R-squared:", kernel3_rsquared))
print(paste("Kernel Model ADHK Adjusted R-squared:", kernel3_adj_rsquared))

# Calculate MSE
mse_kernel3 <- mean((kernel_density3$y - y_pred_kernel3)^2)

# Calculate RMSE
rmse_kernel3 <- sqrt(mse_kernel3)

# Calculate MAPE
mape_kernel3 <- mean(abs((kernel_density3$y - y_pred_kernel3) / kernel_density3$y)) * 100

# Print the results
print(paste("Kernel Model ADHK MSE:", mse_kernel3))
print(paste("Kernel Model ADHK RMSE:", rmse_kernel3))
print(paste("Kernel Model ADHK MAPE:", mape_kernel3))

# Comparing the model
print(paste("Spline Model ADHK Adjusted R Squared:",spline_reg_summary3$r.sq))
print(paste("Kernel Model ADHK Adjusted R-squared:", kernel3_adj_rsquared))

# Compare the plot
par(mfrow=c(1,2))
plot(kernel_density3, main = "Kernel Regression - X3 (ADHK)")
lines(kernel_density3$x, fitted(kernel_reg3), col = "green", lwd = 2)
legend("topright", legend = c("Data","Kernel Fitted Values"), col = c("black","green"), lwd = c(1,1),cex = 0.65, bty = "n")
plot(x3, y, col = "blue", xlab = "ADHK", ylab = "Persentase Kemiskinan", main = "Spline Regression - X3 (ADHK)")
lines(x3, predicted_spline3, col = "red", lwd = 2)
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1,1),cex = 0.65, bty = "n")

# X4 = TPT
x4 <- data$TPT
y <- data$`Persentase Kemiskinan`

# Define the range of possible knots
knot_range <- seq(0.5, 9.5, by = 1)
knot_range
# Empty vector to store GCV scores
gcv_scores_spline4 <- vector()

# Iterate over possible knot positions
for (knot in knot_range) {
  # Build the spline regression model with current knot
  spline_reg4 <- gam(y ~ bs(x4, knots = knot))
  # Calculate GCV score
  gcv_score_spline4 <- gam.check(spline_reg4)$gcv.ubre
  # Append GCV score to vector
  gcv_scores_spline4 <- c(gcv_scores_spline4, gcv_score_spline4)
}

gcv_scores_spline4
min(gcv_scores_spline4)
which.min(gcv_scores_spline4)

# Determine the optimal knot position
optimal_knot4 <- knot_range[which.min(gcv_scores_spline4)]
optimal_knot4
# Build the spline regression model using the optimal knot position
spline_reg_model4 <- gam(y ~ bs(x4, knots = optimal_knot4))
spline_reg_summary4 <- summary(spline_reg_model4)
spline_reg_model4
spline_reg_summary4
# Get the predicted values from the model
predicted_spline4 <- predict(spline_reg_model4)

# Create a scatter plot of the data
par(mfrow=c(1,1))
plot(x4, y, pch = 16, col = "blue", xlab = "TPT", ylab = "Persentase Kemiskinan", main = "Spline Regression")

# Add the spline regression line to the plot
lines(x4, predicted_spline4, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1, 2), bty = "n")

# Calculate the residuals
residuals_spline4 <- y - predicted_spline4

# Calculate MSE
mse_spline4 <- mean(residuals_spline4^2)

# Calculate RMSE
rmse_spline4 <- sqrt(mse_spline4)

# Calculate MAPE
mape_spline4 <- mean(abs(residuals_spline4/y)) * 100

# Print the results
print(paste("Spline Model TPT MSE:", mse_spline4))
print(paste("Spline Model TPT RMSE:", rmse_spline4))
print(paste("Spline Model TPT MAPE:", mape_spline4))
print(paste("Spline Model TPT Adjusted R Squared:",spline_reg_summary4$r.sq))

# Load required library
library(boot)

# Define the GCV function
gcv <- function(y, yhat) {
  mean((y - yhat) ^ 2) / (1 - mean(yhat)^2)
}

# Initialize variables
window_widths <- c(1.2, 1.2, 1.1, 1, 1)  # Range of window widths to consider
gcv_values_kernel4 <- numeric(length(window_widths))

# Calculate GCV for different window widths
for (i in 1:length(window_widths)) {
  kernel_density4 <- density(y, bw = window_widths[i], kernel = "gaussian")
  kernel_reg4 <- glm(kernel_density4$x ~ kernel_density4$y, family = gaussian())
  gcv_values_kernel4[i] <- cv.glm(data = data.frame(x4 = kernel_density4$x, y = kernel_density4$y), glmfit = kernel_reg4, cost = gcv)$delta[1]
}

gcv_values_kernel4
min(gcv_values_kernel4)
which.min(gcv_values_kernel4)

# Determine the optimal window width based on the minimum GCV value
optimal_window_width4 <- window_widths[which.min(gcv_values_kernel4)]
optimal_window_width4
# Build the kernel regression model with the optimal window width
kernel_density4 <- density(y, bw = optimal_window_width4, kernel = "gaussian")

# Load required libraries
library(np)

# Perform kernel regression
kernel_reg4 <- npreg(kernel_density4$y ~ kernel_density4$x, regtype = "ll", bwmethod = "cv.ls")
kernel_reg4

# Plot kernel density and fitted values
par(mfrow=c(1,1))
plot(kernel_density4, main = "Kernel Regression - X4 (TPT)")
lines(kernel_density4$x, fitted(kernel_reg4), col = "red", lwd = 2)
legend("topright", legend = c("Kernel Density", "Fitted Values"), col = c("black", "red"), lwd = c(1, 2), bty = "n")

# Calculate the predicted values
y_pred_kernel4 <- fitted(kernel_reg4)

# Calculate the residuals
residuals_kernel4 <- kernel_density4$y - y_pred_kernel4

# Calculate the total sum of squares
sst4 <- sum((kernel_density4$y - mean(kernel_density4$y))^2)

# Calculate the residual sum of squares
ssr4 <- sum(residuals_kernel4^2)

# Calculate the R-squared
kernel4_rsquared <- 1 - (ssr4 / sst4)

# Calculate the number of predictors
num_predictors4 <- length(coefficients(kernel_reg4)) - 1

# Calculate the number of observations
num_observations <- length(y)

# Calculate the degrees of freedom
df4 <- num_observations - num_predictors4 - 1

# Calculate the adjusted R-squared
kernel4_adj_rsquared <- 1 - (ssr4 / sst4) * ((num_observations - 1) / df4)

# Print the R-squared and adjusted R-squared
print(paste("Kernel Model TPT R-squared:", kernel4_rsquared))
print(paste("Kernel Model TPT Adjusted R-squared:", kernel4_adj_rsquared))

# Calculate MSE
mse_kernel4 <- mean((kernel_density4$y - y_pred_kernel4)^2)

# Calculate RMSE
rmse_kernel4 <- sqrt(mse_kernel4)

# Calculate MAPE
mape_kernel4 <- mean(abs((kernel_density4$y - y_pred_kernel4) / kernel_density4$y)) * 100

# Print the results
print(paste("Kernel Model TPT MSE:", mse_kernel4))
print(paste("Kernel Model TPT RMSE:", rmse_kernel4))
print(paste("Kernel Model TPT MAPE:", mape_kernel4))

# Comparing the model
print(paste("Spline Model TPT Adjusted R Squared:",spline_reg_summary4$r.sq))
print(paste("Kernel Model TPT Adjusted R-squared:", kernel4_adj_rsquared))

# Compare the plot
par(mfrow=c(1,2))
plot(kernel_density4, main = "Kernel Regression - X4 (TPT)")
lines(kernel_density4$x, fitted(kernel_reg4), col = "green", lwd = 2)
legend("topright", legend = c("Data","Kernel Fitted Values"), col = c("black","green"), lwd = c(1,1),cex = 0.65, bty = "n")
plot(x4, y, col = "blue", xlab = "TPT", ylab = "Persentase Kemiskinan", main = "Spline Regression - X4 (TPT)")
lines(x4, predicted_spline4, col = "red", lwd = 2)
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1,1),cex = 0.65, bty = "n")

# X5 = RLS
x5 <- data$RLS
y <- data$`Persentase Kemiskinan`

# Define the range of possible knots
knot_range <- seq(0.5, 9.5, by = 1)
knot_range
# Empty vector to store GCV scores
gcv_scores_spline5 <- vector()

# Iterate over possible knot positions
for (knot in knot_range) {
  # Build the spline regression model with current knot
  spline_reg5 <- gam(y ~ bs(x5, knots = knot))
  # Calculate GCV score
  gcv_score_spline5 <- gam.check(spline_reg5)$gcv.ubre
  # Append GCV score to vector
  gcv_scores_spline5 <- c(gcv_scores_spline5, gcv_score_spline5)
}

gcv_scores_spline5
min(gcv_scores_spline5)
which.min(gcv_scores_spline5)

# Determine the optimal knot position
optimal_knot5 <- knot_range[which.min(gcv_scores_spline5)]
optimal_knot5
# Build the spline regression model using the optimal knot position
spline_reg_model5 <- gam(y ~ bs(x5, knots = optimal_knot5))
spline_reg_summary5 <- summary(spline_reg_model5)
spline_reg_model5
spline_reg_summary5
# Get the predicted values from the model
predicted_spline5 <- predict(spline_reg_model5)

# Create a scatter plot of the data
par(mfrow=c(1,1))
plot(x5, y, pch = 16, col = "blue", xlab = "RLS", ylab = "Persentase Kemiskinan", main = "Spline Regression")

# Add the spline regression line to the plot
lines(x5, predicted_spline5, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1, 2), bty = "n")

# Calculate the residuals
residuals_spline5 <- y - predicted_spline5

# Calculate MSE
mse_spline5 <- mean(residuals_spline5^2)

# Calculate RMSE
rmse_spline5 <- sqrt(mse_spline5)

# Calculate MAPE
mape_spline5 <- mean(abs(residuals_spline5/y)) * 100

# Print the results
print(paste("Spline Model RLS MSE:", mse_spline5))
print(paste("Spline Model RLS RMSE:", rmse_spline5))
print(paste("Spline Model RLS MAPE:", mape_spline5))
print(paste("Spline Model RLS Adjusted R Squared:",spline_reg_summary5$r.sq))

# Load required library
library(boot)

# Define the GCV function
gcv <- function(y, yhat) {
  mean((y - yhat) ^ 2) / (1 - mean(yhat)^2)
}

# Initialize variables
window_widths <- c(1.2, 1.2, 1.1, 1, 1)  # Range of window widths to consider
gcv_values_kernel5 <- numeric(length(window_widths))

# Calculate GCV for different window widths
for (i in 1:length(window_widths)) {
  kernel_density5 <- density(y, bw = window_widths[i], kernel = "gaussian")
  kernel_reg5 <- glm(kernel_density5$x ~ kernel_density5$y, family = gaussian())
  gcv_values_kernel5[i] <- cv.glm(data = data.frame(x5 = kernel_density5$x, y = kernel_density5$y), glmfit = kernel_reg5, cost = gcv)$delta[1]
}

gcv_values_kernel5
min(gcv_values_kernel5)
which.min(gcv_values_kernel5)

# Determine the optimal window width based on the minimum GCV value
optimal_window_width5 <- window_widths[which.min(gcv_values_kernel5)]
optimal_window_width5
# Build the kernel regression model with the optimal window width
kernel_density5 <- density(y, bw = optimal_window_width5, kernel = "gaussian")

# Load required libraries
library(np)

# Perform kernel regression
kernel_reg5 <- npreg(kernel_density5$y ~ kernel_density5$x, regtype = "ll", bwmethod = "cv.ls")
kernel_reg5

# Plot kernel density and fitted values
par(mfrow=c(1,1))
plot(kernel_density5, main = "Kernel Regression - X5 (RLS)")
lines(kernel_density5$x, fitted(kernel_reg5), col = "red", lwd = 2)
legend("topright", legend = c("Kernel Density", "Fitted Values"), col = c("black", "red"), lwd = c(1, 2), bty = "n")

# Calculate the predicted values
y_pred_kernel5 <- fitted(kernel_reg5)

# Calculate the residuals
residuals_kernel5 <- kernel_density5$y - y_pred_kernel5

# Calculate the total sum of squares
sst5 <- sum((kernel_density5$y - mean(kernel_density5$y))^2)

# Calculate the residual sum of squares
ssr5 <- sum(residuals_kernel5^2)

# Calculate the R-squared
kernel5_rsquared <- 1 - (ssr5 / sst5)

# Calculate the number of predictors
num_predictors5 <- length(coefficients(kernel_reg5)) - 1

# Calculate the number of observations
num_observations <- length(y)

# Calculate the degrees of freedom
df5 <- num_observations - num_predictors5 - 1

# Calculate the adjusted R-squared
kernel5_adj_rsquared <- 1 - (ssr5 / sst5) * ((num_observations - 1) / df5)

# Print the R-squared and adjusted R-squared
print(paste("Kernel Model RLS R-squared:", kernel5_rsquared))
print(paste("Kernel Model RLS Adjusted R-squared:", kernel5_adj_rsquared))

# Calculate MSE
mse_kernel5 <- mean((kernel_density5$y - y_pred_kernel5)^2)

# Calculate RMSE
rmse_kernel5 <- sqrt(mse_kernel5)

# Calculate MAPE
mape_kernel5 <- mean(abs((kernel_density5$y - y_pred_kernel5) / kernel_density5$y)) * 100

# Print the results
print(paste("Kernel Model RLS MSE:", mse_kernel5))
print(paste("Kernel Model RLS RMSE:", rmse_kernel5))
print(paste("Kernel Model RLS MAPE:", mape_kernel5))

# Comparing the model
print(paste("Spline Model RLS Adjusted R Squared:",spline_reg_summary5$r.sq))
print(paste("Kernel Model RLS Adjusted R-squared:", kernel5_adj_rsquared))

# Compare the plot
par(mfrow=c(1,2))
plot(kernel_density5, main = "Kernel Regression - X5 (RLS)")
lines(kernel_density5$x, fitted(kernel_reg5), col = "green", lwd = 2)
legend("topright", legend = c("Data","Kernel Fitted Values"), col = c("black","green"), lwd = c(1,1),cex = 0.65, bty = "n")
plot(x5, y, col = "blue", xlab = "RLS", ylab = "Persentase Kemiskinan", main = "Spline Regression - X5 (RLS)")
lines(x5, predicted_spline5, col = "red", lwd = 2)
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1,1),cex = 0.65, bty = "n")

# Compare all the optimal knot and window width
optimal_knot1
optimal_window_width1
optimal_knot2
optimal_window_width2
optimal_knot3
optimal_window_width3
optimal_knot4
optimal_window_width4
optimal_knot5
optimal_window_width5

# Compare all the model Adjusted R-Squared
print(paste("Spline Model IPM Adjusted R Squared:",spline_reg_summary1$r.sq))
print(paste("Kernel Model IPM Adjusted R-squared:", kernel1_adj_rsquared))
print(paste("Spline Model ADHB Adjusted R Squared:",spline_reg_summary2$r.sq))
print(paste("Kernel Model ADHB Adjusted R-squared:", kernel2_adj_rsquared))
print(paste("Spline Model ADHK Adjusted R Squared:",spline_reg_summary3$r.sq))
print(paste("Kernel Model ADHK Adjusted R-squared:", kernel3_adj_rsquared))
print(paste("Spline Model TPT Adjusted R Squared:",spline_reg_summary4$r.sq))
print(paste("Kernel Model TPT Adjusted R-squared:", kernel4_adj_rsquared))
print(paste("Spline Model RLS Adjusted R Squared:",spline_reg_summary5$r.sq))
print(paste("Kernel Model RLS Adjusted R-squared:", kernel5_adj_rsquared))

# Compare the plot
par(mfrow=c(1,2))
plot(kernel_density1, main = "Kernel Regression - X1 (IPM)")
lines(kernel_density1$x, fitted(kernel_reg1), col = "green", lwd = 2)
legend("topright", legend = c("Data","Kernel Fitted Values"), col = c("black","green"), lwd = c(1,1),cex = 0.65, bty = "n")
plot(x1, y, col = "blue", xlab = "IPM", ylab = "Persentase Kemiskinan", main = "Spline Regression - X1 (IPM)")
lines(x1, predicted_spline1, col = "red", lwd = 2)
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1,1),cex = 0.65, bty = "n")

plot(kernel_density2, main = "Kernel Regression - X2 (ADHB)")
lines(kernel_density2$x, fitted(kernel_reg2), col = "green", lwd = 2)
legend("topright", legend = c("Data","Kernel Fitted Values"), col = c("black","green"), lwd = c(1,1),cex = 0.65, bty = "n")
plot(x2, y, col = "blue", xlab = "ADHB", ylab = "Persentase Kemiskinan", main = "Spline Regression - X2 (ADHB)")
lines(x2, predicted_spline2, col = "red", lwd = 2)
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1,1),cex = 0.65, bty = "n")

plot(kernel_density3, main = "Kernel Regression - X3 (ADHK)")
lines(kernel_density3$x, fitted(kernel_reg3), col = "green", lwd = 2)
legend("topright", legend = c("Data","Kernel Fitted Values"), col = c("black","green"), lwd = c(1,1),cex = 0.65, bty = "n")
plot(x3, y, col = "blue", xlab = "ADHK", ylab = "Persentase Kemiskinan", main = "Spline Regression - X3 (ADHK)")
lines(x3, predicted_spline3, col = "red", lwd = 2)
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1,1),cex = 0.65, bty = "n")

plot(kernel_density4, main = "Kernel Regression - X4 (TPT)")
lines(kernel_density4$x, fitted(kernel_reg4), col = "green", lwd = 2)
legend("topright", legend = c("Data","Kernel Fitted Values"), col = c("black","green"), lwd = c(1,1),cex = 0.65, bty = "n")
plot(x4, y, col = "blue", xlab = "TPT", ylab = "Persentase Kemiskinan", main = "Spline Regression - X4 (TPT)")
lines(x4, predicted_spline4, col = "red", lwd = 2)
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1,1),cex = 0.65, bty = "n")

plot(kernel_density5, main = "Kernel Regression - X5 (RLS)")
lines(kernel_density5$x, fitted(kernel_reg5), col = "green", lwd = 2)
legend("topright", legend = c("Data","Kernel Fitted Values"), col = c("black","green"), lwd = c(1,1),cex = 0.65, bty = "n")
plot(x5, y, col = "blue", xlab = "RLS", ylab = "Persentase Kemiskinan", main = "Spline Regression - X5 (RLS)")
lines(x5, predicted_spline5, col = "red", lwd = 2)
legend("topright", legend = c("Data", "Spline Regression"), col = c("blue", "red"), lwd = c(1,1),cex = 0.65, bty = "n")
