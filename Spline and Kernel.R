# Generate sample data
set.seed(123)
data <- read_excel('DATA SKRIPSI FIRSAN.xlsx')
data <- data[,-1]

# X1 = IPM
x1 <- data$IPM
y <- data$`Persentase Kemiskinan`

# Splines
library(splines)

#fit spline regression model
spline_fit <- lm(y ~ bs(x1, knots=c(7, 10)))
plot(spline_fit,main = "Spline Regression X1 - IPM")
#view summary of spline regression model
summary(spline_fit)

# Kernel
library(gplm)
kernel_reg <- kreg(x1,y,bandwidth=1.2)
plot(kernel_reg,lwd=2,main = "Kernel Regression X1 - IPM")


# Fit a spline regression
spline_fit <- smooth.spline(x1, y)

# Extract the GCV value
spline_gcv_value <- spline_fit$cv.crit


library(np)

# Fit a kernel regression
kernel_fit <- npreg(y ~ x1, regtype = "ll", bwmethod = "cv.ls")

# Extract the GCV value
kernel_gcv_value <- kernel_fit$gcv
kernel_gcv_value
