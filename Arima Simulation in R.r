set.seed(6641)
x <- arima.sim(model=list(order=c(1,1,1), ar=0.5, ma=0.3), n=200, rand.gen=rnorm, sd=3)

# initial value is zero
x <- x[-1]

# Plot of the data
library(astsa)
dev.new(width=8, height=6)
tsplot(x, ylab=expression(x[t]), type="o", main=expression(paste("ARIMA model: ", (1 - 0.5*B)*(1-B)*x[t] == (1 + 0.3*B)*w[t])))

# ACF and PACF of x_t
dev.new(width=8, height=6)
acf2(x, max.lag=20, ylim=c(-1,1), main=expression(paste("Estimated ACF & PACF plots for ", x[t])))

# ACF and PACF of first differences
diff_x <- diff(x, lag=1, differences=1)
dev.new(width=8, height=6)
acf2(diff_x, ylim=c(-1,1), max.lag=20, main=expression(paste("Estimated ACF & PACF plots for ", x[t] - x[t-1])))

# True ACF and PACF for ARIMA(1,0,1) (without differences)
ACF <- ARMAacf(ar=0.5, ma=0.3, lag.max=20)[-1]
PACF <- ARMAacf(ar=0.5, ma=0.3, lag.max=20, pacf=TRUE)
dev.new(width=8, height=6)
par(mfrow=c(2,1))
tsplot(ACF, type="h", ylim=c(-1,1), xlab="LAG", ylab="ACF", main="True ACF & PACF plots for ARIMA(1,0,1)")
abline(h=0)
tsplot(PACF, type="h", ylim=c(-1,1), xlab="LAG", ylab="PACF")
abline(h=0)

# Plot of the first differences
dev.new(width=8, height=6)
tsplot(diff_x, ylab=expression(x[t]-x[t-1]), type="o", main="Plot of data after first differences")

# Fit model
mod.fit <- sarima(x, p=1, d=1, q=1, detail=FALSE)
mod.fit

dev.new(width=8, height=6)
forecast <- sarima.for(x, n.ahead=10, p=1, d=1, q=1)
forecast

# Predicted values for t = 1, ..., 100
# residual = observed - predicted
pred_x <- x - resid(mod.fit$fit)

dev.new(width=8, height=6)
tsplot(x, ylab=expression(x[t]), type="o", main=expression(paste("ARIMA model: ", (1 - 0.5*B)*(1-B)*x[t] == (1 + 0.3*B)*w[t])))
lines(pred_x, col="red", type="o", pch=17) 
legend("topleft", legend=c("Observed", "Forecast"), lty=c("solid", "solid"), col=c("black", "red"), pch=c(1, 17), bty="n")

