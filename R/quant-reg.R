library(quantreg)

# fits quantile regression to data
fit.quantreg <- function(y,x,tau) rq(y~log10(x),tau=tau)

# Estimates value for y at given x (predict.at) by fitting quantile regression.
# The cutrade-off value (p) for the gression is adjusted according to the number of points in dataset. If there are few points, use default value for p. Otherwise cutrade-off based on top 20 points
# Returns predicted value of y at given value of x (predict.at)
quantreg95 <- function(x,y, p=0.99, nmin=50, predict.at=10){
  n=length(y)
  if(n<nmin) return(as.numeric(NA))
  f <- fit.quantreg(y, x, p)
  predict(f, newdata=data.frame(x=predict.at))
}

# plot to demonstarte the qunatile gression method for estimating growth rates
quantreg.plot <- function(x,y, p=0.99, nmin=100, predict.at=10,add.legend=TRUE, title,...){
  n=length(y)
  plot(y~x, log="x", main=paste0(title,", n=",n),...)
  if(n > nmin){
    f <- fit.quantreg(y, x, p)
    abline(h=quantile(y, probs=0.95), col="red")
    abline(f, col="blue")
    points(predict.at, predict(f, newdata=data.frame(x=predict.at)), col="blue", pch=16)
  }
  if(add.legend)
    legend("topright", legend = c("Wright2010", "QuantReg"), col=c("red", "blue"), lty=c("solid", "solid"), bty = "n")
}
