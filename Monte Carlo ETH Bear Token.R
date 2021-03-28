data <- read.csv("ETH-USD.csv", stringsAsFactors = FALSE, header = TRUE)
histPx <- data[,5]
ETH.R <- diff(log(histPx))
ETH.vol <- sd(ETH.R) / sqrt(1/252)

acf(ETH.R)
qqnorm(ETH.R)
qqline(ETH.R)

set.seed(2)
d <- 500
T <- 1
m <- T * 252
delta.t <- T / m
r <- 0.015
K <- 2500
B <- 1000
stockPx <- rep(0, m + 1)
payoff <- rep(0, d)
stockPx[1] <- histPx[nrow(data)]

for(j in 1:d)
{
  knockedOut <- FALSE
  for (i in 2:(m + 1)) 
  {     
    W <- sqrt(delta.t) * rnorm(1)
    ret <- (r-(1/2)*ETH.vol^2) * delta.t + ETH.vol * W
    stockPx[i] <- stockPx[i - 1] * exp(ret)
    if (stockPx[i] < B)
    {
      knockedOut <- TRUE
      break
    }
  }
  if (knockedOut == FALSE)
  {
    payoff[j] <- exp(-r * T) * max(stockPx[m + 1] - K, 0)
  } else {
    payoff[j] <- 0
  }
}

cat("Option Price Estimate:", round(mean(payoff), 4), "\n")
