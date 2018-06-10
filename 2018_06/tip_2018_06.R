library(quantmod)
library(lubridate)
library(microbenchmark)
library(ggplot2)
library(parallel)


# do an exercise for multiple computation 
P1 <- get(getSymbols("SPY",from = "1990-01-01"))[,6]
P2 <- get(getSymbols("AAPL",from = "1990-01-01"))[,6]
P <- merge(P1,P2)
R <- na.omit(P/lag(P))-1
names(R) <- c("SPY","AAPL")


beta.f <- function(i) {
  set.seed(i)
  R.i <- R[sample(1:nrow(R),floor(0.5*nrow(R)) ),]
  lm.i <- lm(AAPL~SPY,data = R.i)
  beta.i <- summary(lm.i)$coefficients["SPY",1]
  return(beta.i)
  }



N <- 10^2
f1 <- function() mean(unlist(lapply(1:N, beta.f)))
f2 <- function() mean(unlist(mclapply(1:N, beta.f)) )

ds.time <- microbenchmark(Regular = f1(),Parallel = f2(),times = 100)
ds.time
autoplot(ds.time)
