library(quantmod)
library(rbenchmark)
library(ggplot2)
rm(list = ls())

# get data
v <- c("SPY","XLF","TSLA")
P.list <- lapply(v, function(v_i) get(getSymbols(v_i, from = "1990-01-01")) )
P.list2 <- lapply(P.list, function(p) p[,6] ) # adjusted prices located in the sixth column

# merge manually
f1 <- function(P.list2) {
  P1 <- merge(merge(P.list2[[1]],P.list2[[2]]),P.list2[[3]])
  return(P1)
}

# merge using a loop
f2 <- function(P.list2) { 
  P2 <- P.list2[[1]]
    for(i in 2:length(P.list2)) {
      P2 <- merge(P2,P.list2[[i]])
    }
  return(P2)
}


# merge using Reduce
f3 <- function(P.list2) { 
  P3 <- Reduce(merge,P.list2)
  return(P3)
  }

ds.time <- microbenchmark(manual = f1(P.list2),loop = f2(P.list2),reduce = f3(P.list2),times = 10^2)
autoplot(ds.time)



