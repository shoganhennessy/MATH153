# We want a MH chain that jumps all over the space a lot
# One that does not stay in one place a lot



# target is normal(3,1) <- f
# have candidate normal(1,1) <- g

x <- c() # initiate chain
x[1] <- 1 # starting point

for (i in c(2:10000)){
  y <- rnorm(1) # generate y
  u <- runif(1) # generate a probability in (0, 1)
  r <- dnorm(y, 3, 1)/dnorm(x[i-1], 3, 1)*dnorm(x[i-1])/dnorm(y) # compute MH ratio
  x[i] <- x[i-1]
  if (u < r){ x[i] <- y } # change according to probability of MH ratio
}
mean(x)
hist(x) # not too similar to normal(3,1)



# target is normal(3,1) <- f
# have candidate t dist with 3 df <- g

x <- c() # initiate chain
x[1] <- 1 # starting point

for (i in c(2:10000)){
  y <- rt(1, 3) # generate y
  u <- runif(1) # generate a probability in (0, 1)
  r <- dnorm(y, 3, 1)/dnorm(x[i-1], 3, 1)*dt(x[i-1], 3)/dt(y, 3) # compute MH ratio
  x[i] <- x[i-1] 
  if (u < r){ x[i] <- y } # change according to probability of MH ratio
}
mean(x)
hist(x) # similar to normal(3,1)