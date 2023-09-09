# get function domain
bran_dom <- domain('branin')

# optimize the function
optimize_fun(branin, bran_dom$lower, bran_dom$upper)

# invoke the global optimum
optimize_fun(branin, bran_dom$lower, bran_dom$upper,
    control = list(trueglobal = bran_dom$opt$f))


## Maximize 2-d normal density within [-10,10] and [-10,10]

lower <- c(-10,-10)
upper <- c(10,10)
sig <- diag(2) #can change the covariance/variance matrix
mu <- numeric(2) # can change the center
optimize_fun(mvtnorm::dmvnorm, lower, upper, mean = mu, sigma=sig,
             maximize = TRUE)
