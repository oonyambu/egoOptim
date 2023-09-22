
if(!require(egoOptim)){
  if(!require(remotes)) install.packages('remotes')
  remotes::install_github('oonyambu/egoOptim')
  rstudioapi::restartSession()
}

library(egoOptim)

low <- c(2^-6, 1e-2)
up <- c(2^3, 1e2)
reps <- 100
budget <- 100
rho <- 0.3
#expansion_rate <- NULL#c(0.1, 0.2, 0.3,0.5,1)
trace <- FALSE
nsteps <- 5
n <- 20

control = list(trueglobal=1,n = n,budget=budget,
             trace = trace, nsteps = nsteps,
             kmcontrol = list(multistart = 10, pop.size=100,
                              nugget = 1e-8))

maternalHealth1 <- cbind(maternalHealth[1], scale(maternalHealth[-1]))

method_compare(svm_train, low, up, train=maternalHealth1,
           reps = reps, rho = rho, maximize = TRUE,
           control = control,    overwrite = trace,
           file = 'results/maternal_healthreps100.txt')
