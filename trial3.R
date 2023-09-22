
library(egoOptim)
library(parallel)
dom <- domain('camel3')
control <- list(trueglobal = dom$opt$f, n =10,budget=15,
        kmcontrol = list(multistart = 10, nugget = 1e-7,
                         scaling = TRUE,
                         covtype = 'matern5_2'))
bounds <- data.frame(t(data.frame(dom[1:2])))
controlList <- list(EGO = c(control, basicEGO = TRUE,method = 'TREGO'),
                    TREGO = c(control,basicEGO = TRUE, method = 'fastEGO'),
                    RSO = control)

lower <- dom$lower
upper <- dom$upper

ncores <- if(.Platform$OS.type=='windows') 1 else detectCores()
cat("ncores:", ncores, "\n")

f <- \(ctr,fun, X,y) optimize_fun(fun, lower, upper, X = X, y = y, control = ctr)$env
errfun <- \(x)  rbind(x$EGO$error_init, sapply(x, getElement, "errors"))
timefun <- \(x) sapply(x, getElement, 'end_time')


run1 <- function(fun, dom, control){
  X <- lhs::maximinLHS(control$n, length(dom$lower))
  X <- mcmapply(scales::rescale, data.frame(X), bounds, mc.cores = ncores)
  y <- apply(X, 1, fun)
  Envs <- mclapply(controlList, f, fun, X,y, mc.cores = ncores)
  results <- list(errors = errfun(Envs), time = timefun(Envs))
  structure(results, class='egoOptimList', a=Envs)
}


a <- run1(camel3, dom, control)
print(names(a))
cat("\n\n------ errors ------\n")
print(a$errors)

cat("\n\n-----times-----\n")
print(a$time)

cat("\n\n-----Envs-----\n")
print(ls(attr(a, 'a')$EGO))
