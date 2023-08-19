#' Optimize fun
#'
#' Uses EGO algorithm to optimize any given function.
#'
#' @author BLANK
#'
#' @export
optimize_fun <-  function (fun, lower, upper, ..., X = NULL, y = NULL, tolerr = 1e-04, 
                maxit = 20, nsteps = 3, rho = 0.3, trueglobal = NULL, maximize = FALSE, 
                do_maxit = FALSE, counter = 3, dimplot = 1:2, budget = NULL, 
                basicEGO = FALSE, n = 5 * length(lower), plot = FALSE, trace = 1, 
                method = c("fastEGO", "TREGO"), increament_rate = 0.1, seed = NULL) 
{
  .fun <- function(x) (-1)^(maximize) * fun(x, ...)
  if (!is.null(trueglobal)) 
    trueglobal <- (-1)^maximize * trueglobal
  count <- 0
  signifs <- ceiling(1 - log10(tolerr))
  init_lower <- lower
  init_upper <- upper
  method <- getFromNamespace(paste0(method[1], ".nsteps"), 
                             "DiceOptim")
  p <- length(lower)
  if (!is.null(budget)) {
    add <- budget - n
    n <- n + add%%nsteps
    maxit <- add%/%nsteps
  }
  errors <- numeric(maxit)
  set.seed(seed)
  if (is.null(X)) {
    X <- lhs::maximinLHS(n, p)
    X <- mapply(scales::rescale, data.frame(X), data.frame(rbind(lower, 
                                                                 upper)))
    if (!is.null(names(lower))) 
      colnames(X) <- names(lower)
    y <- apply(X, 1, .fun)
  }
  optimal <- min(y)
  error_init <- if (!is.null(trueglobal)) {
    maximize + optimal - trueglobal * (!maximize)
  }
  else NULL
  model <- DiceKriging::km(design = X, response = y, control = list(trace = 0))
  if (plot) {
    use_colors <- c("red", "blue", "green", "yellow", "orange", 
                    "purple", rainbow(maxit))
    if (length(dimplot) != 2) 
      stop("you can only plot 2d", call. = FALSE)
    col_order <- order(c(dimplot, setdiff(seq_len(p), dimplot)))
    rem <- if (p > 2) 
      tail((lower + upper)/2, -2)
    else NULL
    x1 <- seq(lower[dimplot[1]], upper[dimplot[1]], length = 50)
    x2 <- seq(lower[dimplot[2]], upper[dimplot[2]], length = 50)
    Z <- outer(x1, x2, Vectorize(function(x, y) .fun(c(x, 
                                                       y, rem)[col_order])))
    my_contour(x1, x2, Z)
    points(cbind(X, y), pch = 16, col = 1)
    rect(lower[1], lower[2], upper[1], upper[2], border = "black")
  }
  for (i in seq_len(maxit)) {
    mod1 <- method(model, .fun, nsteps, lower, upper, trace = FALSE)
    model <- mod1$lastmodel
    o <- order(model@y)
    center <- model@X[o[1], ]
    if (!basicEGO) {
      top_n <- ceiling(rho * nrow(model@X))
      dist <- diff(apply(model@X[o[seq_len(top_n)], ], 
                         2, range))/2
      lower <- c(pmax(center - dist, init_lower))
      upper <- c(pmin(center + dist, init_upper))
    }
    
    err <- optimal - model@y[o[1]]
    optimal <- model@y[o[1]]
    if (trace) 
      cat(sprintf(sprintf("it: %02d\tf(x*): %%.%df\terrr: %%.%df\t", 
                          i, signifs, signifs), optimal * (-1)^(maximize), 
                  err))
    if (p < 6 & trace) {
      fmt <- sprintf("[%s]\tcount:%02i\t", trimws(strrep("%.4f, ", 
                                                         p), "r", ", "), count)
      cat(do.call(sprintf, c(fmt, as.list(center))))
    }
    if (!is.null(trueglobal)) {
      trueERR <- optimal - trueglobal
      errors[i] <- trueERR
      if (trace) 
        cat(sprintf(sprintf("trueERR: %%.%df", signifs), 
                    trueERR))
      if (trueERR < tolerr & !do_maxit) 
        break
    }
    if (trace) 
      cat("\n")
    if (err < tolerr && !basicEGO) {
      if (!is.null(trueglobal) && (trueERR < tolerr) & 
          !do_maxit) 
        break
      if(increament_rate >0){
        lower <- c(pmax(center - (1 + increament_rate)^(1/p) * 
                        dist, init_lower))
        upper <- c(pmin(center + (1 + increament_rate)^(1/p) * 
                        dist, init_upper))
      }
      else {
        lower <- init_lower
        upper <- init_upper
      }
      count <- count + 1
    }
    if (plot) {
      rect(lower[dimplot[1]], lower[dimplot[2]], upper[dimplot[1]], 
           upper[dimplot[2]], border = use_colors[i])
      points(mod1$par[, dimplot, drop = FALSE], col = use_colors[i], 
             pch = 16)
    }
    cat("lower:[", lower, "] upper:[",upper, "] count:", count,"\n")
    if (count >= counter && !do_maxit) 
      break
  }
  
  structure(list(par = unname(center), value = optimal * (-1)^(maximize), 
                 model = model, env = environment(), errors = c(error_init, 
                                                                errors)), class = "egoOptim")
}

#' print.egoOptim
#' print an egoOptim object
#' @param x an object of class egoOptim
#' @export
print.egoOptim <- function(x,...){
  cat("\t\tKriging Based RSO\n")
  cat(strrep("=", 50), "\n")

  cat(do.call(sprintf,
              c(sprintf("\tx*=[%s]", trimws(strrep("%.4f, ", x$model@d),'r',", ")),
                as.list(x$par))))
  cat(sprintf("\tf(x*) = %.4f\n", x$value))
  cat("N used:", x$model@n, "\n")
}

