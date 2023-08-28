
#' Optimize fun
#'
#' Uses EGO algorithm to optimize any given function.
#'
#' @author BLANK
#'
#' @export
#' @importFrom lhs maximinLHS
#' @importFrom DiceOptim fastEGO.nsteps TREGO.nsteps
#' @param fun function to be optimized
#' @param lower vector of lower bounds for the variables to be optimized over,
#' @param upper vector of upper bounds for the variables to be optimized over,
#' @param ... additional arguments to be passed to fun
#' @param X,y the initial points where fun has been evaluated. If null, a maximinLHS
#' design is used to generate 5d points as X and y_i = fun(X_i)
#' @param rho a double [0,1] to determine the ton n observations to be used for
#' region of interest determination
#' @param budget The total number of function evaluations to be carried out
#' @param maximize logical. Should we maximize or minimize (the default)?
#' @param expansion_rate double [0,1] for the ROI expansion in case of a failed iteration. 0 implies reverting back to
#' the initial function domain.
#' @param control a list of control parameters. See ‘Details’.
#'
#' @details
#'  \describe{
#'  \item{\code{nsteps}}{A positive integer representing the
#'  desired number of additional points per iteration}
#'  \item{\code{cost}}{cost function to evaluate the loss}
#'  \item{\code{trace}}{logical. Tracing the information on the progress of the optimization}
#'  \item{\code{trueglobal}}{double. The true function optimum.}
#'  \item{\code{tol}}{double. the desired accuracy level.ie convergence tolerance.
#'  If reached before maxit, the code stops and the result returned}
#'  \item{\code{do_maxit}}{logical. Whether to iterate until maxit}
#'  \item{\code{counter}}{Integer. Number of times to switch to global search once local search is unsuccessful}
#'  \item{\code{plot}}{logical. Should the Region be ploted?}
#'  \item{\code{dimplot}}{an integer vector of length 2. The dimensions of the function to be plotted}
#'  \item{\code{basicEGO}}{logical. Whether to maintain the domain as given.
#'  TRUE denotes that the domain will not change throughout the optimization. This is the basic EGO algorithm}
#'  \item{\code{method}}{choise between \code{fastEGO} and \code{TREGO}}
#'  \item{\code{n}}{number of observations for the initial design.
#'  Used if X is not provided. Defaults to \code{5*length(lower)}}
#'  \item{\code{maxit}}{Integer. maximum number of iterations to be carried out. Defaults to 20}
#'  \item{\code{seed}}{Set a seed for reproducability}
#' }
#'
optimize_fun <- function(fun, lower, upper, ..., X = NULL, y = NULL, rho = 0.3,
                          maximize = FALSE, budget = NULL, expansion_rate = 0,
                         control = list()){
  ctr <- control_pars(control,lower)
  # Rewrite the function in case of maximization:
  .fun <-  function(x) (-1)^(maximize)*fun(x, ...)
  if(!is.null(ctr$trueglobal)) ctr$trueglobal <- (-1)^maximize*ctr$trueglobal
  # Initialization
  count <- 0
  signifs <- ceiling(1-log10(ctr$tol))
  init_lower <- lower
  init_upper <- upper
  method <- getFromNamespace(paste0(ctr$method[1], ".nsteps"), "DiceOptim")


  # Generate scaled X matrix and Y values
  p <- length(lower)

  if(!is.null(budget)) {
    add <- budget - ctr$n
    if(add<=0) stop('budget must be at least ', budget - add)
    ctr$n <- ctr$n + add %% ctr$nsteps
    ctr$maxit <- add %/% ctr$nsteps
    if(ctr$maxit<=0) stop('budget must be at least ', budget - add + ctr$nsteps)
    ctr$do_maxit <- TRUE
  }
  errors <- numeric(ctr$maxit)
  set.seed(ctr$seed)
  if(is.null(X)) {
    X <- lhs::maximinLHS(ctr$n, p)
    X <- mapply(rescale, data.frame(X), data.frame(rbind(lower, upper)))
    y <- apply(X, 1, .fun) #
  }


  # Starting Optimal value -- Enable to compute error.
  optimal <- min(y)
  error_init <- if(!is.null(ctr$trueglobal)) {
    maximize + optimal - ctr$trueglobal*(!maximize)
  }else NULL

  if(!is.function(ctr$cost)) ctr$cost <- .fun
  loss_init <- ctr$cost(X[which.min(y),])
  # Run kriging model
  set.seed(ctr$seed)
  model <- DiceKriging::km(design = X, response = y, control = list(trace = 0))


  #plot the first 2 dimensions holding the other dimensions at midpoint:
  if(ctr$plot){
    use_colors <- c("red", "blue", "green","yellow", "orange","purple",
                    rainbow(ctr$maxit))
    if(length(ctr$dimplot)!=2) stop("you can only plot 2d", call. = FALSE)
    col_order <- order(c(ctr$dimplot, setdiff(seq_len(p), ctr$dimplot)))

    rem <- if (p>2) tail((lower + upper)/2, -2) else NULL
    x1 <- seq(lower[ctr$dimplot[1]], upper[ctr$dimplot[1]], length = 50)
    x2 <- seq(lower[ctr$dimplot[2]], upper[ctr$dimplot[2]], length = 50)
    Z <- outer(x1, x2, Vectorize(\(x, y).fun(c(x, y, rem)[col_order])))
    my_contour(x1, x2, Z)
    points(cbind(X, y), pch=16, col=1)
    rect(lower[ctr$dimplot[1]], lower[ctr$dimplot[2]],
         upper[ctr$dimplot[1]], upper[ctr$dimplot[2]], border = 'black')
  }
  loss <- NULL
  for(i in seq_len(ctr$maxit)){

    #Fit the EGO
    if(!is.null(ctr$seed))set.seed(ctr$seed + i)
    oEGO <- method(model, .fun, ctr$nsteps, lower, upper,trace = FALSE)
    model <- oEGO$lastmodel

    # Obtain the center and ROI
    o <- order(model@y)
    center <- model@X[o[1], ]

    # Compute the error and set the previous optimal to be the current optimal
    err <- optimal - model@y[o[1]]
    optimal <- model@y[o[1]]
    if(is.function(ctr$cost)) loss[i] <- ctr$cost(center)

    if(!ctr$basicEGO) {
      top_n <- ceiling(rho * nrow(model@X))
      dist <- diff(apply( model@X[o[seq_len(top_n)], ], 2, range))/2
      lower <- c(pmax(center - dist, init_lower))
      upper <- c(pmin(center + dist, init_upper))
      if(err < ctr$tol) {
        count <- count + 1
        if(expansion_rate > 0 ){
          lower <- c(pmax(center - (1 + expansion_rate)^(1/p)*dist, init_lower))
          upper <- c(pmin(center + (1 + expansion_rate)^(1/p)*dist, init_upper))
        }
        else {
          lower <- init_lower
          upper <- init_upper
        }
      }
    }

    #Plot the new ROI and the added points
    if(ctr$plot){
      rect(lower[ctr$dimplot[1]], lower[ctr$dimplot[2]],
           upper[ctr$dimplot[1]], upper[ctr$dimplot[2]], border = use_colors[i])
      points(oEGO$par[,ctr$dimplot, drop = FALSE], col=use_colors[i], pch=16)
      Sys.sleep(1)
    }
    if(!is.null(ctr$trueglobal)) {
      trueERR <- optimal - ctr$trueglobal
      errors[i] <- trueERR
    }

    if(ctr$trace){
      cat(sprintf(sprintf("it: %02d\tf(x*): %%.%df\terr: %%.%df\t", i, signifs, signifs),
                  optimal*(-1)^(maximize),err))
      if(p<6){
        fmt <- sprintf("[%s]\tcount:%02i\t", trimws(strrep("%.4f, ", p),'r',", "), count)
        cat(do.call(sprintf, c(fmt, as.list(center))))
      }
      if(!is.null(ctr$trueglobal))cat(sprintf(sprintf("trueERR: %%.%df", signifs), trueERR))
      cat("\n")
    }


    if((count >= ctr$counter  || !is.null(ctr$trueglobal) && trueERR < ctr$tol) && !ctr$do_maxit) break
    #cat("lower:[", lower,"] upper:[", upper,"]\n")

  }
  structure(list(par = unname(center),
                 value = optimal * (-1)^(maximize),
                 model = model,
                 call = match.call(expand.dots = TRUE),
                 env = environment(),
                 errors = c(error_init, errors),
                 loss = c(loss_init, loss)),
            class = 'egoOptim')
}



print.egoOptim <- function(x){
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("\t\t\t\tKriging Based RSO\n")
  cat(strrep("=", 73), "\n")
  cat(do.call(sprintf,c(sprintf("\tx*=[%s]", trimws(strrep("%.4f, ", x$env$model@d),'r',", ")),
                        as.list(unname(x$env$center)))))
  cat(sprintf("\tf(x*) = %.4f\t\t", x$env$optimal))
  cat("Total Points:", x$env$model@n, "\n")
  cat(strrep("_", 73), "\n")
}

#' @importFrom utils modifyList
control_pars <- function(x, lower){
  nms <- names(x)
  if(any(!nzchar(nms)))stop("arguments in control list must be named")

  y <- list(nsteps = 5, cost = NULL, trueglobal = NULL,
            maxit = 20, tol = 1e-4, do_maxit = FALSE, counter = 3,
            plot = FALSE, dimplot = 1:2, basicEGO = FALSE,
            method = c('fastEGO','TREGO'), n = 5*length(lower),
            seed = NULL,  trace = TRUE)
  if(any(z<-!nms%in%names(y)))
    stop("there is no ", toString(nms[z]),"in contol list")
  z <- modifyList(y, x)
  if(!is.null(z$trueglobal)) z$counter <- z$maxit
  z
}
.S3method('print', 'egoOptim')
