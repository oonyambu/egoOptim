

#' @importFrom grDevices rainbow
#' @importFrom graphics axis contour grid par points rect
#' @importFrom stats sd setNames
#' @importFrom utils getFromNamespace suppressForeignCheck stack tail
#' type.convert write.csv

utils::suppressForeignCheck(c('Var1', 'error_init','nsteps', 'point'))
requireNamespace("ggplot2")

#' Function to compare different methods
#'
#' @author BLANK
#'
#' @param fun function to be optimized
#' @param lower vector of lower bounds for the variables to be optimized over,
#' @param upper vector of upper bounds for the variables to be optimized over,
#' @param ... additional arguments to be passed to fun
#' @param budget The total number of function evaluations to be carried out
#' @param maximize logical. Should we maximize or minimize (the default)?
#' @param p Integer. The dimension of the function in case lower is not given.
#' @param expansion_rate double [0,1]. For the ROI expansion in case of a
#' failed iteration. 0 implies reverting back to
#' the initial function domain.
#' @param reps Integer, number of replicates/trials.
#' @param file a character string specifying where the results will be stored
#' in the hard drive.
#' @param control a list of control parameters passed to the optimize_fun.
#'  See ‘optimized_fun’ for more information.
#' @export
#'
method_compare <- function(fun,lower, upper, ..., budget = 70, p = NULL,
                           maximize = FALSE, reps = 20L,
                           expansion_rate= 0, file = NULL,
                           control = list(nsteps = 3)){
  fun_name <- gsub("\\W", '', deparse1(substitute(fun)))

  optimal <- control$trueglobal
  if(missing(lower)){
    dom <- domain(fun)
    fun <- getFromNamespace(fun, 'egoOptim')
    if(is.function(dom)){
      if(is.null(p)) stop('the dimension `p` must be provided for ', fun_name)
      else dom <- dom(p)
    }
    if(is.null(optimal)) optimal <- dom$opt$f[1]
    lower <- if(!is.null(dom$lower))dom$lower else rep(0, p)
    upper <- if(!is.null(dom$upper))dom$upper else rep(1, p)
  }
  if(maximize & is.null(optimal)) optimal <- -1
  control$trueglobal <- optimal
  res <- setNames(vector('list', 3), c('RSO', 'EGO', 'TREGO'))
  control$do_maxit <- TRUE
  RScontrol <- modifyList(control, list(basicEGO = FALSE))
  EGcontrol <- modifyList(control, list(basicEGO = TRUE))
  TRcontrol <- modifyList(control, list(basicEGO = TRUE, method = 'TREGO'))

  for(i in seq_len(reps)){
    X <- lhs::maximinLHS(5*length(lower), length(lower))
    X1 <- mapply(rescale, data.frame(X),data.frame(rbind(lower, upper)))
    y1 <- apply(X1, 1, function(x) (-1)^(maximize)*fun(x, ...))
    cat("RSO ITERATION:", i, "\n")
    res[['RSO']][[i]] <- optimize_fun(fun, lower, upper,..., X = X1, y=y1,
                                      budget = budget,
                                      maximize = maximize,
                                      control = RScontrol)
    cat("EGO ITERATION:", i, "\n")
    res[['EGO']][[i]] <- optimize_fun(fun, lower, upper,..., X = X1, y=y1,
                                      budget = budget,
                                      maximize = maximize,
                                      control = EGcontrol)
    cat("TREGO ITERATION:", i, "\n")
    res[['TREGO']][[i]] <- optimize_fun(fun, lower, upper,..., X = X1, y=y1,
                                        maximize = maximize,
                                        budget = budget,
                                        control = TRcontrol)
    if(expansion_rate>0){
      cat("RSO",expansion_rate, "ITERATION: ", i, "\n", sep="")
      res[['RSO1']][[i]] <- optimize_fun(fun, lower, upper,..., X = X1, y=y1,
                                          expansion_rate = expansion_rate,
                                          maximize = maximize,
                                          budget = budget,
                                          control = RScontrol)
    }
  }
  r <- lapply(res, \(x){
    vals <- sapply(x, getElement, 'errors')
    vals <- if(maximize) 1- vals else log10(vals)
    y <- data.frame(t(apply(vals, 1,
                            \(y)c(mean = mean(y), sd = sd(y)))))
    y})
  d <- transform(array2DF(structure(r, dim = 3)),
                 point = res$RSO[[1]]$env$ctr$nsteps *
                   seq(0,nrow(r[[1]])-1))
  assign(fun_name, d)
  if(!is.null(file)) write.csv(d, file = file)
  list(res=res, plot = plotComparison(d,
                          maximize = maximize,
                          nsteps = control$nsteps,
                          errorbars = !maximize))
}

rescale <- function(x, to){
  (x - min(x))/diff(range(x))*diff(to) + to[1]
}


#branin_results <- method_compare('branin', budget = 35)
# save(branin_results, file = "data/branin_results.rda")
#
# hart3_results <- my_fn('hart3', budget = 80)
# save(hart3_results, file = "data/hart3_results.rda")
#
# hart6_results <- my_fn('hart6', budget = 80)
# save(hart6_results, file = "data/hart6_results.rda")
#
# camel3_results <- my_fn('camel3', budget = 80)
# save(camel3_results, file = "data/camel3_results.rda")
#
# camel6_results <- my_fn('camel6', budget = 80)
# save(camel6_results, file = "data/camel6_results.rda")
#

# dat <- read.csv('../../data/spambase/spambase.data', h=F, col.names = nms)
#
# names(dat) <- nms
# dat$class <- factor(dat$class)
#
# spambase <- tidyr::as_tibble(model.frame(class~.,dat))
# save(spambase, file = "data/spambase.rda")
#
#
#
#
# wine_results <- my_fn(svm_accuracy, c(2^-16, 2^-6), c(2^6, 2^16),
#       param_names = c('gamma', 'cost'),
#       object = svm(class~., wine, kernel = 'radial'),
#       maximize = TRUE)
# #
# accoustic_results <- my_fn(svm_accuracy, c(2^-16, 2^-6), c(2^6, 2^16),
#             param_names = c('gamma', 'cost'),
#             object = svm(Class~., accoustic, kernel = 'radial'),
#             maximize = TRUE)

# seed_results <- my_fn(svm_accuracy, c(2^-16, 2^-6), c(2^6, 2^16),
#                        param_names = c('gamma', 'cost'),
#                        object = svm(Type~., seeds, kernel = 'radial'),
#                        maximize = TRUE)
#

utils::globalVariables(c("Var1", "point"))

#' plot the compared methods
#'
#' Uses EGO algorithm to optimize any given function.
#'
#' @author BLANK
#'
#' @import ggplot2
#' @param res a list of egooptim objects
#' @param n the upper cutoff
#' @param m lower cutoff
#' @param maximize whether to plot accuracy or error
#' @param nsteps determines the breaks at the x axis
#' @param errorbars logical. Whether to plot the errorbars
#' @param which The element in the egoOptim object to be plotted
#' @export


plotComparison <- function(res, n = NULL,
                           maximize = FALSE, m=0,
                           nsteps = 5, errorbars = TRUE, which = 'errors'){
  nn <- 0
  if(!is.data.frame(res)){
    nn <- nrow(res[[1]][[1]]$env$X)
    r <- lapply(res, \(x){
      vals <- sapply(x, getElement, which)
      vals <- if(maximize) 1 - vals else log10(vals)
      y <- data.frame(t(apply(vals, 1,
                              \(y)c(mean = mean(y), sd = sd(y)))))
      y})
    d <- transform(array2DF(structure(r, dim = length(r))),
                   point = seq(0,by = nsteps, length = nrow(r[[1]])))
  }
  else d <- res
  if(is.null(n)) n <- max(d$point)
  d <- subset(d, point >= m & point <= n)
  ggplot(d, aes(x = point, mean, color = Var1))+
    geom_point() +
    geom_line(linewidth = 1) +
    { if(errorbars)
      geom_errorbar(aes(ymax = (if(maximize)pmin(mean + sd,1) else mean+sd),
                        ymin =(if(maximize) pmax(mean - sd, 0) else mean - sd)),
                    linewidth = 1, width = 1, alpha=0.5)
    }+
    labs(color = 'Method') +
    ylab(if(maximize)'accuracy' else bquote(Log[10]* ' Loss')) +
    xlab('Total points Used') +
    scale_x_continuous(labels=~.x + nn)
}



#' contour plots
#'
#' Uses EGO algorithm to optimize any given function.
#'
#' @author BLANK
#'
#' @param x1,x2 locations of grid lines at which the values in z are measured.
#'  These must be in ascending order. By default, equally spaced values
#' from 0 to 1 are used. If x is a list, its components x$x and x$y are
#' used for x and y, respectively. If the list has
#' component z this is used for z.
#'
#' @param Z a matrix containing the values to be plotted
#' (NAs are allowed). Note that x can be used instead of z
#' for convenience.
#'
#' @param xlab,ylab labels for x and y axis respectively. defaults to NULL
#'
#' @param title,subtitle title/subtitle of the plot
#'
#' @param background the background color
#'
#' @param title.adjust adjust the title
#'
#' @param nlevels number of contour levels desired *iff* levels is not supplied.
#'
#' @param col the color for the contours
#'
#' @param ... 	additional arguments to plot.window, title, Axis and box,
#'     typically graphical parameters such as cex.axis.
#' @export
#'
my_contour <- function(x1, x2, Z, xlab = NULL,
                       ylab = NULL, title = NULL,subtitle = "it1",
                       background = "white", title.adjust = 0.5,
                       nlevels=100, col='blue', ...){
  par(bg = background,
      adj=0.5,
      tck = -0.02,
      #mgp = c(1.5, 0.4, 0), tcl = -0.25,
      ## Shrink the tick labels.
      cex.axis = 0.8,
      ## Set the axis label color
      col.lab = "black",
      ## Adjust the margin:  bottom, left, top, right
      mar = c(2, 2.5, 2, 1.2)-c(0,0,is.null(title), 0),
      bty = "n",
      mgp = c(1, 0.2, 0))

  if(is.null(xlab)) xlab <- deparse1(substitute(x1))
  if(is.null(ylab)) ylab <- deparse1(substitute(x2))
  contour(x1, x2, Z, xaxt = "n", yaxt = "n")

  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = "#ebebeb", border = background)

  grid(nx = 5, ny = 5,
       lty = 1,      # Grid line type
       col = "white", # Grid line color
       lwd = 2)      # Grid line width
  contour(x1, x2, Z, add = TRUE, nlevels = nlevels,...)

  #box("plot", bty = "l", lwd = 2)
  axis(side = 1, lwd = 0, lwd.ticks = 1, mgp=c(1, 0.2, 0))
  axis(side = 2, lwd = 0, lwd.ticks = 1, las = 2, mgp=c(1.5, 0.4, 0))
  title(main = title, xlab = xlab, ylab=ylab,
        adj = title.adjust, sub = subtitle)

}

#' budget addition
#'
#' Uses EGO algorithm to optimize any given function.
#'
#' @author BLANK
#' @param object an egoOptim object or a list of egoOptim objects
#' @param new_budget the number of points to be added
#' @param its the new number of iterations
#' @export




add_budget <- function(object, its, new_budget){
  UseMethod('add_budget')
}

add_budget.egoOptim <- function(object, new_budget, its){
  if(!missing(its)) object$env$ctr$maxit <- its
  if(!missing(new_budget)) {
    object$env$ctr$maxit <- new_budget %/% object$env$ctr$nsteps
    object$env$ctr$do_maxit <- TRUE
  }
  new_err <- object$env$errors
  errors <- 0
  v <- body(optimize_fun)
  len <- length(v)
  v[[len]][[2]][[5]] <- NULL

  results <- eval(call("{", v[[len - 1]], v[[len]]),
                 as.list(object$env,all.names = TRUE))
  results$errors <- c(object$errors, results$env$errors)
  results$call <- object$call
  results
}

add_budget.list <- function(object, its, new_budget){
  for(i in names(object$res)){
    for(j in seq_along(object$res[[i]])){
      cat("updating", i, "it:", j, "\n")
      object$res[[i]][[j]] <- add_budget(object$res[[i]][[j]],
                                         its = its, new_budget = new_budget)
    }
  }
  object$plot <- plotComparison(object$res)
  object
}

#' Replications addition
#' @author None
#' @export
#' @param object a list of egoObjects
#' @param reps the number of replicates to be added
add_replicates <- function(object, reps){

  a <- object$call$reps
  object$call$reps <- reps
  res <- Map(c, object$res, eval(object$call)$res)
  object$call$reps <- reps + a
  v <- mget(c('maximize','nsteps'), object$res[[1]][[1]]$env)
  list(res = res,
       plot = plotComparison(res, maximize = v$maximize,
                             nsteps = v$nsteps,
                             errorbars = !v$maximize),
       call=object$call)
}

.S3method('add_budget', 'egoOptim')
.S3method('add_budget', 'list')


