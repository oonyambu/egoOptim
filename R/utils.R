

#' @importFrom lhs maximinLHS
#' @importFrom DiceOptim fastEGO.nsteps TREGO.nsteps
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
#' @param maximize logical. Should we maximize or minimize (the default)?
#' @param p Integer. The dimension of the function in case lower is not given.
#' @param rho a double [0,1] to determine the ton n observations to be used for
#' region of interest determination
#' @param reps Integer, number of replicates/trials.
#' @param file a character string specifying where the results will be stored
#' in the hard drive.
#' @param control a list of control parameters passed to the optimize_fun.
#'  See ‘optimized_fun’ for more information.
#' @export
#' @inherit optimize_fun seealso
#' @examples
#' cat("==================Branin Function===========")
#' ex1 <- method_compare("branin", control = list(trueglobal = domain('branin')$opt$f))
#' ex1
#'
#'
#'
#'
method_compare <- function(fun,lower, upper, ..., p = NULL,rho = 0.3,
                           maximize = FALSE, reps = 20L, file = NULL,
                           control = list(), overwrite = FALSE){
  fun_name <- gsub("\\W", '', deparse1(substitute(fun)))
  time_file <- sub("\\.([^.]+$)", "_time.\\1", file)
  nsteps <- if(is.null(nsteps<-control$nsteps)) 5 else nsteps
  if(is.null(control$budget)) control$budget <- 50
  if(!is.null(file)){
    if(file.exists(file) && overwrite) {
      file.remove(c(file, time_file))
    }
    cat("\n",strrep("\n#", 80), "\n", sep="",file = time_file, append = TRUE)
    cat(strrep("#", 80),
        "\n# nsteps: ", nsteps,
        "\n# rho: ", rho,
        "\n# Lower Bound: (",toString(lower), ')',
        "\n# Upper Bound: (",toString(upper), ')',
        "\n# Budget: ", control$budget,"\n",
        file = file, append = TRUE, sep="")
  }
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
  res <- setNames(vector('list',3), c('RSO', 'EGO', 'TREGO'))
  errors_list <- res
  RScontrol <- modifyList(control, list(basicEGO = FALSE))
  EGcontrol <- modifyList(control, list(basicEGO = TRUE))

  TRcontrol <- modifyList(control, list(basicEGO = TRUE, method = 'TREGO'))
  time <- matrix(NA, nrow = reps, ncol = 3+length(control$expansion_rate),
                 dimnames = list(NULL,
                          unique(c(names(res),paste0('RSO', control$expansion_rate)))))
  #for(i in seq_len(reps))
   parallel::mclapply(seq_len(reps), \(i){

    X <- lhs::maximinLHS(5*length(lower), length(lower))
    X1 <- mapply(rescale, data.frame(X),data.frame(rbind(lower, upper)))
    cat("\n\nComputing f(x)...")
    y1 <- apply(X1, 1, function(x) (-1)^(maximize)*fun(x, ...))
    cat("Done\nRSO ITERATION:", i, "\n")
    t1 <- proc.time()[['elapsed']]
    res[['RSO']][[i]] <<- optimize_fun(fun, lower, upper,..., X = X1, y=y1,
                                      maximize = maximize,
                                      rho = rho,
                                      control = modifyList(RScontrol,
                                                  list(expansion_rate = 0)))
    time[i, 'RSO'] <<- proc.time()[['elapsed']] - t1
    if(!is.null(file)){
      cat("RSO", time[i, 'RSO'],"\n", file = time_file, append = TRUE)
      cat("RSO", i, res[['RSO']][[i]]$errors, "\n",file = file, append = TRUE)
    }
    cat("EGO ITERATION:", i, "\n")
    t2 <- proc.time()[['elapsed']]
    res[['EGO']][[i]] <<- optimize_fun(fun, lower, upper,..., X = X1, y=y1,
                                      maximize = maximize,
                                      rho = rho,
                                      control = EGcontrol)
    time[i, 'EGO'] <<- proc.time()[['elapsed']] - t2
    if(!is.null(file)){
      cat("EGO", time[i, 'EGO'],"\n", file = time_file, append = TRUE)
      cat("EGO", i, res[['EGO']][[i]]$errors, "\n",file = file, append = TRUE)
    }

    cat("TREGO ITERATION:", i, "\n")
    t3 <- proc.time()[['elapsed']]
    res[['TREGO']][[i]] <<- optimize_fun(fun, lower, upper,..., X = X1, y=y1,
                                        maximize = maximize,
                                        rho = rho,
                                        control = TRcontrol)
    time[i, 'TREGO'] <<- proc.time()[['elapsed']] - t3
    if(!is.null(file)){
      cat("TREGO", time[i, 'TREGO'], "\n",file = time_file, append = TRUE)
      cat("TREGO", i, res[['TREGO']][[i]]$errors, "\n",file = file, append = TRUE)
    }


    if(!is.null(control$expansion_rate) &&
       any(control$expansion_rate>0)){
      for(j in control$expansion_rate){
        nms <- paste0('RSO', j)
        cat(nms, "ITERATION: ", i, "\n", sep="")
        t4 <- proc.time()[['elapsed']]
        res[[nms]][[i]] <<- optimize_fun(fun,lower, upper,..., X = X1, y=y1,
                                        maximize = maximize, rho = rho,
                            control = modifyList(RScontrol,
                                                 list(expansion_rate = j)))
      time[i, nms] <<- proc.time()[['elapsed']] - t4
      if(!is.null(file)){
        cat(nms, time[i, nms], "\n",file = time_file, append = TRUE)
        cat(nms, i, res[[nms]][[i]]$errors,"\n", file = file, append = TRUE)
      }
      }
    }
  },mc.cores = if(.Platform$OS.type=='windows') 1 else parallel::detectCores())

  len <- (control$budget - if(is.null(n<-control$n)) 10 else n)/nsteps
  r <- parallel::mclapply(res, \(x){
    vals <- sapply(x, getElement, 'errors')
    vals <- if(maximize) 1- vals else log10(vals)
    cat("here\n\n")
    data.frame(t(apply(vals, 1, \(y)c(mean = mean(y), sd = sd(y)))))
    }, mc.cores = if(.Platform$OS.type=='windows') 1 else parallel::detectCores())

  d <- transform(array2DF(structure(r, dim = length(r))),
                 point = res$RSO[[1]]$env$ctr$nsteps *
                   seq(0, len))

  times <- colSums(time)
  structure(list(res=res, plot = plotComparison(d,
                          maximize = maximize,
                          nsteps = control$nsteps,
                          errorbars = !maximize),
       times = times[!is.na(times)]),
       class = "egoOptimList")
}

#' @export
print.egoOptimList <- function(x){
  cat("\tnsteps:", x[[c(1,1,1)]]$env$ctr$nsteps,
      "rho:", x[[c(1,1,1)]]$env$rho,
      "initial n:", x[[c(1,1,1)]]$env$X|>nrow(),
      "\n\n")
  cat("    Time Taken (Seconds) for ")
  cat(length(x$res[[1]]), "Replicates\n")
  cat("    EGO:",x$times['EGO'],
      "TREGO:",x$times['TREGO'],"RSO:",x$times['RSO'],"\n")

  print(x$plot)
  invisible(x)
}


rescale <- function(x, to){
  (x - min(x))/diff(range(x))*diff(to) + to[1]
}




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



#' A function to add budget on the evaluated object
#'
#'
#' @title add_budget
#' @param object @param object an egoOptim object or a list of egoOptim objects
#' @param new_budget Additional number of function evaluations
#' @param its Integer. Number of iterations to add
#' @export

add_budget <- function(object, its, new_budget){
  UseMethod('add_budget')
}

#' @export
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

#' @export
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


#' Used to compute the means and medians of the resulting egoOptim replications
#' @param object list of list of egoObjects
#' @param ... extra arguments such as the FUN to be evaluated.
#' @export
egoApply <- function(object, ...){
  UseMethod('egoApply')
}

#' @export
#' @method egoApply egoOptim
egoApply.egoOptim <- function(object, tol=1e-3){
  v <- object$errors < tol
  if(any(v)) which(v)[1] * object$env$ctr$nsteps
  else object$env$model@n
}

#' @export
#' @method egoApply egoOptim
egoApply.list <- function(object, FUN, tol=1e-3){
  sapply(object, \(x)FUN(sapply(x, egoApply, tol)))
}


# library(tidyverse)
# fn <- function(path){
#   {if(is.character(path))
#   read.table(path, comment.char = "=")
#   else path}%>%
#     rename(Var1=V1)%>%
#     pivot_longer(!(Var1:V2), names_to = 'point',values_drop_na =TRUE,
#                  names_transform = ~ 5* (parse_number(.)-3)) %>%
#     summarise(mean = mean(1-value), .by = c(Var1, point))|>
#     plotComparison(errorbars = FALSE)
# }
#
# fn("results/seeds.txt")
