

#' @export

method_compare <- function(fun,low, up, ..., budget = 50, p = NULL,
                           maximize = FALSE, reps = 20L, nsteps = 5, plt = FALSE,
                           increament_rate= 0, file = NULL){
  fun_name <- gsub("\\W", '', deparse1(substitute(fun)))

  library(ggplot2)
  optimal <- NULL
  if(missing(low)){
    dom <- domain(fun)
    fun <- getFromNamespace(fun, 'egoOptim')
    if(is.function(dom)){
      if(is.null(p)) stop('the dimension `p` must be provided for ', f)
      else dom <- dom(p)
    }
    optimal <- dom$opt$f
    if(is.matrix(optimal)) optimal <- optimal[1,]
    low <- if(!is.null(dom$low))dom$low else rep(0, p)
    up <- if(!is.null(dom$up))dom$up else rep(1, p)
  }
  if(maximize & is.null(optimal)) optimal <- -1
  res <- setNames(vector('list', 3), c('RSO', 'EGO', 'TREGO'))
  for(i in seq_len(reps)){
    X <- lhs::maximinLHS(5*length(low), length(low))
    X1 <- mapply(scales::rescale, data.frame(X),data.frame(rbind(low, up)))
    y1 <- apply(X1, 1, function(x) (-1)^(maximize)*fun(x, ...))
    cat("RSO ITERATION:", i, "\n")
    res[['RSO']][[i]] <- optimize_fun(fun, low, up,...,
                                      increament_rate = 0,
                                      X = X1,y=y1,
                                      budget = budget,
                                      maximize = maximize,
                                      trueglobal = optimal,
                                      nsteps = nsteps,
                                      do_maxit = TRUE,basicEGO = FALSE)
    cat("EGO ITERATION:", i, "\n")
    res[['EGO']][[i]] <- optimize_fun(fun, low, up,..., X = X1, y=y1,
                                      budget = budget,
                                      maximize = maximize,
                                      nsteps = nsteps,
                                      trueglobal = optimal,
                                      do_maxit = TRUE,basicEGO = TRUE)
    cat("TREGO ITERATION:", i, "\n")
    res[['TREGO']][[i]] <- optimize_fun(fun, low, up,...,X = X1,y=y1,
                                        trueglobal = optimal,
                                        maximize = maximize,
                                        budget = budget,
                                        nsteps = nsteps,
                                        do_maxit = TRUE,basicEGO = TRUE,
                                        method = 'TREGO')
    if(increament_rate>0){
      cat("RSO",increament_rate, "ITERATION: ", i, "\n", sep="")
      res[['TREGO']][[i]] <- optimize_fun(fun, low, up,...,X = X1,y=y1,
                                          increament_rate = increament_rate,
                                          trueglobal = optimal,
                                          maximize = maximize,
                                          budget = budget,
                                          nsteps = nsteps,
                                          do_maxit = TRUE,basicEGO = TRUE,
                                          method = 'TREGO')
    }
  }
  r <- lapply(res, \(x){
    vals <- sapply(x, getElement, 'errors')
    vals <- if(maximize) 1- vals else log10(vals)
    y <- data.frame(t(apply(vals, 1,
                            \(y)c(mean = mean(y), sd = sd(y)))))
    y})
  d <- transform(array2DF(structure(r, dim = 3)), point = 5*seq(0,nrow(r[[1]])-1))
  assign(fun_name, d)
  if(!is.null(file)) write.csv(d, file = file)
  list(res=res, plot = plotComparison(d))
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

#' @export

plotComparison <- function(res,
                n = NULL, maximize = FALSE, m=0, nsteps = 5){
  if(!is.data.frame(res)){
    r <- lapply(res, \(x){
      vals <- sapply(x, getElement, 'errors')
      vals <- if(maximize) 1- vals else log10(vals)
      y <- data.frame(t(apply(vals, 1,
                              \(y)c(mean = mean(y), sd = sd(y)))))
      y})
    d <- transform(array2DF(structure(r, dim = 3)), point = 5*seq(0,nrow(r[[1]])-1))
  }
  else d <- res
  if(is.null(n)) n <- max(d$point)
  d <- subset(d, point>=m & point<=n)
  ggplot(d, aes(x = point, mean, color = Var1))+
    geom_point() +
    geom_line(linewidth = 1) +
    geom_errorbar(aes(ymax = (if(maximize)pmin(mean + sd,1) else mean+sd),
                      ymin =(if(maximize) pmax(mean - sd, 0) else mean - sd)),
                  linewidth = 1, width = 1, alpha=0.5) +
    labs(color = 'Method') +
    ylab(if(maximize)'accuracy' else bquote(Log[10]* ' Loss')) +
    xlab('Total points Used') +
    scale_x_continuous(labels=~.x+10, breaks = ~seq(0, .x[2],nsteps))
}




#' @export
#'
my_contour <- function(x1, x2, Z, xlab = NULL,
                       ylab = NULL, title = NULL,
                       background = "white", title.adjust = 0.5,
                       subtitle = "it1",nlevels=100, col='blue', ...){
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

#' @export

add_budget <- function(object, ...){
  UseMethod('add_budget')
}

add_budget.egoOptim <- function(object, its, new_budget){
  list2env(mget(names(object$env), object$env), environment())
  if(!missing(its)) maxit <- its
  if(!missing(new_budget)) {
    maxit <- new_budget %/% nsteps
    do_maxit <- TRUE
  }
  new_err <- errors
  errors <- 0
  v <- rev(body(optimize_fun))[1:2]
  eval(v[[2]])
  results <- eval(v[[1]])
  results$errors <- c(error_init, new_err, errors)
  results
}

add_budget.list <- function(object, its, new_budget, inplace = FALSE){
  nms <-  deparse1(substitute(object))
  for(i in names(object$res)){
    for(j in seq_along(object$res[[i]])){
      cat("updating", i, "it:", j, "\n")
      object$res[[i]][[j]] <- add_budget(object$res[[i]][[j]],
                                         its = its, new_budget = new_budget)
    }
  }
  object$plot <- plotComparison(object$res)
  if(inplace)
    setNames(list(object), nms)|>
    list2env(parent.frame())|>
    invisible()
  else object
}

#' @export
add_replicates <- function(call, reps){

  a <- call$call$reps
  call$call$reps <- reps
  res <- Map(c, call$res, eval(call$call)$res)
  call$call$reps <- reps + a
  v <- mget(c('maximize','nsteps'), call$res[[1]][[1]]$env)
  list(res = res,
       plot = plotComparison(res, maximize = v$maximize,
                             nsteps = v$nsteps,
                             errorbars = !v$maximize),
       call=call$call)
}

