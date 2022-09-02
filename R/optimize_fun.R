#' Optimize fun
#'
#' Uses EGO algotiyhm to optimize any given function.
#'
#' @author BLANK
#'
#' @export
optimize_fun <- function(FUN, lower, upper,
                         n = 5, nsteps = 2, top = .15,
                         ntimes = 5){
  #Initial Domain
  init_lower <- lower
  init_upper <- upper

  # Design points
  p <- length(lower)
  design <- lhs::randomLHS(n, p)

  # Rescale the design to the entire domain
  design <- mapply(scales::rescale, unname(data.frame(design)),
                   to = rbind.data.frame(lower, upper), from = list(c(0, 1)))
  y <- apply(design, 1, FUN)


  # Plot design points
  if(p == 2){
    grid_x <- seq(lower[1], upper[1], length = 200)
    grid_y <- seq(lower[2], upper[2], length = 200)
    z <- matrix(apply(expand.grid(grid_x, grid_y), 1, FUN), 200)

    contour(grid_x, grid_y, z)
    points(cbind(design, y), col=4, pch=20)
  }

  prev_points <- cbind(design, y)
  model1 <- DiceKriging::km(design = design,
                            response = y, control = list(trace=0))
  # REPEAT:
  for(i in seq(ntimes)){

    oego <- DiceOptim::TREGO.nsteps(model1, FUN, nsteps = nsteps,
                                    lower=lower, upper = upper, trace = -1)
    model1 <- oego$lastmodel

    # New Points from the TREGO model
    new_x <- unname(oego$par)
    new_y <- apply(new_x, 1, FUN)

    # all points obtained so far:
    prev_points <- rbind(prev_points, cbind(new_x, new_y))

    ## Obtain the top_n % of  new points + previous_points

    # Number of rows/points to choose - choose at least p+2 points.
    n_best <- max(ceiling(top * nrow(prev_points)), p+2)

    best <- prev_points[head(order(prev_points[,p+1]), n_best), ]

    # Determine minimum and maximum x, y : ROI
    lower <- apply(best[,-p-1], 2, min)
    upper <- apply(best[,-p-1], 2, max)

    # Center the ROI at the best position
    dist <- (upper - lower)/2
    center <- best[1,-p-1]
    lower <- pmax(center - dist, init_lower)
    upper <- pmin(center + dist, init_upper)

    # Include the rectangle to visualize the ROI
    if(p==2) {
      do.call(rect, as.list(c(lower, upper)))
      ## Plot new points
      points(cbind(new_x, new_y), col=2, pch=17)
      Sys.sleep(1)
    }
  }
  list(all_points = prev_points, best = best[1,])
}

