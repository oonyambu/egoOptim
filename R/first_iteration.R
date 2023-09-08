# library(tidyverse)
#
#
#
# xx <- function(k){
#   f <- "branin"
#   fn <- getFromNamespace(f, "egoOptim")
#   dom <- egoOptim::domain(f)
#
#   #fn <- function(x)-mvtnorm::dmvnorm(x)
#   # dom$lower<-  c(-2,-2)
#   # dom$upper <- -dom$lower
#   # dom$opt <- list(x=c(0,0), f = fn(c(0,0)))
#
#   xy <- setNames(Map(seq, dom$lower, dom$upper, length=50), c("x", "y"))
#
#     #z <- outer(xy[,1], xy[,2], \(x,y)mapply(\(i,j) fn(c(i,j)), x, y))
#   X <-data.frame(lhs::maximinLHS(10, 2) ) %>%
#     set_names(c("x", "y"))%>%
#     mutate(x = scales::rescale(x, c(dom$lower[1], dom$upper[1])),
#            y = scales::rescale(y, c(dom$lower[2], dom$upper[2])),
#            z = apply(pick(x:y),1,fn))
#
#
#
#   mod <- DiceKriging::km(design = X[c("x", "y")], response = X$z, control = list(trace = 0))
#
#   mod1 <- DiceOptim::fastEGO.nsteps(mod, fn, 5, dom$lower, dom$upper)
#
#   x <- mod1$lastmodel@X
#   y <- mod1$lastmodel@y
#   o <- order(y)
#   pts <- x[o[1:k],]
#
#   dff <- apply(pts, 2, range)
#   d <- diff(dff)
#
#
#   dat1 <-  data.frame(xmin = dff[1,1], xmax=dff[2,1],
#                       ymin=dff[1,2], ymax=dff[2,2], z=1, x=1, y=1)
#   dat2 <- data.frame(pts[1,,drop = FALSE], height = d[2],
#                      width = d[1], z=1)
#   list2env(as.list(environment()), parent.frame())
# }
#
# w <- function(k){
#
#   xx(k)
#   cat(names(environment()))
#   p <- as_tibble(xy)  %>%
#     mutate(y = list(y)) %>%
#     unnest(y)%>%
#     mutate(z = map2_dbl(x,y,~fn(c(.x,.y))))%>%
#     ggplot(aes(x,y, z=z))+
#     geom_contour(bins = 100) +
#     geom_point(aes(x=x, y=y),
#                data = X, size= 2)
#
#   # Initial points
#   p
#
#
#   # added points
#   p <- p +
#     geom_point(aes(x,y), data = cbind(setNames(data.frame(mod1$par),
#                                          c("x", "y")), z=mod1$value), color=2,
#                size = 2)
#   p
#   # Best 3 points
#
#   for(i in 1:k){
#   p <- p +
#     geom_point(aes(x,y, alpha=0.5), data = cbind(setNames(data.frame(pts[i,,drop =FALSE]),
#                                                c("x", "y")), z=1),
#                shape = "\u2605", fill="red", size=2*(k-i+1)+1, color="green")
#   }
#   list2env(as.list(environment()), parent.frame())
#   p
# }
#
#
#
# # p <- p +
# #   geom_point(aes(x,y), data = cbind(setNames(data.frame(pts[2,, drop = FALSE]),
# #                                              c("x", "y")), z=1),
# #              shape = "\u2605", fill="red", size=5, color="red")
# # p <- p +
# #   geom_point(aes(x,y), data = cbind(setNames(data.frame(pts[3, , drop = FALSE]),
# #                                              c("x", "y")), z=1),
# #              shape = "\u2605", fill="red", size=4, color="red")
# #
# #
# p <- w(5)
#
# p <- p +   labs(y=NULL, x=NULL)+
#   guides(x='none', y='none')+
#   theme(legend.position = 'none')+
#   coord_fixed();p
#
# # ROI
# r <- p +
#   geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#             data = dat1, alpha =0, color=2, linewidth=1.1);r
#
# #Shifted ROI
# s <- r +
#   geom_tile(aes(x, y, height = height, width = width),
#             data =dat2 , alpha = 0, color="green", linewidth=1.1);s
#
# dat3 <- dat2%>%
#   mutate(xmin = x-width/2, ymax = y+height/2,
#          yc = (dat1$ymax-dat1$ymin)/2, x_start = dat1$xmin)%>%
#   reframe(x = x_start + 0.2,
#           xend = xmin-0.2, yend = c(ymax, y, y-height/2)+0.2
#           , y = c(dat1$ymax, yc, dat1$ymin) - 0.2,
#           z=1)%>%
#   summarise(x=x[2], y=y[2], xend=xend[2], yend=y, z=1)
#
#
# r <- s + geom_segment(aes(x = x,   y = y, xend = xend, yend = yend),
#                  arrow = arrow(length = unit(0.5, "cm")),
#                  data=dat3, linewidth=1.1)+
#   labs(y=NULL, x=NULL)+
#   #guides(x='none', y='none')+
#   theme(legend.position = 'none')+
#   coord_fixed();r
#
# lower <- unname(c(pmax(pts[1,] - d/2, dom$lower)))
# upper <- c(unname(pmin(pts[1,] + d/2, dom$upper)))
#
# dat3 <-  data.frame(xmin = lower[1], xmax=upper[1],
#                     ymin=lower[2], ymax=upper[2], z=1, x=1, y=1)
#
# # Contrained within search space ROI
# t<-r +
#   geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#             data = dat3, alpha =0, color="black", linewidth=1.01,
#             linetype=2);t
#
# p +
#   geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#             data = dat3, alpha =0, color="black", linewidth=1.01,
#             linetype=2)
#
#
#
#
#
#
#
#
#
#
