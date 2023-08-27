#' Domain
#'A function to output the domain of the various functions in the
#' egoOptim package
#' @usage domain(fun_name = NULL)
#' @param fun_name An optional character of length 1. The name of the function
#'   of which we desire to obtain the domain of  the dimensions.
#' @return A list with the domain and the global minimum(s):
#' \item{lower}{A vector containing the lower bounds}
#' \item{upper}{A vector containing the upper bounds}
#' \item{opt}{A list containing the global minimum \bold{x*}
#' and the function value \bold{f(x*)}}
#' @examples
#' domain()
#' domain("camel3")
#' domain()$camel3
#' @export


domain <- function(fun_name = NULL){
  dm <- list(
  #Many local Minima
  ackley = function(d)list(lower = rep(-32.768, d), upper = rep(32.768, d),
                           opt = list(x=numeric(d), f = 0)),
  bukin6 = list(lower = c(-15, -3), upper = c(-5, 3),
                opt = list(x = c(-10, 1), f=0)),
  crossit = list(lower = c(-10, -10), upper = c(10,10),
                 opt = list(x = rbind(c(1.3491,-1.3491), c(1.3491, 1.3491),
                                      c(-1.3491,1.3491), c(-1.3491,-1.3491)),
                            f = -2.06261)),
  drop = list(lower = c(-5.12, -5.12), upper = c(5.12, 5.12),
              opt = list(x = c(0,0), f = -1)),
  egg = list(lower = c(-512, -512), upper = c(512, 512),
             opt = list(x = c(512,404.2319), f = -959.6407)),
  grlee12 = list(lower = 0.5, upper = 2.5),
  griewank = function(d) list(lower = rep(-600, d), upper = rep(600,d),
                              opt = list(x = numeric(d), f = 0)),
  holder = list(lower = c(-10, -10), upper = c(10,10),
                opt = list(x = rbind(c(8.05502,9.66459),
                                     c(8.05502,-9.66459),
                                     c(-8.05502,9.66459),
                                     c(-8.05502,-9.66459)),
                           f = -19.2085)),
  langer = function(d) list(lower = rep(0, d), upper = rep(10, d)),
  levy = function(d) list(lower = rep(-10, d), upper = rep(10,d),
                          opt = list(x = rep(1, d), f = 0)),
  levy13 = list(lower = c(-10, -10), upper = c(10,10),
                opt = list(x = rep(1,2), f = 0)),
  rastr = function(d) list(lower = rep(-5.12, d), upper = rep(5.12, d),
                           opt = list(x = rep(0,d), f = 0)),
  schaffer2 = list(lower = c(-100, -100), upper = c(100, 100),
                   opt = list(x = rep(0,2), f = 0)),
  schaffer4 = list(lower = c(-100, -100), upper = c(100, 100)),
  schwef = function(d) list(lower = rep(-500, d), upper = rep(500,d),
                            opt = list(x = rep(420.9687,d), f = 0)),
  shubert = list(lower = c(-5.12, -5.12), upper = c(5.12, 5.12),
                  opt = list(x = c(-0.800322, 4.858051),
                            f =  -186.7309088)),

  # Bowl-Shaped
  boha1 = list(lower = c(-100, -100), upper = c(100, 100),
               opt = list(x = rep(0,2), f = 0)),
  boha2 = list(lower = c(-100, -100), upper = c(100, 100)),
  boha3 = list(lower = c(-100, -100), upper = c(100, 100)),
  perm0db = function(d) list(lower = rep(-d, d), upper = rep(d, d),
                             opt = list(x = 1/seq(d), f = 0)),
  rothyp = function(d) list(lower = rep(-65.536, d), upper = rep(65.536, d),
                            opt = list(x = rep(0,d), f = 0)),
  spheref = function(d) list(lower = rep(-5.12, d), upper = rep(5.12, d),
                             opt = list(x = rep(0,d), f = 0)),
  spherefmod = list(lower = rep(0, 6), upper = rep(1, 6)),
  sumpow = function(d) list(lower = rep(-1, d), upper = rep(1, d),
                            opt = list(x = rep(0,d), f = 0)),
  sumsqu = function(d) list(lower = rep(-5.12, d), upper = rep(5.12, d),
                            opt = list(x = rep(0,d), f = 0)),
  trid = function(d) list(lower = rep(-d^2, d), upper = rep(d^2, d),
                          opt = list(x = seq(d)*(d+1-seq(d)), f=-d*(d+4)*(d-1)/6)),

  #Plate - Shaped
  booth = list(lower = c(-10, -10), upper = c(10, 10),
               opt = list(x = c(1, 3), f=0)),
  matya = list(lower = c(-10, -10), upper = c(10, 10),
               opt = list(x = c(0, 0), f = 0)),
  mccorm = list(lower = c(-1.5, -3), upper = c(4, 4),
                opt = list(x=c(-0.54719, -1.54719), f=-1.9133)),
  powersum = function(d)list(lower = rep(0, d), upper = rep(d,d)),
  zakharov = function(d)list(lower = rep(-5, d), upper = rep(10,d),
                             opt = list(x = rep(0, d), f = 0)),

  #Valley-Shape
  camel3 = list(lower = c(-5,-5), upper = c(5,5),
                opt = list(x = c(0,0), f = 0)),
  camel6 = list(lower= c(-3,-2), upper = c(3,2),
                opt = list(x = rbind(c(0.0898, -0.7126),
                                     c(-0.0898, 0.7126)),
                           f = -1.031628)),
  dixonpr = function(d)list(lower = rep(-10, d), upper = rep(10,d),
                            opt = list(x = 2^(-((2^seq(d) - 2)/(2^seq(d)))),
                                       f = 0)),
  rosen = function(d) list(lower = rep(-2.048, d), upper = rep(2.048, d),
                           opt = list(x = rep(1, d), f = 0)),

  #Steep Ridges/Drops
  dejong5 = list(lower = rep(-65.536,2), upper = rep(65.536, 2)),
  easom = list(lower = c(-100, -100), upper = c(100, 100),
               opt = list(x = c(pi, pi), f = -1)),
  michal = function(d)list(lower = numeric(d), upper = rep(pi, d),
                           opt = switch(d,`2` = list(x=c(2.2, 1.57),f=-1.8013), `5` = c(f=-4.687658),
                                        `10`=c(f=-9.66015))),

  #Other
  beale = list(lower = rep(-4.5,2), upper = rep(4.5, 2),
               opt = list(x = c(3, 0.5), f = 0)),
  branin = list(lower = c(-5, 0), upper= c(10, 15),
                opt = list(x = rbind(c(-pi, 12.275), c(pi, 2.275),
                            c(9.42478, 2.475)), f = 0.397887)),

  colville = list(lower = rep(-10, 4), upper = rep(10, 4),
                  opt = list(x = rep(1,4), f = 0)),
  forretal08 = list(loser = 0, upper = 1),
  goldpr = list(lower = c(-2,-2), upper = c(2,2),
                opt = list(x = c(0, -1), f = 3)),
  hart3 = list(lower = numeric(3), upper =rep(1, 3),
               opt = list(x = c(0.114614, 0.555649, 0.852547),
                          f = -3.86278)),
  hart4 = list(lower = numeric(4), upper =rep(1, 4)),
  hart6 = list(lower = numeric(6), upper =rep(1, 6),
               opt = list(x = c(0.20169, 0.150011,0.476874,
                                0.275332,0.311652,0.6573),
                          f = -3.32237)),
  permdb = function(d)list(lower = rep(-d,d), upper = rep(d,d),
                           opt = list(x = seq(d), f = 0)),
  powell = function(d)list(lower = rep(-4, d), upper = rep(5, d),
                           opt = list(x=rep(0, d), f = 0)),
  shekel = list(lower = rep(0, 4), upper = rep(10, 4),
                opt = list(x = rep(4, 4),
                           f=setNames(c(-10.1532, -10.4029, -10.5364),
                                      paste0("m=", c(5,7,10))))),
  stybtang = function(d) list(lower = rep(-5, d), upper = rep(5, d),
                              opt = list(x = rep(-2.903534, d),
                                         f = -39.16599*d)))
  if(is.null(fun_name))dm
  else dm[[fun_name]]
}
