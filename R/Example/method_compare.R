
if(!require('egoOptim')){
    if(!require('remotes'))
	install.packages(c('remotes', 'devtools'))
    remotes::install_github('oonyambu/egoOptim')
}
library(egoOptim)

# # FUNCTIONS
#

cat("==================Branin Function===========")
ex1 <- method_compare("branin", control = list(trueglobal = domain('branin')$opt$f))
ex1

ex2 <- method_compare('branin', control = list(trueglobal = domain('branin')$opt$f,
                                        kmcontrol = list(pop.size = 100, multistart = 10)))

ex2
cat("==================Gold Price Function===========")
ex1 <- method_compare("goldprsc", control = list(trueglobal = domain('goldprsc')$opt$f))

cat("==================Hartman3 Function===========")
ex1 <- method_compare("hart3", control = list(trueglobal = domain('hart3')$opt$f))

cat("==================Hartman4 Function===========")
ex1 <- method_compare("hart4", control = list(trueglobal = domain('hart4')$opt$f))



# ### SVM DATASETS:

low <- c(2^-16, 10^-2)
up <- c(2^3, 10^2)
reps <- 20
budget = 100


cat("=========WINE DATASET==========\n")
wine1 <- cbind(wine[1], scale(wine[-1]))
method_compare(svm_train, low, up, train=wine1,reps = reps,
               maximize = TRUE,  file = 'results/wine.txt',
               control = list(trueglobal=1, budget=budget),overwrite = FALSE)
