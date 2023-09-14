#
 library(egoOptim)
#
# # FUNCTIONS
#
cat("==================Branin Function===========")
ex1 <- method_compare("branin", control = list(trueglobal = domain('branin')$opt$f))


cat("==================Gold Price Function===========")
ex2 <- method_compare("goldprsc", control = list(trueglobal = domain('goldprsc')$opt$f))

cat("==================Hartman3 Function===========")
ex3 <- method_compare("hart3", control = list(trueglobal = domain('hart3')$opt$f))

cat("==================Hartman4 Function===========")
ex4 <- method_compare("hart4", control = list(trueglobal = domain('hart4')$opt$f))



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

cat("=========GLASS DATASET==========\n")
glass1 <-cbind(glass[1], scale(glass[-1]))
method_compare(svm_train, low, up, train=glass1,reps = reps,
               maximize = TRUE,  file = 'results/glass.txt',
               control = list(trueglobal=1, budget=budget),overwrite = FALSE)




cat("=========BREAST CANCER DATASET==========\n")
breastTissue1 <-cbind(breastTissue[1], scale(breastTissue[-1]))
method_compare(svm_train, low, up, train=breastTissue1 ,reps = reps,
               maximize = TRUE,  file = 'results/breastTissue.txt',
               control = list(trueglobal=1, budget=budget),overwrite = FALSE)


cat("=========BREAST SEEDS DATASET==========\n")
seeds1 <-cbind(seeds[1], scale(seeds[-1]))
method_compare(svm_train, low, up, train=seeds1 ,reps = reps,
               maximize = TRUE,  file = 'results/seeds.txt',
               control = list(trueglobal=1, budget=budget),overwrite = FALSE)

cat("=========RAISIN DATASET==========\n")
raisin1 <- cbind(raisin[1], scale(raisin[-1]))

method_compare(svm_train, low, up, train=raisin1,reps = reps,
               maximize = TRUE,  file = 'results/raisin.txt',
               control = list(trueglobal=1, budget=budget))


cat("=========TURKISH MUSIC DATASET==========\n")
accoustic1 <- cbind(accoustic[1], scale(accoustic[-1]))

method_compare(svm_train, low, up, train=accoustic1,reps = reps,
               maximize = TRUE,  file = 'results/accoustic.txt',
               control = list(trueglobal=1, budget=budget),overwrite = FALSE)


cat("=========MATERNAL HEALTH DATASET==========\n")
maternalHealth1 <- cbind(maternalHealth[1], scale(maternalHealth[-1]))

method_compare(svm_train, low, up, train=maternalHealth1,reps = reps,
                maximize = TRUE,  file = 'results/maternal_health.txt',
                control = list(trueglobal=1),overwrite = FALSE)

cat("=========ESLMIXTURE DATASET==========\n")
ESLmixture1 <- with(ESLmixture, data.frame(y = factor(y), scale(x)))
method_compare(svm_train, low, up, train=ESLmixture1,reps = reps,
               maximize = TRUE,  file = 'results/esl_mixture.txt',
               control = list(trueglobal=1,budget=budget),overwrite = FALSE)



cat("=========TRAIN3 DATASET==========\n")
vec_trainX <- scale(vehicle_train[-1])
vec_testX <- do.call(scale, c(list(vehicle_test[-1]),
                              unname(tail(attributes(vec_trainX), 2))))
train3 <- cbind(vehicle_train[1], vec_trainX)
test3 <- cbind(vehicle_test[1], vec_testX)

method_compare(svm_train, low, up, train=train3, cv=FALSE, reps = 2,
               maximize = TRUE,  file = 'results/vehicle_train3.txt',
               control = list(trueglobal=1,
                              cost = \(x)egoOptim:::svm_accuracy(x, train3, test3)))


cat("=========TRAIN1 DATASET==========\n")
trainX <- scale(astroparticle_train[-1])
testX <- do.call(scale, c(list(astroparticle_test[-1]),
                          unname(tail(attributes(trainX), 2))))
train1 <- cbind(astroparticle_train[1], trainX)
test1 <- cbind(astroparticle_test[1], testX)

method_compare(svm_train, low, up, train=train1,reps = reps,
               maximize = TRUE,  file = 'results/astroparticle_train1.txt',
               control = list(trueglobal=1,
                              cost = \(x)egoOptim:::svm_accuracy(x, train1, test1)))


