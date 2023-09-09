## code to prepare `DATASET` dataset goes here


## Tukish Music Emotion Data Set
my_data <- function(){
  accoustic <- read.csv("D:/Work/Ego/data/data use/Acoustic Features.csv")
  head(accoustic)
  accoustic$Class <- factor(accoustic$Class)
  accoustic <- as_tibble(model.frame(Class~.,accoustic))
  # save(accoustic, file = 'data/accoustic.rda')
  # rm(accoustic)
  # load('data/accoustic.rda')
  # accoustic
  # rm(accoustic)

  ## Maternal Health Data Set
  maternalHealth <- read.csv("D:/Work/Ego/data/data use/Maternal Health Risk Data Set.csv")
  head(maternalHealth)
  maternalHealth$RiskLevel <- factor(maternalHealth$RiskLevel)
  maternalHealth <- as_tibble(model.frame(RiskLevel~.,maternalHealth))
  # save(maternalHealth, file = 'data/maternalHealth.rda')
  # rm(maternalHealth)
  # load('data/maternalHealth.rda')
  # maternalHealth
  # rm(maternalHealth)

  ## Raisin Data set
  raisin<- readxl::read_excel("D:/Work/Ego/data/data use/Raisin_Dataset/Raisin_Dataset/Raisin_Dataset.xlsx")
  head(raisin)
  raisin$Class <- factor(raisin$Class)
  raisin <- as_tibble(model.frame(Class~.,raisin))
  # save(raisin, file = 'data/raisin.rda')
  # rm(raisin)
  # load('data/raisin.rda')
  # raisin
  # rm(raisin)

  ## Seeds Data set
  seeds <- read.table("D:/Work/Ego/data/data use/seeds/seeds_dataset.txt")
  names(seeds) <- c("area", "perimeter", "compactness", "kernel_length",
                    "kernel_width", "asymmetry_coefficient", "kernel_groove_length",
                    "class")
  head(seeds)
  seeds$class <- factor(seeds$class)
  seeds <- as_tibble(model.frame(class~.,seeds))
  # save(seeds, file = 'data/seeds.rda')
  # rm(seeds)
  # load('data/seeds.rda')
  # seeds
  # rm(seeds)
  #
  ## Breast Tissue Data set
  breastTissue<- readxl::read_excel("D:/Work/Ego/data/data use/breast+tissue/BreastTissue.xls",
                                    sheet = 'Data')[-1]
  head(breastTissue)
  breastTissue$Class <- factor(breastTissue$Class)
  breastTissue <- as_tibble(model.frame(Class~.,breastTissue))
  # save(breastTissue, file = 'data/breastTissue.rda')
  # rm(breastTissue)
  # load('data/breastTissue.rda')
  # breastTissue
  # rm(breastTissue)

  ## glass identification Data set
  glass<- read.csv("D:/Work/Ego/data/data use/glass+identification/glass.data",
                   header = FALSE, row.names = 1)
  names(glass) <- c("refractive_index", "sodium_oxide", "magnesium_oxide", "aluminum_oxide",
                    "silcon_oxide", "potassium_oxide", "calcium_oxide", "barium_oxide",
                    "iron_oxide", 'type')
  head(glass)
  glass$type <- factor(glass$type)
  glass <- as_tibble(model.frame(type~.,glass))
  save(glass, file = 'data/glass.rda')
  rm(glass)
  load('data/glass.rda')
  glass
  rm(glass)
  ## Liver Disorders Data set
  liverDisorders <- read.csv("D:/Work/Ego/data/data use/liver+disorders/bupa.data",
                             header = FALSE)
  names(liverDisorders) <- c("mcv", "alkphos", "sgpt", "sgot", "gammagt", "drinks",
                             "selector")
  head(liverDisorders)
  liverDisorders$selector <- factor(liverDisorders$selector)
  liverDisorders <- as_tibble(model.frame(selector~.,liverDisorders))
  # save(liverDisorders, file = 'data/liverDisorders.rda')
  # rm(liverDisorders)
  # load("data/liverDisorders.rda")
  # liverDisorders
  # rm(liverDisorders)

  ## parkinsons Data Set
  parkinsons <- read.csv("D:/Work/Ego/data/data use/parkinsons/parkinsons.data",
                         header = FALSE)[-1]
  names(parkinsons) <- parkinsons[1,]
  parkinsons <- type.convert(parkinsons[-1,], as.is = TRUE)
  head(parkinsons)
  parkinsons$status <- factor(parkinsons$status)
  parkinsons <- as_tibble(model.frame(status~.,parkinsons))
  # save(parkinsons, file = 'data/parkinsons.rda')
  # rm(parkinsons)
  # load("data/parkinsons.rda")
  # parkinsons
  # rm(parkinsons)

  ## Vertebral data

  vertebral2 <- foreign::read.arff('D:/Work/Ego/data/data use/vertebral+column/column_2C_weka.arff')
  vertebral2 <- as_tibble(model.frame(class~.,vertebral2))
  vertebral3 <- foreign::read.arff('D:/Work/Ego/data/data use/vertebral+column/column_3C_weka.arff')
  vertebral3 <- as_tibble(model.frame(class~.,vertebral3))
  # save(vertebral2,vertebral3, file = 'data/vertebral.rda')
  # rm(vertebral2,vertebral3)
  # load("data/vertebral.rda")
  # vertebral2;vertebral3
  # rm(vertebral2,vertebral3)


  ## wine Data
  wine <- read.csv("D:/Work/Ego/data/data use/wine/wine.data", header = FALSE)
  names(wine) <- c("Class", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium",
                   "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins",
                   "Color intensity", "Hue", "OD280/OD315 of diluted wines",
                   "Proline")
  head(wine)
  wine$Class <- factor(wine$Class)
  wine <- as_tibble(model.frame(Class~.,wine))
  # save(wine, file = 'data/wine.rda')
  # rm(wine)
  # load("data/wine.rda")
  # wine
  # rm(wine)
}


usethis::use_data(DATASET, overwrite = TRUE)
