hart3_res <- method_compare("hart3",  budget = 100, file = "data/hart3_3.csv")
hart4_res <- method_compare("hart4",  budget = 100, file = "data/hart4_3.csv")
hart6_res <- method_compare("hart6",  budget = 100, file = "data/hart6_3.csv")







shekel10 <- method_compare("shekel", m = 10, budget = 100,
                  control = list(truglobal = domain('shekel')$opt$f[3]),
                  file = "data/shekel10")