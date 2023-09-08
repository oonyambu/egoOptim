hart3_res <- method_compare("hart3",  budget = 100, file = "data/hart3_3.csv")
hart4_res <- method_compare("hart4",  budget = 100, file = "data/hart4_3.csv")
hart6_res <- method_compare("hart6",  budget = 100, file = "data/hart6_3.csv")

camel3_res <- method_compare("camel3",  budget = 100, file = "data/camel3_3.csv")
camel6_res <- method_compare("camel6",  budget = 100, file = "data/camel6_3.csv")


beale_res <- method_compare("beale",  budget = 100, file = "data/beale_3.csv")
easom_res <- method_compare("easom",  budget = 100, file = "data/easom_3.csv")

branin_res <- method_compare("branin",  budget = 100, file = "data/branin_3.csv")
goldpr_res <- method_compare("goldpr",  budget = 100, file = "data/goldpr_3.csv")




shekel10_res <- method_compare("shekel", m = 10, budget = 100,
                  control = list(trueglobal = domain('shekel')$opt$f[3]),
                  file = "data/shekel10_3.csv")
shekel7_res <- method_compare("shekel", m = 7, budget = 100,
													 control = list(trueglobal = domain('shekel')$opt$f[2]),
													 file = "data/shekel7_3.csv")

shekel5_res <- method_compare("shekel", m = 1, budget = 100,
													 control = list(trueglobal = domain('shekel')$opt$f[1]),
													 file = "data/shekel5_3.csv")


