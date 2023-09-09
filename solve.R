library(egoOptim)

ct <- list(budget = 100)


hart3_res <- method_compare("hart3", file = "results/hart3_5.txt", control = ct)
hart4_res <- method_compare("hart4", file = "results/hart4_5.txt", control = ct)
hart6_res <- method_compare("hart6", file = "results/hart6_3_5.txt", control = ct)

camel3_res <- method_compare("camel3",   file = "results/camel3_5.txt", control = ct)
camel6_res <- method_compare("camel6",   file = "results/camel6_5.txt", control = ct)


beale_res <- method_compare("beale", file = "results/beale_5.txt", control = ct)
easom_res <- method_compare("easom",  file = "results/easom_5.txt", control = ct)

branin_res <- method_compare("branin", file = "results/branin_5.txt", control = ct)
goldpr_res <- method_compare("goldpr", file = "results/goldpr_5.txt", control = ct)
goldpr_res <- method_compare("goldprsc", file = "results/goldprsc_5.txt", control = ct)
beale_res <- method_compare("beale", file = "results/beale_5.txt", control = ct)
