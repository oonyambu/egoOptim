
# D:\Programs\R\R-4.3.1\bin\x64\Rscript.exe

library(egoOptim)

ct <- list(budget = 100)


# hart3_res <- method_compare("hart3", file = "results/hart3_5.txt", control = ct)
# hart4_res <- method_compare("hart4", file = "results/hart4_5.txt", control = ct)
# hart6_res <- method_compare("hart6", file = "results/hart6_3_5.txt", control = ct)
#
# camel3_res <- method_compare("camel3",   file = "results/camel3_5.txt", control = ct)
# camel6_res <- method_compare("camel6",   file = "results/camel6_5.txt", control = ct)
#
#
# beale_res <- method_compare("beale", file = "results/beale_5.txt", control = ct)
# easom_res <- method_compare("easom",  file = "results/easom_5.txt", control = ct)

branin_res <- method_compare("branin", file = "results/branin_5.txt", control = ct)
goldpr_res <- method_compare("goldpr", file = "results/goldpr_5.txt", control = ct)
goldpr_res <- method_compare("goldprsc", file = "results/goldprsc_5.txt", control = ct)
beale_res <- method_compare("beale", file = "results/beale_5.txt", control = ct)




library(tidyverse)

read.table("results/hart4_5.txt")%>%
  summarise(across(-V2, list(mean=~mean(log10(.)),sd=~sd(log10(.)))),.by=V1)%>%

  pivot_longer(-V1,names_to = c("point", ".value"),
               names_sep = "_",
      names_transform = list(point=~parse_number(.)%>%{5*(.-min(.))}))|>
  rename(Var1=V1)|>
  egoOptim::plotComparison(errorbars = F, n=70)+
  ggplot2::scale_x_continuous(labels=~.+10)

pivot_longer(read.table("results/hart4_5.txt"), !V1:V2,
             values_transform = ~.*NA^(log10(.)< -3),
             names_transform = ~parse_number(.)%>%{5*(.-min(.))+10},
             values_drop_na = TRUE)|>
  summarise(n=max(name), .by=c(V1,V2))%>%
  summarise(mean(n), .by=V1)



library(egoOptim)
wine
