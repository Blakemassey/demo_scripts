library(xtable)
## Load example dataset
data(tli)

## Demonstrate data.frame

xtable1 <- xtable(tli[1:20, ], only.contents=TRUE, floating=F,
  caption = "My caption\\label{tab:MyTable1}" ,label="tab:MyTable1")

print(xtable1, floating= F, file = "Results/Tables/Xtable_example.tex")

glm_example <- glm(disadvg ~ ethnicty*grade, data = tli, family = binomial())
xtable2 <- xtable(fm3)
print(xtable2, floating= F, file = "Results/Tables/Xtable_example2.tex")


## Demonstrate glm
## Taken from help(glm) in R 1.1.1
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
print(xtable(glm.D93, align = "r|llrc"), , floating= F,
  file = "Results/Tables/Xtable_example3.tex")
