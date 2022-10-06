remove(list = ls())
library( MASS )
library( car )
library( faraway )

tab <- read.table('tab_anova_07_16.txt', header = TRUE, stringsAsFactors = FALSE)
circoscrizioni <- c("A","B","C","D","E")
macroregioni <- c("N","C","S")
circo <- circoscrizioni[2]
macror <- macroregioni[1]
ind_voti_cir <- which(tab$circoscr == circo)
ind_voti_reg <- which(tab$macroreg == macror)
new_tab <- tab[ind_voti_cir,]

mod <- aov(new_tab$voti~new_tab$partito)
#mod <- boxcox(mod)
print(bartlett.test(new_tab$voti~new_tab$partito))
print(leveneTest(new_tab$voti,as.factor(new_tab$partito)))
boxplot(new_tab$voti~new_tab$partito)

# ind_voti <- which(tab$macroreg == "S" & tab$partito == "m5s")
# hist(tab$voti[ind_voti], breaks = 10)
# print(shapiro.test(tab$voti[ind_voti]))