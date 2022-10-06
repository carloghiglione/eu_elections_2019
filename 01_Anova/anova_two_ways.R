remove(list = ls())
library( MASS )
library( car )
library(ggpubr)
tab <- read.table('tab_anova_07_16.txt', header = TRUE, stringsAsFactors = FALSE)


partito_names = unique( as.character( tab$partito ) )
macroreg_names = unique( as.character( tab$macroreg ) )
n_partito = tapply( tab$partito, tab$partito, length )[ partito_names ]
n_macroreg = tapply( tab$macroreg, tab$macroreg, length )[ macroreg_names ]

cat("\n")
cat("Numerosità macro-regioni")
cat("\n")
print(n_macroreg)
cat("---------------------")
cat("\n")
cat("Numerosità partiti")
cat("\n")
print(n_partito)


#x11()
#ggboxplot(tab, x = "partito", y = "voti", color = "macroreg", palette = c("forestgreen","dodgerblue3","red"))
#x11()
#ggboxplot(tab, x = "macroreg", y = "voti", color = "partito", palette = c("dodgerblue3","gold","red"))

x11()
par(mfrow = c(1, 2))
interaction.plot(tab$partito,tab$macroreg,tab$voti)
interaction.plot(tab$macroreg,tab$partito,tab$voti)


mod <- aov(tab$voti ~ tab$partito*tab$macroreg)

cat("\n")
cat("Analisi ipotesi modello")
cat("\n")
cat("---------------------")
print(bartlett.test(tab$voti,as.factor(tab$partito):as.factor(tab$macroreg)))
cat("---------------------")
cat("\n")
print(leveneTest(tab$voti,as.factor(tab$partito):as.factor(tab$macroreg)))
cat("---------------------")
cat("\n")
cat("Shapiro-Wilk test sui gruppi")
cat("\n")
print(tapply( tab$voti,as.factor(tab$partito):as.factor(tab$macroreg), function( x ) shapiro.test( x )$p ))
cat("\n\n")


out_val <- as.numeric(boxplot(mod$residuals, plot = FALSE)$out)
out_ind <- which(mod$residuals %in% out_val )
tab$voti[out_ind] <- NA


mod_adj <- aov(tab$voti ~ tab$partito*tab$macroreg)
cat("Analisi ipotesi modello senza outlier")
cat("\n")
cat("---------------------")
print(bartlett.test(tab$voti,as.factor(tab$partito):as.factor(tab$macroreg)))
cat("---------------------")
cat("\n")
print(leveneTest(tab$voti,as.factor(tab$partito):as.factor(tab$macroreg)))
cat("---------------------")
cat("\n")
cat("Shapiro-Wilk test sui gruppi")
cat("\n")
print(tapply( tab$voti,as.factor(tab$partito):as.factor(tab$macroreg), function( x ) shapiro.test( x )$p ))
cat("\n\n")


b = boxcox( mod_adj, lambda = seq(-3,3,by=0.01),plotit = FALSE)
best_lambda = b$x[ which.max( b$y ) ]
lambda <- 0.5

mod_cox <- aov(tab$voti^lambda ~ tab$partito*tab$macroreg)


cat("Analisi ipotesi modello trasformato")
cat("\n")
cat("---------------------")
print(bartlett.test(tab$voti^lambda,as.factor(tab$partito):as.factor(tab$macroreg)))
cat("---------------------")
cat("\n")
print(leveneTest(tab$voti^lambda,as.factor(tab$partito):as.factor(tab$macroreg)))
cat("---------------------")
cat("\n")
cat("Shapiro-Wilk test sui gruppi")
cat("\n")
print(tapply( tab$voti^lambda,as.factor(tab$partito):as.factor(tab$macroreg), function( x ) shapiro.test( x )$p ))
cat("\n\n")


print(summary(mod_cox))
tab_medie <- model.tables(mod_cox,type="means")
print(tab_medie)




# x11()
# par(mfrow = c(1, 2))
# boxplot(tab$voti^best_lambda ~ tab$partito)
# boxplot(tab$voti^best_lambda ~ tab$macroreg)



x11()
par(mfrow = c(2, 3))
boxplot(mod_cox$residuals)
hist(rstandard(mod_cox), breaks=seq(min(rstandard(mod_cox)),max(rstandard(mod_cox)),l=10+1))
qqnorm(mod_cox$residuals, pch = 16)
qqline(mod_cox$residuals, col='red')
plot(mod_cox$fitted.values,rstandard(mod_cox))
abline(h = 0, col='red')
abline(h = 2, col='blue')
abline(h = -2, col='blue')
obs_ord <- 1:length(mod_cox$residuals)
plot(obs_ord,rstandard(mod_cox))
abline(h = 0, col='red')
abline(h = 2, col='blue')
abline(h = -2, col='blue')
ind_m5s <- which(tab$partito %in%  "m5s" )
ind_sx <- which(tab$partito %in%  "sx" )
ind_dx <- which(tab$partito %in%  "dx" )
points(obs_ord[ind_m5s], rstandard(mod_cox)[ind_m5s], col = "gold", pch = 16)
points(obs_ord[ind_dx], rstandard(mod_cox)[ind_dx], col = "dodgerblue3", pch = 16)
points(obs_ord[ind_sx], rstandard(mod_cox)[ind_sx], col = "red", pch = 16)
plot(obs_ord,rstandard(mod_cox))
abline(h = 0, col='red')
abline(h = 2, col='blue')
abline(h = -2, col='blue')
ind_N <- which(tab$macroreg %in%  "N" )
ind_C <- which(tab$macroreg %in%  "C" )
ind_S <- which(tab$macroreg %in%  "S" )
points(obs_ord[ind_N], rstandard(mod_cox)[ind_N], col = "dodgerblue3", pch = 16)
points(obs_ord[ind_C], rstandard(mod_cox)[ind_C], col = "forestgreen", pch = 16)
points(obs_ord[ind_S], rstandard(mod_cox)[ind_S], col = "red", pch = 16)


nomi_N <- c("m5s:N-dx:N","sx:N-dx:N","sx:N-m5s:N")
nomi_C <- c("m5s:C-dx:C","sx:C-dx:C","sx:C-m5s:C")
nomi_S <- c("m5s:S-dx:S","sx:S-dx:S","sx:S-m5s:S")
nomi_m5s <- c("m5s:N-m5s:C","m5s:S-m5s:C","m5s:S-m5s:N")
nomi_dx <- c("dx:N-dx:C","dx:S-dx:C","dx:S-dx:N")
nomi_sx <- c("sx:N-sx:C","sx:S-sx:C","sx:S-sx:C","sx:S-sx:N")
tuk <- TukeyHSD(mod_cox)
tuk_tab <- tuk$`tab$partito:tab$macroreg`
tuk_N <- tuk_tab[which(nomi_N == row.names(tuk_tab)),]
tuk_C <- tuk_tab[which(nomi_C == row.names(tuk_tab)),]
tuk_S <- tuk_tab[which(nomi_S == row.names(tuk_tab)),]
tuk_m5s <- tuk_tab[(row.names(tuk_tab) %in% nomi_m5s),]
tuk_dx <- tuk_tab[(row.names(tuk_tab) %in% nomi_dx),]
tuk_sx <- tuk_tab[(row.names(tuk_tab) %in% nomi_sx),]


cat("\n")
cat("Test Tukey macro-regioni Lega")
cat("\n")
print(tuk_dx)
cat("---------------------")
cat("\n")
cat("Test Tukey macro-regioni PD")
cat("\n")
print(tuk_sx)
cat("---------------------")
cat("\n")
cat("Test Tukey macro-regioni M5S")
cat("\n")
print(tuk_m5s)
