remove(list = ls())
library(MASS)
library(car)
tab <- read.table('norm_tabella_tutto_07_14_bis.txt', header = TRUE, stringsAsFactors = FALSE);
m5s <- tab$X3;
sx <- tab$X1;
dx <- tab$X2;

#tab_no_problem <- data.frame(tab[,1:19], tab[,21:25])     #levo la riga di eta5 perché crea dei NA brutti


partito <- sx
mod <- lm(formula = partito ~ ., data = tab[, 10:24])


lev <- hat(model.matrix(mod))
print(summary(mod))
print(shapiro.test(mod$residuals))
pp <- mod$rank
nn <- length(tab$province)


out_val <- as.numeric(boxplot(mod$residuals, plot = FALSE)$out);
out_box <- which(mod$residuals %in% out_val );
lever = which(lev > 2*pp/nn)
out_et_lev = which(lever %in% out_box)
out_et_lev = lever[out_et_lev]
max_lev <- which(lev == max(lev))
max_cook <- which(as.numeric(cooks.distance(mod)) == max(as.numeric(cooks.distance(mod))))
cok = which(as.numeric(cooks.distance(mod)) >= 4/(nn-pp))
out_et_cook = which(cok %in% out_box)
out_et_cook = cok[out_et_cook]
out_ind = unique(c(out_et_lev,out_et_cook,max_lev,max_cook))
#out_ind = unique(c(out_et_lev,out_et_cook))
print(tab$provincie[out_ind])


partito_adj <- partito;
partito_adj[out_ind] <- NA;
mod_adj <- lm(partito_adj ~ ., data = tab[, 10:24]);
lev_adj <- hat(model.matrix(mod_adj))
print(summary(mod_adj))
print(shapiro.test(mod_adj$residuals))
pp <- mod$rank
nn_adj <- length(partito_adj)


b <- boxcox(mod_adj, plotit = FALSE);
best_lambda_ind = which.max( b$y )
best_lambda = b$x[ best_lambda_ind ]

partito_cox <- partito_adj^best_lambda;
mod_cox <- lm(partito_cox ~ ., data = tab[, 10:24]);
lev_cox <- hat(model.matrix(mod_cox))
print(summary(mod_cox))
print(shapiro.test(mod_cox$residuals))


save(mod_adj, partito_adj, tab, file = "mod_adj.RData")



x11()                        #modello completo
par(mfrow = c(2, 3));
boxplot(mod$residuals)
hist(rstandard(mod), breaks=seq(min(rstandard(mod)),max(rstandard(mod)),l=10+1))
plot(mod$fitted.values,rstandard(mod), pch = 16);
points(mod$fitted.values[out_et_cook],rstandard(mod)[out_et_cook], pch = 16, col='orange');
points(mod$fitted.values[max_cook],rstandard(mod)[max_cook], pch = 16, col='orange');
points(mod$fitted.values[out_et_lev],rstandard(mod)[out_et_lev], pch = 16, col='green');
points(mod$fitted.values[max_lev],rstandard(mod)[max_lev], pch = 16, col='green');
abline(h = 0, col='red')
abline(h = 2, col='blue')
abline(h = -2, col='blue')
qqnorm(mod$residuals, pch = 16)
qqline(mod$residuals, col='red')
plot(mod$fitted.values,lev, pch = 16)
points(mod$fitted.values[out_et_lev],lev[out_et_lev], pch = 16, col='green')
points(mod$fitted.values[max_lev],lev[max_lev], pch = 16, col='green')
abline(h = 2*pp/nn, col='red')
plot(mod$fitted.values, cooks.distance(mod), pch = 16)
points(mod$fitted.values[out_et_cook],cooks.distance(mod)[out_et_cook], pch = 16, col='orange')
points(mod$fitted.values[max_cook],cooks.distance(mod)[max_cook], pch = 16, col='orange')
abline(h = 4/(nn-pp), col='red')

x11()                            #modello senza gli outliers
par(mfrow = c(2, 3));
boxplot(mod_adj$residuals)
hist(rstandard(mod_adj), breaks=seq(min(rstandard(mod_adj)),max(rstandard(mod_adj)),l=10+1))
plot(mod_adj$fitted.values,rstandard(mod_adj), pch = 16);
abline(h = 0, col='red')
abline(h = 2, col='blue')
abline(h = -2, col='blue')
qqnorm(mod_adj$residuals, pch = 16)
qqline(mod_adj$residuals, col='red')
plot(mod_adj$fitted.values,lev_adj, pch = 16)
abline(h = 2*pp/nn_adj, col='red')
plot(mod_adj$fitted.values, cooks.distance(mod_adj), pch = 16)
abline(h = 4/(nn_adj-pp), col='red')

#avPlots(mod_adj)             non farlo nello script che crasha
