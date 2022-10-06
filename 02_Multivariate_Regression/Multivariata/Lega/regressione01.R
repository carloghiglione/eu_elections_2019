remove(list = ls())
library(MASS)
library(car)
tab <- read.table('tabella_tutto_07_11.txt', header = TRUE, stringsAsFactors = FALSE);
m5s <- tab$X3;
sx <- tab$X1;
dx <- tab$X2;

partito <- m5s
dd1 <- data.frame(partito,log(tab$popol), tab[,11:12] )
dd2 <- data.frame(partito, tab[,13:15] )
dd3 <- data.frame(partito, tab[,16:20] )
dd4 <- data.frame(partito, tab[,21:25] )
x11()
pairs(dd1)
x11()
pairs(dd2)
x11()
pairs(dd3)
x11()
pairs(dd4)



mod <- lm(partito ~ tab$FasciaEta2 + tab$uni + tab$aste + tab$stranieri + tab$redditobasso + tab$disoccup);
lev <- hat(model.matrix(mod))
print(summary(mod))
print(shapiro.test(mod$residuals))
pp <- mod$rank
nn <- length(tab$province)



# out_ind <- c(out_box, out_lev);
# out_cook <- which(cooks.distance(mod) == max(cooks.distance(mod)));
# out_ind <- c(out_ind, out_cook);
# out_ind <- unique(out_ind);

out_val <- as.numeric(boxplot(mod$residuals, plot = FALSE)$out);
out_box <- which(mod$residuals %in% out_val );
lever = which(lev > 2*pp/nn)
out_et_lev = which(lever %in% out_box)
out_et_lev = lever[out_et_lev]
max_lev <- which(lev == max(lev))
cok = which(as.numeric(cooks.distance(mod)) >= 4/(nn-pp))
out_et_cook = which(cok %in% out_box)
out_et_cook = cok[out_et_cook]
out_ind = unique(c(out_et_lev,out_et_cook,max_lev))
print(tab$provincie[out_ind])


partito_adj <- partito;
partito_adj[out_ind] <- NA;
mod_adj <- lm(partito_adj ~ tab$FasciaEta2 + tab$uni + tab$aste + tab$stranieri + tab$redditobasso + tab$disoccup);
lev_adj <- hat(model.matrix(mod_adj))
print(summary(mod_adj))
print(shapiro.test(mod_adj$residuals))
pp <- mod$rank
nn_adj <- length(partito_adj)

# b <- boxcox(mod_adj, plotit = FALSE);
# best_lambda_ind = which.max( b$y )
# best_lambda = b$x[ best_lambda_ind ]

# partito_cox <- partito_adj^best_lambda;
# mod_cox <- lm(partito_cox ~ tab$FasciaEta2 + tab$uni + tab$aste + tab$stranieri + tab$redditobasso + tab$disoccup);
# lev_cox <- hat(model.matrix(mod_cox))
# print(summary(mod_cox))
# print(shapiro.test(mod_cox$residuals))
# pp <- mod$rank
# nn_cox <- length(partito_cox)

x11()                        #modello completo
par(mfrow = c(2, 3));
boxplot(mod$residuals)
hist(rstandard(mod), breaks=seq(min(rstandard(mod)),max(rstandard(mod)),l=10+1))
plot(mod$fitted.values,rstandard(mod), pch = 16);
#points(mod$fitted.values[out_box],rstandard(mod)[out_box], pch = 16, col='red');
points(mod$fitted.values[out_et_cook],rstandard(mod)[out_et_cook], pch = 16, col='orange');
points(mod$fitted.values[out_et_lev],rstandard(mod)[out_et_lev], pch = 16, col='green');
points(mod$fitted.values[max_lev],rstandard(mod)[max_lev], pch = 16, col='red');
abline(h = 0, col='red')
abline(h = 2, col='blue')
abline(h = -2, col='blue')
qqnorm(mod$residuals, pch = 16)
qqline(mod$residuals, col='red')
plot(mod$fitted.values,lev, pch = 16)
points(mod$fitted.values[out_et_lev],lev[out_et_lev], pch = 16, col='green')
points(mod$fitted.values[max_lev],lev[max_lev], pch = 16, col='red')
abline(h = 2*pp/nn, col='red')
plot(mod$fitted.values, cooks.distance(mod), pch = 16)
points(mod$fitted.values[out_et_cook],cooks.distance(mod)[out_et_cook], pch = 16, col='orange')
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

# x11()                            #modello boxcox
# par(mfrow = c(2, 3));
# boxplot(mod_cox$residuals)
# hist(rstandard(mod_cox), breaks=seq(min(rstandard(mod_cox)),max(rstandard(mod_cox)),l=10+1))
# plot(mod_cox$fitted.values,rstandard(mod_cox), pch = 16);
# abline(h = 0, col='red')
# abline(h = 2, col='blue')
# abline(h = -2, col='blue')
# qqnorm(mod_cox$residuals, pch = 16)
# qqline(mod_cox$residuals, col='red')
# plot(mod_cox$fitted.values,lev_adj, pch = 16)
# abline(h = 2*pp/nn_cox, col='red')
# plot(mod_cox$fitted.values, cooks.distance(mod_cox), pch = 16)
# abline(h = 4/(nn_cox-pp), col='red')

#x11()
#heatmap(abs(cor(X)),Rowv = NA, Colv = NA, symm = T)
