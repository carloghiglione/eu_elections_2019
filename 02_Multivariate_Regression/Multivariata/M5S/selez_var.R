remove(list = ls())
library(car)
library(pheatmap)
load("mod_no_col.RData")


#vedo che il dato 51 è troppo brutto per il grafico di regressione parziale per la popol
#partito_adj[51] <- NA  #non lo faccio perché crea cosine un po' strane
mod_no_col <- lm(partito_adj~ ., data = dati_mod_no_col)
R2_adj_start <- summary(mod_no_col)$adj.r.squared


mod_new <- step(mod_no_col, direction = "backward", k=qchisq(0.95,1))
#mod_new <- step(mod_no_col_adj, direction = "backward",test="F")
aic_list <- mod_new$anova[,6]
#R2_adj_parz <-  summary(mod_new)$adj.r.squared
#mod_new <- update(mod_new,~ .-perc_posti_osp)
R2_adj_end <-  summary(mod_new)$adj.r.squared


print(summary(mod_new))
print(shapiro.test(mod_new$residuals))
print(vif(mod_new))


Z = model.matrix(mod_new)
Z = Z[,2:dim(Z)[2]]
cor_tab = abs(cor(Z))
pheatmap( cor_tab, color = colorRampPalette(c("papayawhip", "indianred2"))(5), cluster_rows = FALSE, cluster_cols = FALSE)


x11()
avPlots(mod_new)


x11()
par(mfrow = c(2, 2));
boxplot(mod_new$residuals)
hist(rstandard(mod_new), breaks=seq(min(rstandard(mod_new)),max(rstandard(mod_new)),l=10+1))
plot(mod_new$fitted.values,rstandard(mod_new), pch = 16);
abline(h = 0, col='red')
abline(h = 2, col='blue')
abline(h = -2, col='blue')
qqnorm(mod_new$residuals, pch = 16)
qqline(mod_new$residuals, col='red')

# Z = model.matrix(mod_new)
# Z = Z[,2:dim(Z)[2]]
# cor_tab = abs(cor(Z))
# pheatmap( cor_tab, color = colorRampPalette(c("papayawhip", "indianred2"))(5), cluster_rows = FALSE, cluster_cols = FALSE)