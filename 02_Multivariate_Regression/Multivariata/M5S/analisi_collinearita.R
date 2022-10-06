remove(list = ls())
library(pheatmap)
library(car)
load("mod_adj.RData")

Z = model.matrix(mod_adj)
Z = Z[,2:dim(Z)[2]]
cor_tab = abs(cor(Z))
pheatmap( cor_tab, color = colorRampPalette(c("papayawhip", "indianred2"))(5), cluster_rows = FALSE, cluster_cols = FALSE)
print(summary(mod_adj))
print(vif(mod_adj))


cov_escluse <- c(14,16,17)
cov_yes <- which(! 10:24 %in% cov_escluse)
dati_mod_no_col <- data.frame(tab[,cov_yes + 9])
mod_new <- lm(partito_adj ~ ., data = dati_mod_no_col)
Znew = model.matrix(mod_new)
Znew = Znew[,2:dim(Znew)[2]]
cor_tab_new = abs(cor(Znew))


print(summary(mod_new))
print(shapiro.test(mod_new$residuals))
print(vif(mod_new))
#pheatmap( cor_tab_new, color = colorRampPalette(c("papayawhip", "indianred2"))(5), cluster_rows = FALSE, cluster_cols = FALSE)


indici_si = !is.na(partito_adj)
x11()
tab_eta <- data.frame(tab$FasciaEta1,tab$FasciaEta2,tab$FasciaEta4)
tab_eta <- tab_eta[indici_si,]
pairs(tab_eta,lower.panel = NULL)
x11()
cov_legate = data.frame(tab[indici_si,11:15])
pairs(cov_legate, lower.panel = NULL)


mod_no_col <- mod_new
save(mod_no_col, partito_adj, tab, dati_mod_no_col, file = "mod_no_col.RData")

#avPlot(mod_new,variable.names(mod_new)[4])