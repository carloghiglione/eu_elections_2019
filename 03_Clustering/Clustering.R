library(factoextra)
tabtab <- read.table('norm_tabella_tutto_07_14_bis.txt', header = TRUE, stringsAsFactors = FALSE);

tab = scale(tabtab[,2:20]);
names(tab)

#Prendo uni,aste,stranieri,disoccup
tab = tab[,c(10,11,12,14)];
clust = kmeans(tab,2);     #crea due cluster secondo il principio delle covariate pi+ significative
clust
fviz_cluster(clust, data = tab , palette = "jco", ggtheme = theme_minimal());

tab_clust = tabtab[,1];
tab_clust = data.frame(tabtab[,1] , clust$cluster)
names(tab_clust) = c('provincia' , 'cluster')

write.csv(tab_clust, 'clust.csv')

pairs(tab , col = clust$cluster)