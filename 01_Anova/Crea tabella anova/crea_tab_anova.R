remove(list = ls())
tab <- read.table('tabella_tutto_07_11.txt', header = TRUE, stringsAsFactors = FALSE)
tab_norm <- read.table('tabella_7_16.txt', header = TRUE, stringsAsFactors = FALSE)
count <- 1
voti <- 0
partito <- 0
circoscr <- 0
macroreg <- 0
part <- c("sx","dx","m5s")
ii <- 1
count <- 1
voti <- rep(0,3*dim(tab)[1])
partito <- rep(0,3*dim(tab)[1])
while(ii <= 3*dim(tab)[1]){
  voti[ii] <- tab$X1[count]
  partito[ii] <- part[1]
  circoscr[ii] <- tab_norm$V26[count]
  macroreg[ii] <- tab_norm$V27[count]
  voti[ii+1] <- tab$X2[count]
  partito[ii+1] <- part[2]
  circoscr[ii+1] <- tab_norm$V26[count]
  macroreg[ii+1] <- tab_norm$V27[count]
  voti[ii+2] <- tab$X3[count]
  partito[ii+2] <- part[3]
  circoscr[ii+2] <- tab_norm$V26[count]
  macroreg[ii+2] <- tab_norm$V27[count]
  ii <- ii + 3
  count <- count + 1
}
tab_anova <- data.frame(voti,partito,circoscr,macroreg)
write.table(tab_anova, 'tab_anova_07_16.txt', row.names = FALSE)
