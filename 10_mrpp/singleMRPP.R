
library(reshape2)
library(ggplot2)
library(vegan)
library(gridExtra)
library(ggdendro)
library(plyr)


# all stamps
minCodes <- 10
presence <- read.csv("../09_presence/presenceTN.csv", header=T, sep=';')
counts <- read.csv("../07_countSample/countsTN.csv", header=T, sep=";")
locations <- read.csv("../04c_euclidean/locationsTN.csv", header=T, sep=";")
counts$province <- locations$province
countsWithProvince <-  subset(counts, province != '')

sitesSample <- subset(countsWithProvince, numCodes >= minCodes)

sitesInProvince = count(sitesSample$province)
provincesSingleSite = subset(sitesInProvince, freq<2)$x
sitesSample <- subset(sitesSample, !province %in% provincesSingleSite)

presenceSample <- subset(presence, site %in% sitesSample$idSite)
# remove the column with id site (the first one)
presenceSample <- presenceSample[,-1]
# remove code if no site has the code    
presenceSample <- presenceSample[,colSums(presenceSample) != 0]

# distribution of stamps in sites of provinces
#ggplot(sitesSample, aes(x=province, y=numCodes)) + geom_jitter()
#ggplot(sitesSample, aes(x=province, y=numCodes, fill=factor(idSite))) + geom_bar(stat="identity")


codeMrpp <- mrpp(presenceSample, sitesSample$province, distance="jaccard",weight.type=1)
codeMrpp




MrppDF <- data.frame(numSites=codeMrpp$n, distance=codeMrpp$classdelta, province=attr(codeMrpp$n, "names"))

MrppDF <- subset(MrppDF, !is.na(distance))


ggplot(MrppDF, aes(x=reorder(province, distance), y=distance, size=numSites)) + geom_point() + geom_hline(yintercept=codeMrpp$delta, col="red") + scale_y_continuous(limits=c(0,1)) + theme_bw()


# aquest plot va be per mirar les P que les permutacions aleatories generin clusters mes agrupats que no pas el resultat
# Save and change plotting parameters
def.par <- par(no.readonly = TRUE)
layout(matrix(1:2,nr=1))

plot(codes.ord <- metaMDS(presenceSample), type="text", display="sites" )
ordihull(codes.ord, sitesSample$province)

with(codeMrpp, {
  fig.dist <- hist(boot.deltas, xlim=range(c(delta,boot.deltas)), 
                 main="Test of Differences Among Groups")
  abline(v=delta); 
  text(delta, 2*mean(fig.dist$counts), adj = -0.5,
     expression(bold(delta)), cex=1.5 )  }
)
par(def.par)



## meandist
codes.md <- meandist(vegdist(presenceSample, method='jaccard'), sitesSample$province)
codes.md
summary(codes.md)



codeCluster <- hclust(as.dist(codes.md), method="average")
ggdendrogram(codeCluster)
plot(codes.md)
plot(codeCluster)
#dendroCluster <- as.dendrogram(codeCluster)
#dCodes <- dendro_data(dendroCluster, type = "rectangle")
#ggplot(segment(dCodes)) + geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +  coord_flip() +  scale_y_reverse(expand = c(0.2, 0))


pdf("linksTN.pdf", width=15, height=10)   
    
plot(codes.md)

dev.off()

foo <- melt(codes.md)
ggplot(foo, aes(x=Var1, y=Var2, fill=value, label=round(value,2))) + geom_raster() + geom_text()

