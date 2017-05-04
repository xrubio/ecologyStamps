
library(reshape2)
library(ggplot2)
library(vegan)
library(gridExtra)
library(plyr)
library(scales)
library(RColorBrewer)

# all stamps
minCodes <- 1

presence <- read.csv("../09_presence/presenceFamilyDr.csv", header=T, sep=';')
counts <- read.csv("../07_countSample/countsFamilyDr.csv", header=T, sep=";")
locations <- read.csv("../04_locations/locationsFamilyDr.csv", header=T, sep=";")

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


# plot diversity of stamps per site
numProvinces <- length(unique(sitesSample$province))
cols <- colorRampPalette(brewer.pal(12, "Set3"))
myPalette <- cols(numProvinces)


# mean codes per site based on province
#tapply(sitesSample$numCodes, sitesSample$province, FUN=mean)

svg("stampsPerProvince.svg", width=8, height=10)    
ggplot(sitesSample, aes(y=reorder(province, numCodes), x=numCodes, fill=province)) + geom_jitter(col="grey50", alpha=0.5, shape=21, height=0.2, width=0.3, size=3) + scale_colour_manual(values=myPalette) + theme_bw() + theme(legend.position="None") + xlab("number of codes") + ylab("province distribution")
dev.off()



# distribution of stamps in sites of provinces
#ggplot(sitesSample, aes(x=province, y=numCodes)) + geom_jitter()
#ggplot(sitesSample, aes(x=province, y=numCodes, fill=factor(idSite))) + geom_bar(stat="identity")

#### INSTRUMENTA #####

svg("figura_2.svg", width=8, height=7)    
ggplot(counts, aes(x=numCodes)) + geom_histogram(binwidth=5, fill="grey70", col="grey30") +theme_bw() + ylab("n. de yacimientos") + xlab("volumen de sellos")
dev.off()

codeMrpp <- mrpp(presenceSample, sitesSample$province, distance="jaccard",weight.type=1)
codeMrpp




MrppDF <- data.frame(numSites=codeMrpp$n, distance=codeMrpp$classdelta, province=attr(codeMrpp$n, "names"))

MrppDF <- subset(MrppDF, !is.na(distance))


svg("distanceGroups.svg", width=8, height=7)    
ggplot(MrppDF, aes(y=reorder(province, -distance), x=distance, size=numSites)) + geom_point(col="skyblue3") + geom_vline(xintercept=codeMrpp$delta, col="indianred2") + scale_x_continuous(limits=c(0.85,1)) + theme_bw() + annotate("label", label="mean group distance", x=0.97, y=13.2, colour="white", fill="indianred2", fontface="bold") + theme(legend.position="bottom")
dev.off()


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

pdf("dendro.pdf")
plot(codeCluster)
dev.off()    

pdf("linksFamily.pdf", width=15, height=10)   
plot(codes.md)
dev.off()


foo <- melt(codes.md)
# paint distance matrix as quantiles to avoid linear palette colors
qn = quantile(foo$value, c(0.01, 0.99), na.rm = TRUE)
qn01 <- rescale(c(qn, range(foo$value)))

foo$Var2 <- with(foo,factor(Var2,levels = rev(sort(unique(Var2)))))

svg("dist2.svg", width=17, height=10)    
#pdf("distances.pdf", width=17, height=9)    
ggplot(foo, aes(x=Var1, y=Var2, fill=value, label=round(value,2))) + geom_raster() + geom_text(col="grey90", fontface="bold")  + theme_bw()+ scale_fill_gradientn(colours=c("indianred2", "skyblue3", "grey60"), values = c(0, seq(qn01[1], qn01[2], length.out = 18), 1)) + theme(panel.border=element_blank(), legend.position="none", axis.ticks.y=element_blank(), axis.ticks.x=element_blank()) + xlab("") + ylab("") + scale_x_discrete(position="top")
dev.off()
MrppDF$province2 <- with(MrppDF,factor(province,levels = rev(sort(unique(province)))))

#a2 <- ggplot(MrppDF, aes(y=province2, x=distance, size=numSites)) + geom_point(col="skyblue3") + geom_vline(xintercept=codeMrpp$delta, col="indianred2") + scale_x_continuous(limits=c(0.85,1)) + theme_bw() + annotate("label", label="mean group distance", x=0.96, y=13.2, colour="white", fill="indianred2", fontface="bold") + theme(legend.position="top") + ylab("")


svg("dist1.svg", width=3, height=10)
ggplot(MrppDF, aes(x=province2, y=numSites)) + geom_bar(stat="identity", fill="skyblue3") + theme_bw() + coord_flip() + xlab("") + ylab("") + ggtitle("num. sites") + scale_y_reverse() + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank())
dev.off()


grid.arrange(a2,a1, layout_matrix = rbind(c(1,2,2,2)))

