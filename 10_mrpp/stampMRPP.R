
library(reshape2)
library(ggplot2)
library(vegan)
library(gridExtra)
library(ggdendro)
library(plyr)
library(pwr)

    
getStampMrpp <- function(locations, countsWithProvince, presence,minCodes)
{
    print(minCodes)

    numReps <- apply(presence,2,sum)
    listCodes <- numReps[which(numReps>=minCodes)]
    presenceSample <- presence[,which(colnames(presence) %in% names(listCodes))]

    # remove sites without any stamp
    stampsPerSite <- apply(presenceSample,1,sum)
    presenceSample2 <- presenceSample[which(stampsPerSite>0),]

    locSample <- locations[which(locations$id %in% rownames(presenceSample2)),]

#    sitesSample <- subset(countsWithProvince, numCodes >= minCodes)

    # remove sites from province with less than 2 sites
    sitesInProvince = count(locSample$province)
    provincesSingleSite = subset(sitesInProvince, freq<2)$x
    sitesSample <- subset(locSample, !province %in% provincesSingleSite)

    presenceSample3 <- presenceSample2[which(rownames(presenceSample2) %in% sitesSample$id),]
    codeMrpp <- mrpp(presenceSample3, sitesSample$province, distance="jaccard",weight.type=1)
#    print(codeMrpp)
    return (c(codeMrpp$delta, codeMrpp$A, codeMrpp$Pvalue, nrow(sitesSample), ncol(presenceSample3)))
}

presence <- read.csv("../09_presence/presenceFamily.csv", header=T, sep=';')    
rownames(presence) <- presence[,1]
presence <- presence[,-1]

counts <- read.csv("../07_countSample/countsFamily.csv", header=T, sep=";")
locations <- read.csv("../04_locations/locationsFamily.csv", header=T, sep=";")

counts$province <- locations$province
countsWithProvince <-  subset(counts, province != '')


mrppValues <- data.frame(delta=double(), effect=double(), pvalue=double(), codeReps=integer(), numSites=integer(), numCodes=integer())
for(i in 1:100)
{
    results <- getStampMrpp(locations, countsWithProvince, presence, i)

    mrppValues<- rbind(mrppValues, data.frame(delta=results[1], effect=results[2], pvalue=results[3], codeReps=i, numSites=results[4], numCodes=results[5]))
}


mrppValuesBelowP <- subset(mrppValues, pvalue<0.05)
mrppValuesBelowP[which.max(mrppValuesBelowP$effect),]




g1 <- ggplot(mrppValues, aes(x=codeReps, y=numSites)) + geom_line(size=1, col="goldenrod1") + xlab("") + ylab("freq. of stamps") +  annotate("label", label="sample size", x=95, y=500, colour="white", fill="goldenrod1", fontface="bold") + theme_bw()
g2 <- ggplot(mrppValues, aes(x=codeReps, y=delta)) + geom_line(size=1, col="indianred2")  + xlab("") + ylab("delta") + annotate("label", label="mean group distance", x=90, y=0.1, colour="white", fill="indianred2", fontface="bold") + theme_bw() + scale_y_continuous(limits=c(0,1))
g3 <- ggplot(mrppValues, aes(x=codeReps, y=effect)) + geom_line(size=1, col="palegreen4") + xlab("") + ylab("distance means") + annotate("label", label="effect", x=95, y=0.003, colour="white", fill="palegreen4", fontface="bold") + theme_bw()
#g4 <- ggplot(mrppValues, aes(x=codeReps, y=pvalue)) + geom_line(size=1, col="skyblue3") + xlab("n. stamps (threshold)") + ylab("p-value")+ annotate("label", label="significance", x=95, y=0.6, colour="white", fill="skyblue3", fontface="bold") + geom_hline(yintercept=0.05, col="grey50", size=1, linetype="twodash") +  theme_bw() + scale_y_continuous(limits=c(0,1))
g4 <- ggplot(mrppValues, aes(x=codeReps, y=numCodes)) + geom_line(size=1, col="skyblue3") + xlab("n. stamps (threshold)") + ylab("num. codes")+ annotate("label", label="significance", x=95, y=0.6, colour="white", fill="skyblue3", fontface="bold") +  theme_bw()

grid.arrange(g1,g2,g3,g4, ncol=1)



######################## single run code
codeReps <- 73

presence <- read.csv("../09_presence/presenceFamily.csv", header=T, sep=';')    
rownames(presence) <- presence[,1]
presence <- presence[,-1]

counts <- read.csv("../07_countSample/countsFamily.csv", header=T, sep=";")
locations <- read.csv("../04_locations/locationsFamily.csv", header=T, sep=";")

counts$province <- locations$province
countsWithProvince <-  subset(counts, province != '')

numReps <- apply(presence,2,sum)
listCodes <- numReps[which(numReps>=codeReps)]
presenceSample <- presence[,which(colnames(presence) %in% names(listCodes))]

# remove sites without any stamp
stampsPerSite <- apply(presenceSample,1,sum)
presenceSample2 <- presenceSample[which(stampsPerSite>0),]

locSample <- locations[which(locations$id %in% rownames(presenceSample2)),]

# remove sites from province with less than 2 sites
sitesInProvince = count(locSample$province)
provincesSingleSite = subset(sitesInProvince, freq<2)$x
sitesSample <- subset(locSample, !province %in% provincesSingleSite)

presenceSample3 <- presenceSample2[which(rownames(presenceSample2) %in% sitesSample$id),]
codeMrpp <- mrpp(presenceSample3, sitesSample$province, distance="jaccard",weight.type=1)

codes.md <- meandist(vegdist(presenceSample3, method='jaccard'), sitesSample$province)
codes.md
summary(codes.md)
plot(codes.md)

foo <- melt(codes.md)
# paint distance matrix as quantiles to avoid linear palette colors
qn = quantile(foo$value, c(0.01, 0.99), na.rm = TRUE)
qn01 <- rescale(c(qn, range(foo$value)))

foo$Var2 <- with(foo,factor(Var2,levels = rev(sort(unique(Var2)))))
ggplot(foo, aes(x=Var1, y=Var2, fill=value, label=round(value,2))) + geom_raster() + geom_text(col="grey90", fontface="bold")  + theme_bw()+ scale_fill_gradientn(colours=c("indianred2", "skyblue3", "grey60"), values = c(0, seq(qn01[1], qn01[2], length.out = 18), 1)) + theme(panel.border=element_blank(), legend.position="none", axis.ticks.y=element_blank(), axis.ticks.x=element_blank()) + xlab("") + ylab("") + scale_x_discrete(position="top")

    

MrppDF <- data.frame(numSites=codeMrpp$n, distance=codeMrpp$classdelta, province=attr(codeMrpp$n, "names"))

MrppDF <- subset(MrppDF, !is.na(distance))
ggplot(MrppDF, aes(y=reorder(province, -distance), x=distance, size=numSites)) + geom_point(col="skyblue3") + geom_vline(xintercept=codeMrpp$delta, col="indianred2")  + theme_bw() + theme(legend.position="bottom")

