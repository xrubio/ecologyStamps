
library(reshape2)
library(ggplot2)
library(vegan)
library(gridExtra)


getMantel <- function( jaccardDist, euclidDist, counts, minCodes)
{
    print(minCodes)
    sitesSample <- subset(counts, numCodes >= minCodes)
    jaccardDistSample <- jaccardDist[sitesSample$idSite,sitesSample$idSite]
    euclidDistSample <- euclidDist[sitesSample$idSite,sitesSample$idSite]
    mantelResult <- mantel(jaccardDistSample, euclidDistSample, permutations=999)
    mantelResult$sampleSize <- nrow(sitesSample)
    return(mantelResult)
}

# all stamps
#euclidDist <- as.matrix(read.table("../05_euclidean/geoCostAll.txt"))
#jaccardDist <- as.matrix(read.table("../06_jaccard/jaccardAll.txt"))
#locations <- read.csv("../04_locations/locations.csv", header=T, sep=";")
#counts <- read.csv("../07_countSample/countsAll.csv", header=T, sep=";")

# Dressel 20
#euclidDist <- as.matrix(read.table("../05_euclidean/geoCostDr.txt"))
#jaccardDist <- as.matrix(read.table("../06_jaccard/jaccardDr.txt"))
#locations <- read.csv("../04_locations/locationsDr.csv", header=T, sep=";")
#counts <- read.csv("../07_countSample/countsDr.csv", header=T, sep=";")

# family
#euclidDist <- as.matrix(read.table("../05_euclidean/geoCostFamily.txt"))
#jaccardDist <- as.matrix(read.table("../06_jaccard/jaccardFamily.txt"))
#locations <- read.csv("../04_locations/locationsFamily.csv", header=T, sep=";")
#counts <- read.csv("../07_countSample/countsFamily.csv", header=T, sep=";")

# family Dressel 20
euclidDist <- as.matrix(read.table("../05_euclidean/geoCostFamilyDr.txt"))
jaccardDist <- as.matrix(read.table("../06_jaccard/jaccardFamilyDr.txt"))
locations <- read.csv("../04_locations/locationsFamilyDr.csv", header=T, sep=";")
counts <- read.csv("../07_countSample/countsFamilyDr.csv", header=T, sep=";")

counts$province <- locations$province 

rownames(euclidDist) <- locations[,1]
colnames(euclidDist) <- locations[,1]                                                                                                                                                                                                    
rownames(jaccardDist) <- locations[,1]
colnames(jaccardDist) <- locations[,1]              


mantelValues <- data.frame(numCodes=integer(), value=double(), significance=double(), numSites=integer())
for( i in 1:100)
{
    results <- getMantel(jaccardDist, euclidDist, counts, i)
    mantelValues <- rbind(mantelValues, data.frame(numCodes=i, value=results$statistic, significance=results$signif, numSites=results$sampleSize))
}

# distances
#geoDists <- melt(jaccardDist)
#cultDists <- melt(euclidDist)
#g1 <- ggplot(counts, aes(x=numCodes)) + geom_histogram()    
#g2 <- ggplot(geoDists,aes(x=value)) + geom_histogram()
#g3 <- ggplot(cultDists,aes(x=value)) + geom_histogram()
#grid.arrange(g1,g2,g3)

# structure of stamps per site
#ggplot(counts, aes(x =reorder(factor(idSite), -numCodes), y=numCodes)) + geom_bar(stat='identity')

# mantel results
g1 <- ggplot(mantelValues, aes(x=numCodes, y=numSites)) + geom_line(size=1, col="goldenrod1") + xlab("") + ylab("number of sites") +  annotate("label", label="sample size", x=95, y=100, colour="white", fill="goldenrod1", fontface="bold") + theme_bw()
g2 <- ggplot(mantelValues, aes(x=numCodes, y=value)) + geom_line(size=1, col="indianred2") + xlab("") + ylab("Mantel statistic r") + annotate("label", label="correlation", x=95, y=0.05, colour="white", fill="indianred2", fontface="bold")+ theme_bw()
g3 <- ggplot(mantelValues, aes(x=numCodes, y=significance)) + geom_line(size=1, col="skyblue3") + xlab("n. stamps (threshold)") + ylab("p-value")+ annotate("label", label="significance", x=95, y=0.01, colour="white", fill="skyblue3", fontface="bold") + geom_hline(yintercept=0.05, col="grey50", size=1, linetype="twodash") +  theme_bw() 

svg("mantelFamilyDr.svg", width=12, height=9)
grid.arrange(g1,g2,g3, top="Mantel test for family Dressel 20")
dev.off()


