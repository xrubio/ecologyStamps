
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
#euclidDist <- as.matrix(read.table("../05b_distEuclidean/geoCost.txt"))
#jaccardDist <- as.matrix(read.table("../06_jaccard/jaccardDist.txt"))
#locations <- read.csv("../04c_euclidean/locations.csv", header=T, sep=";")
#counts <- read.csv("../07_countSample/counts.csv", header=T, sep=";")

# Dressel 20
euclidDist <- as.matrix(read.table("../05b_distEuclidean/geoCostDr.txt"))
jaccardDist <- as.matrix(read.table("../06_jaccard/jaccardDistDr.txt"))
locations <- read.csv("../04c_euclidean/locationsDr.csv", header=T, sep=";")
counts <- read.csv("../07_countSample/countsDr.csv", header=T, sep=";")

# Tria Nomina 
#euclidDist <- as.matrix(read.table("../05b_distEuclidean/geoCostTN.txt"))
#jaccardDist <- as.matrix(read.table("../06_jaccard/jaccardDistTN.txt"))
#locations <- read.csv("../04c_euclidean/locationsTN.csv", header=T, sep=";")
#counts <- read.csv("../07_countSample/countsTN.csv", header=T, sep=";")

# Tria Nomina AND Dressel 20
#euclidDist <- as.matrix(read.table("../05b_distEuclidean/geoCostDrTN.txt"))
#jaccardDist <- as.matrix(read.table("../06_jaccard/jaccardDistDrTN.txt"))
#locations <- read.csv("../04c_euclidean/locationsDrTN.csv", header=T, sep=";")
#counts <- read.csv("../07_countSample/countsDrTN.csv", header=T, sep=";")

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
g1 <- ggplot(mantelValues, aes(x=numCodes, y=numSites)) + geom_line() + ggtitle("number of sites per threshold value")
g2 <- ggplot(mantelValues, aes(x=numCodes, y=value)) + geom_line() + ggtitle("spatial autocorrelation value")
g3 <- ggplot(mantelValues, aes(x=numCodes, y=significance)) + geom_line() + ggtitle("p-value")

pdf("mantel.pdf")
grid.arrange(g1,g2,g3)
dev.off()


