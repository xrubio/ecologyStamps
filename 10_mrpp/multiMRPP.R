
library(reshape2)
library(ggplot2)
library(vegan)
library(gridExtra)
library(ggdendro)

getMrpp <- function(countsWithProvince, presence,minCodes)
{
    print(minCodes)
    sitesSample <- subset(countsWithProvince, numCodes >= minCodes)

    # remove sites from province with less than 2 sites
    sitesInProvince = count(sitesSample$province)
    provincesSingleSite = subset(sitesInProvince, freq<2)$x
    sitesSample <- subset(sitesSample, !province %in% provincesSingleSite)

    presenceSample <- subset(presence, site %in% sitesSample$idSite)
    # remove the column with id site (the first one)
    presenceSample <- presenceSample[,-1]
    # remove code if no site has the code    
    presenceSample <- presenceSample[,colSums(presenceSample) != 0]

    codeMrpp <- mrpp(presenceSample, sitesSample$province, distance="jaccard",weight.type=1)
#    print(codeMrpp)
    return (c(codeMrpp$delta, codeMrpp$A, codeMrpp$Pvalue, nrow(sitesSample)))
}

presence <- read.csv("../09_presence/presenceDrTN.csv", header=T, sep=';')
counts <- read.csv("../07_countSample/countsDrTN.csv", header=T, sep=";")
locations <- read.csv("../04c_euclidean/locationsDrTN.csv", header=T, sep=";")
counts$province <- locations$province
countsWithProvince <-  subset(counts, province != '')


mrppValues <- data.frame(delta=double(), effect=double(), pvalue=double(), minCodesPerSite=integer(), numSites=integer())
for(i in 1:100)
{
    results <- getMrpp(countsWithProvince, presence, i)
    mrppValues<- rbind(mrppValues, data.frame(delta=results[1], effect=results[2], pvalue=results[3], minCodesPerSite=i, numSites=results[4]))
}

# print best value

mrppValuesBelowP <- subset(mrppValues, pvalue<0.05)
mrppValuesBelowP[which.max(mrppValuesBelowP$effect),]


# multi MRPP results
g1 <- ggplot(mrppValues, aes(x=minCodesPerSite, y=numSites)) + geom_line() + ggtitle("number of sites per threshold value")
g2 <- ggplot(mrppValues, aes(x=minCodesPerSite, y=delta)) + geom_line() + ggtitle("delta") + scale_y_continuous(limits=c(0,1))
g3 <- ggplot(mrppValues, aes(x=minCodesPerSite, y=effect)) + geom_line() + ggtitle("effect size")
g4 <- ggplot(mrppValues, aes(x=minCodesPerSite, y=pvalue)) + geom_line() + ggtitle("p-value") + scale_y_continuous(limits=c(0,1))

pdf("multiMprrDrTN.pdf", width=10, height=10)
grid.arrange(g1,g2,g3,g4,ncol=2)
dev.off()

