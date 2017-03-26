
library(reshape2)
library(ggplot2)
library(vegan)
library(gridExtra)
library(ggdendro)
library(plyr)
library(pwr)

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
# all
#presence <- read.csv("../09_presence/presenceAll.csv", header=T, sep=';')
#counts <- read.csv("../07_countSample/countsAll.csv", header=T, sep=";")
#locations <- read.csv("../04_locations/locations.csv", header=T, sep=";")

# dr 20
#presence <- read.csv("../09_presence/presenceDr.csv", header=T, sep=';')
#counts <- read.csv("../07_countSample/countsDr.csv", header=T, sep=";")
#locations <- read.csv("../04_locations/locationsDr.csv", header=T, sep=";")

# family
presence <- read.csv("../09_presence/presenceFamily.csv", header=T, sep=';')
counts <- read.csv("../07_countSample/countsFamily.csv", header=T, sep=";")
locations <- read.csv("../04_locations/locationsFamily.csv", header=T, sep=";")

# family Dr.20
#presence <- read.csv("../09_presence/presenceFamilyDr.csv", header=T, sep=';')
#counts <- read.csv("../07_countSample/countsFamilyDr.csv", header=T, sep=";")
#locations <- read.csv("../04_locations/locationsFamilyDr.csv", header=T, sep=";")

counts$province <- locations$province
countsWithProvince <-  subset(counts, province != '')


mrppValues <- data.frame(delta=double(), effect=double(), pvalue=double(), minCodesPerSite=integer(), numSites=integer(), power=numeric())
for(i in 1:100)
{
    results <- getMrpp(countsWithProvince, presence, i)

    # statistical power computation
    sites <- subset(countsWithProvince, numCodes >= i)
    numProvinces = length(unique(sites$province))
    meanSitesPerProvince = mean(count(sites$province)$freq)
    power = pwr.anova.test(k=numProvinces, n=meanSitesPerProvince, f=results[2], sig.level=results[3])$power 
    mrppValues<- rbind(mrppValues, data.frame(delta=results[1], effect=results[2], pvalue=results[3], minCodesPerSite=i, numSites=results[4], power=power))
}

# print best value

mrppValuesBelowP <- subset(mrppValues, pvalue<0.05)
mrppValuesBelowP[which.max(mrppValuesBelowP$effect),]


# multi MRPP results
g1 <- ggplot(mrppValues, aes(x=minCodesPerSite, y=numSites)) + geom_line(size=1, col="goldenrod1") + xlab("") + ylab("number of sites") +  annotate("label", label="sample size", x=95, y=500, colour="white", fill="goldenrod1", fontface="bold") + theme_bw()
g2 <- ggplot(mrppValues, aes(x=minCodesPerSite, y=delta)) + geom_line(size=1, col="indianred2")  + xlab("") + ylab("delta") + annotate("label", label="mean group distance", x=90, y=0.1, colour="white", fill="indianred2", fontface="bold") + theme_bw() + scale_y_continuous(limits=c(0,1))
g3 <- ggplot(mrppValues, aes(x=minCodesPerSite, y=effect)) + geom_line(size=1, col="palegreen4") + xlab("") + ylab("distance means") + annotate("label", label="effect", x=95, y=0.003, colour="white", fill="palegreen4", fontface="bold") + theme_bw()
g4 <- ggplot(mrppValues, aes(x=minCodesPerSite, y=pvalue)) + geom_line(size=1, col="skyblue3") + xlab("n. stamps (threshold)") + ylab("p-value")+ annotate("label", label="significance", x=95, y=0.6, colour="white", fill="skyblue3", fontface="bold") + geom_hline(yintercept=0.05, col="grey50", size=1, linetype="twodash") +  theme_bw() + scale_y_continuous(limits=c(0,1))

# power
#g5 <- ggplot(mrppValues, aes(x=minCodesPerSite, y=power)) + geom_line(size=1, col="skyblue3") + xlab("n. stamps (threshold)") + ylab("p-value")+ annotate("label", label="power", x=95, y=0.6, colour="white", fill="skyblue3", fontface="bold") + geom_hline(yintercept=0.05, col="grey50", size=1, linetype="twodash") +  theme_bw() + scale_y_continuous(limits=c(0,1))

#svg("multiMprrFamily.svg", width=10, height=10)
pdf("multiMprrFamily.pdf", width=10, height=10)
grid.arrange(g1,g2,g3,g4,ncol=1, top="MRPP for all stamps")
dev.off()



