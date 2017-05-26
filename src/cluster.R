# Copyright (c) 2017 - Xavier Rubio-Campillo 
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version
#
# The source code is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#   
# You should have received a copy of the GNU General Public 
# License along with this library.  If not, see <http://www.gnu.org/licenses/>.


#### hierarchical clustering for a specific threshold


library(reshape2)
library(ggplot2)    
library(scales)

source("mrppBase.R")

minCodes <- 20

allSites <- read.table("../data/sites.csv", header=T, sep=";")
allPresence <-  as.matrix(read.csv("../data/presence.csv", row.names = 1, header=T, sep=';'))

result <- filterByNumCodes(allSites, allPresence, minCodes)
result <- filterByMultiSite(result$sites, result$presence)

sites = result$site    
presence = result$presence

################# dendrogram

codes.md <- meandist(vegdist(presence, method='jaccard'), sites$province)

pdf("dendrogram.pdf", width=12, height=12)
plot(codes.md)
dev.off()    

############## with ggtree?
library(ggtree)
    
codeCluster <- hclust(as.dist(codes.md), method="average")
phyloStamp <- as.phylo(codeCluster)
ggtree(phyloStamp)    


################# distance matrix

meltedCodes <- melt(codes.md)
# paint distance matrix as quantiles to avoid linear palette colors
qn = quantile(meltedCodes$value, c(0.01, 0.99), na.rm = TRUE)
qn01 <- rescale(c(qn, range(meltedCodes$value)))

pdf("distances.pdf", width=17, height=9)    
ggplot(meltedCodes, aes(x=Var1, y=Var2, fill=value, label=round(value,2))) + geom_raster() + geom_text(col="grey90", fontface="bold")  + theme_bw()+ scale_fill_gradientn(colours=c("indianred2", "skyblue3", "grey60"), values = c(0, seq(qn01[1], qn01[2], length.out = 18), 1)) + theme(panel.border=element_blank(), legend.position="none", axis.ticks.y=element_blank(), axis.ticks.x=element_blank()) + xlab("") + ylab("") + scale_x_discrete(position="top")
dev.off()

############### average distance 

results <- getMrpp(sites, presence)

resultsDF <- data.frame(numSites=results$mrpp$n, distance=results$mrpp$classdelta, province=attr(results$mrpp$n, "names"))

svg("distanceGroups.svg", width=8, height=7)    
ggplot(resultsDF, aes(y=reorder(province, -distance), x=distance, size=numSites)) + geom_point(col="skyblue3") + geom_vline(xintercept=results$mrpp$delta, col="indianred2") + scale_x_continuous(limits=c(0.85,1)) + theme_bw() + annotate("label", label="mean group distance", x=0.97, y=9.2, colour="white", fill="indianred2", fontface="bold") + theme(legend.position="bottom")
dev.off()

