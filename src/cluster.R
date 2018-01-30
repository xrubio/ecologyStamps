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
codes.md <- meandist(vegdist(presence, method='jaccard'), sites$province)

################# dendrogram

pdf("dendrogram.pdf", width=12, height=12)
plot(codes.md)
dev.off()    

############## with ggtree?

source("https://bioconductor.org/biocLite.R")
biocLite("ggtree")

library(ggtree)
library(ape)

codeCluster <- nj(as.dist(codes.md))

# node ids
ggtree(codeCluster, branch.length="none") + geom_text2(aes(label=node), hjust=-.3) + geom_tiplab()
# 17 -> mediterranean, 22 -> limes


pdf("cladogram.pdf", width=11, height=11)
p1 <- ggtree(codeCluster, layout="circular", branch.length="none") + geom_hilight(node=17, fill="indianred2", extend=4, alpha=0.2) + geom_tiplab2() + geom_cladelabel(node=17, label="Mediterranean", offset=4, geom="label", fill="indianred2", alpha=0.4, offset.text=0.5, barsize=1, fontsize=5) + geom_cladelabel(node=22, label="Northern limes", offset=4, geom="label", fill="palegreen4", alpha=0.4, offset.text=3.5, barsize=1, fontsize=5) + geom_hilight(node=22, fill="palegreen4", extend=4, alpha=0.2) 
#p1 <- p1 + theme(panel.border = element_blank(),panel.background = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
p1
dev.off()



################# distance matrix
get_lower_tri <- function(cormat)
{
    cormat[upper.tri(cormat)]<- NA
    return(cormat)
}

reorder_cormat <- function(cormat)
{
    # Use correlation between variables as distance
    dd <- as.dist(cormat)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
}

meltedCodes <- reorder_cormat(codes.md)
meltedCodes <- melt(get_lower_tri(meltedCodes))

# paint distance matrix as quantiles to avoid linear palette colors
qn = quantile(meltedCodes$value, c(0.01, 0.99), na.rm = TRUE)
qn01 <- rescale(c(qn, range(meltedCodes$value)))
meltedCodes$Var2 <- with(meltedCodes,factor(Var2,levels = rev(sort(unique(Var2)))))

pdf("distances.pdf", width=11, height=7)    
ggplot(meltedCodes, aes(x=Var1, y=Var2, fill=value, label=round(value,2))) + geom_raster()+ geom_text(col="grey20")  + theme_bw() + scale_fill_gradientn(colours=c("indianred2", "steelblue2", "cornsilk"), values = c(0, seq(qn01[1], qn01[2], length.out = 18), 1.01), breaks=c(0.925,0.94,0.96,0.98,0.999), labels=c(0.92,0.94,0.96,0.98,1), limits=c(0.922, 1), na.value="transparent") + theme(panel.border=element_blank(), legend.position = "right", legend.direction="vertical", legend.title=element_blank(), legend.key.width=unit(0.02, "npc"), legend.key.height=unit(0.1, "npc"), axis.ticks.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_text(angle=45, hjust=0)) + xlab("") + ylab("") + scale_x_discrete(position="top")
dev.off()



############### average distance 

results <- getMrpp(sites, presence)

resultsDF <- data.frame(numSites=results$mrpp$n, distance=results$mrpp$classdelta, province=attr(results$mrpp$n, "names"))

svg("distanceGroups.svg", width=8, height=7)    
ggplot(resultsDF, aes(y=reorder(province, -distance), x=distance, size=numSites)) + geom_point(col="skyblue3") + geom_vline(xintercept=results$mrpp$delta, col="indianred2") + scale_x_continuous(limits=c(0.85,1)) + theme_bw() + annotate("label", label="mean group distance", x=0.97, y=9.2, colour="white", fill="indianred2", fontface="bold") + theme(legend.position="bottom")
dev.off()

