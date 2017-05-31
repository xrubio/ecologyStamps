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


### Exploratory Data Analysis of dataset

library(ggplot2)
library(plyr)


######### AMPHORAE TYPES

stamps <- read.csv("../data/stamps.csv", header=T, sep=";")
types <- count(stamps$type)
mostFrequent <- subset(types, freq>100)
# remove amphora incerta
mostFrequent <- subset(mostFrequent, x!="Amphora incerta")    
# remove tegula
mostFrequent <- subset(mostFrequent, x!="Tegula")

pdf("types.pdf", width=8, height=9)
ggplot(mostFrequent, aes(x=reorder(x, freq), y=freq)) + geom_bar(stat="identity", fill="indianred2", col="indianred4") + theme_bw() + xlab("type") + ylab("number of stamps") + coord_flip()
dev.off()

########## FREQUENCY DISTRIBUTIONS

sites <- read.table("../data/sites.csv", header=T, sep=";")
presence <-  as.matrix(read.csv("../data/presence.csv", row.names = 1, header=T, sep=';'))

sites$numCodes <- rowSums(presence)


############## FREQUENCY DISTRIBUTION OF CODES PER SITE
   
pdf("freqs.pdf", width=6, height=4)
ggplot(sites, aes(x=numCodes)) + geom_histogram(binwidth=5, fill="indianred2", col="indianred4") + theme_bw() + xlab("number of stamps") + ylab("number of sites")
dev.off()


############## NUMBER OF CODES PER SITE AND PROVINCE
codesPerProvince <- aggregate(sites$numCodes, by=list(province=sites$province), FUN=sum)

pdf("stampsPerProvince.pdf", width=8, height=10)    
ggplot(sites, aes(y=reorder(province, numCodes, FUN=sum), x=numCodes, fill=province)) + geom_jitter(col="grey50", alpha=0.5, shape=21, height=0.2, width=0.3, size=3) + scale_colour_manual(values=myPalette) + theme_bw() + theme(legend.position="None") + xlab("number of code stamps") + ylab("province")
dev.off()


count(sites$province)
    
