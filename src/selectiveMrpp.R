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


#### MRPP for the rolling threshold 

library(ggplot2)
library(gridExtra)


source("mrppBase.R")

# multi mrpp
sites <- read.table("../data/sites.csv", header=T, sep=";")
presence <-  as.matrix(read.csv("../data/presence.csv", row.names = 1, header=T, sep=';'))

multiMrpp <- rollingMrpp(sites, presence, 100)

########### figure multi mrpp

g1 <- ggplot(multiMrpp, aes(x=minCodesPerSite, y=numSites)) + geom_line(size=1, col="goldenrod1") + xlab("") + ylab("number of sites") + theme_bw()
g2 <- ggplot(multiMrpp, aes(x=minCodesPerSite, y=delta)) + geom_line(size=1, col="indianred2")  + xlab("") + ylab(expression(delta)) + theme_bw() + scale_y_continuous(limits=c(0,1))
g3 <- ggplot(multiMrpp, aes(x=minCodesPerSite, y=effect)) + geom_line(size=1, col="palegreen4") + xlab("Minimum Number of Codes per site)") + ylab("A") + theme_bw()

#pdf("multiMprrFamily.pdf", width=10, height=10)
grid.arrange(g1,g2,g3,ncol=1, top="MRPP for all stamps")
#dev.off()
g1 <- ggplot(multiMrpp, aes(x=minCodesPerSite, y=numSites)) + geom_line(size=1, col="skyblue3", alpha=0.5) + xlab("") + ylab("number of sites") + theme_bw() + geom_point(col="skyblue3", size=2)
g2 <- ggplot(multiMrpp, aes(x=minCodesPerSite, y=delta)) + geom_line(size=1, col="indianred2", alpha=0.5)  + xlab("") + ylab(expression(delta)) + theme_bw() + scale_y_continuous(limits=c(0,1)) + geom_point(col="indianred2", size=2)
g3 <- ggplot(multiMrpp, aes(x=minCodesPerSite, y=effect)) + geom_line(size=1, col="palegreen4", alpha=0.5) + xlab("Minimum Number of Codes per site)") + ylab("A") + theme_bw() + geom_point(col="palegreen4", size=2)+ scale_y_continuous(limits=c(0,0.03)) 

pdf("selective.pdf", width=10, height=7)
grid.arrange(g1,g2,g3,ncol=1)
dev.off()

