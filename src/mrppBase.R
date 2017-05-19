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


#### Basic functions 

library(vegan)
library(plyr)

# filter sites by minCodes before performing Mrpp
filterByMultiSite <- function(allSites, allPresence,minCodes)
{    
    provincesMultiSite = subset(count(allSites$province), freq>2)$x
    sites = subset(allSites, province %in% provincesMultiSite)
    presence <- subset(allPresence, rownames(allPresence) %in% sites$id)
    # remove code if no site has the code    
    presence <- presence[,colSums(presence) != 0]
    return(list("sites"=sites, "presence"=presence))
}

# perform mrpp on all sites from provinces with multisite
getMrpp <- function(allSites, allPresence)
{
    values <- filterByMultiSite(allSites, allPresence)
    sites = values$site
    presence = values$presence

    result <- mrpp(presence, sites$province, distance="jaccard", weight.type=1)
    return(list("sites" = sites, "presence" = presence, "mrpp" = result))
}


# filter sites by minCodes before performing Mrpp
filterByNumCodes <- function(allSites, allPresence,minCodes)
{
    print(minCodes)
    sites <- subset(allSites, rowSums(allPresence)>=minCodes)  
    presence <- subset(allPresence, rownames(allPresence) %in% sites$id)

    return(list("sites"=sites, "presence"=presence))
}

# rolling mrpp
rollingMrpp <- function(allSites, allPresence, maxValue)
{
    mrppValues <- data.frame(delta=double(), effect=double(), pvalue=double(), minCodesPerSite=integer(), numSites=integer())
    for(i in 1:maxValue)
    {
        filtered <- filterByNumCodes(allSites, allPresence, i)
        resultsMrpp <- getMrpp(filtered$sites, filtered$presence)  
        results <- c(resultsMrpp$mrpp$delta, resultsMrpp$mrpp$A, resultsMrpp$mrpp$Pvalue, nrow(resultsMrpp$sites))
        mrppValues<- rbind(mrppValues, data.frame(delta=results[1], effect=results[2], pvalue=results[3], minCodesPerSite=i, numSites=results[4]))
    }
    return(mrppValues)
}

