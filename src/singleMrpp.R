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


#### MRPP results for the entire dataset

source("mrppBase.R")

sites <- read.table("../data/sites.csv", header=T, sep=";")
presence <-  as.matrix(read.csv("../data/presence.csv", row.names = 1, header=T, sep=';'))

results <- getMrpp(sites, presence)
print(results$mrpp)

