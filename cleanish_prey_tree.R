# install.packages("taxize")
# install.packages("ape")
# install.packages("rotl")
# install.packages("ggimage")
# install.packages("ggstance")
# install.packages("phangorn")
# install.packages("geiger")
# install.packages("phylobase")
# install.packages("devtools")
# install.packages("ggnewscale")
# 
# library(devtools)
# install_github("GuangchuangYu/ggtree")
# install_github("liamrevell/phytools")
# install_github("YuLab-SMU/treeio")
# 
# 
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("Biostrings")

library(tidyverse)
library(ggtree)
library(taxize)
library(rotl)
library(ape)
library(ggimage)
library(ggstance)
library(Biostrings)
library(phytools)
library(phangorn)
library(geiger)
library(treeio)
library(phylobase)
library(ggnewscale)

my_tree <- read.tree("albacore_diet_tree")

# name a tree object
mycirc <- ggtree(my_tree, layout = "circular")

# load my data
my_prey <- read_csv("Prey_list_fo.csv")
#I am replacing the space in the names to an underscore so it matches the names that 
#are in my_tree. I'm not sure if this was an issue when you did it, but it is a start
my_prey$PreySP <- gsub(" ", "_", my_prey$PreySP) 
#View(my_prey)



#this is directly from the troubleshoot code and I'm not 100% sure what the initial purpose was
#because whenever I ran it the fo data were NAs. But I am keeping it and inserting another 
#column with the species name, with the same name as the one in my_prey, so I can merge them later
#This fixes the dimension issue
my_prey2 = my_prey[my_tree$tip.label,2:3] #takes the species values that equal the remaining sp that were parsed to the tree
#and just selecting the columns of data
View(my_prey2)
nrow(my_prey2) #234 #problem partly solved!
rownames(my_prey2) <- my_tree$tip.label # relabel rows to match species
my_prey2$PreySP <- my_tree$tip.label

#here I am merging the two data.frames above, this should create a data.frame only with 
#the species that match between the two data.frame. I then made the species names the row names
#and removed all the columns other than avg and max fo
my_prey3 <- merge(my_prey, my_prey2, by = "PreySP")
rownames(my_prey3) <- my_prey3[, 1]
my_prey3 <- my_prey3[,2:3]
nrow(my_prey3) #n=218  this is less than there were in what we were trying before(234). I have
#not looked into this yet but I am putting this document together to see what you guys think.
#I suspect there are extra species in my_prey as I think you assumed. But also I think
#there might be species in my_tree$tip.label that are not the same as in my_prey
#but I didn't make the datasets or look into this much yet, so it is all speculation
View(my_prey3)

#the heatmap now has some colour on it. There are the blanks where missing species are, but I think this is a start
p1 <- gheatmap(mycirc, my_prey3, offset=0, width=0.5,
         colnames_angle=95, colnames_offset_y = .25)

p1
