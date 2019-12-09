#Final Plot Code - Only to be edited with pull requests

#libraries
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
`%notin%` = Negate(`%in%`)
#demo line
#TASKS

################# 1. A plot that colours species by class 
### Current Issues:
##### a) poorly coloured segments
##### b) not reproducbile ATM
######## Wish List
############ i) Coloured branches and segments - so as to look like 'prey_tree_classcollegend.pdf'

################# 2. A plot that colours species by maximum frequency of occurence - largely done
### Current Issues:
##### a) Nothing explicit, but needs some tweeks, to be re-visited at some point
######## Wish List -- TBD

################# 3. A plot that shows the different frequency of occurence of prey from ocean basins
### Current Issues:
##### a) data not piping into figure
##### b) all grey boxes where trait values should be
######## Wish List -- Fig. 7.4 from treedata book

################# 4. A plot that shows categorical or other trait values on complimentary graph to 3)
### Current Issues:
##### a) data not piping into figure ?
##### b) all grey boxes where trait values should be ?
######## Wish List -- Fig. 7.4 from treedata book

###### Cleaning Code

#data load/fix
my_tree <- read.tree("albacore_diet_tree") #make initial tree object
mycirc <- ggtree(my_tree, layout = "circular") #turn tree into circular
my_prey <- read_csv("Prey_list_fo.csv") #load prey data

my_tree_class = my_tree 
my_tree_class$tip.label <- gsub("_", " ", my_tree_class$tip.label) #for ease of plotting take away _
tree_names = data.frame(sort(my_tree_class$tip.label))
prey_names = data.frame(sort(my_prey$PreySP))
nrow(tree_names); nrow(prey_names) #okay there are some discrepancies
names_not_in_tree = prey_names %>% 
  filter(sort.my_prey.PreySP. %notin% tree_names$sort.my_tree_class.tip.label.) 
names_not_in_prey =  tree_names %>% 
  filter(sort.my_tree_class.tip.label. %notin% prey_names$sort.my_prey.PreySP.)

###################################### IMPORTANT NOTE #########################################
##  if we look at the two 'names_not_in_...' above, we can see that a lot of them are just
##  different spellings of each other -- attempting to fix below -- TASH TO DOUBLE CHECK THIS
#things I'm guessing are the same (index of names_..._prey,names_..._tree)
# 1,1 - 2,2 - 4,4 - 5,6 - 6,7 - 7,9 - 11,13 - 13,15 - 16,21
#so since 'names_..._tree' are the names from the prey, I'm going to change the prey names to match the tree ones
#then exclude any in the prey that just aren't in the tree at all
my_prey_class_keep = my_prey %>% 
  filter(PreySP %notin% names_not_in_tree$sort.my_prey.PreySP.)
my_prey_class_fix = my_prey %>% 
  filter(PreySP %in% names_not_in_tree$sort.my_prey.PreySP.)
my_prey_class_fix = my_prey_class_fix[order(my_prey_class_fix$PreySP),]
x = my_prey_class_fix #rename for easier writing this next bit
y = as.vector(names_not_in_prey$sort.my_tree_class.tip.label.) #getting this into easier format for reassigning

#gonna do this manually so easy to change specific ones in case Tash wants something to be different
x[1,1] = y[1]; x[2,1] = y[2]; x[12,1] = y[3]; x[4,1] = y[4]; x[6,1] = y[5]; x[7,1] = y[6]; x[9,1] = y[7]; 
x[16,1] = y[8]; x[17,1] = y[9]; x[10,1] = y[10]; x[13,1] = y[11]; x[19,1] = y[12]; x[15,1] = y[13];  
x[18,1] = y[14]; x[3,1] = y[15]; x[21,1] = y[16]

my_prey_class = rbind(my_prey_class_keep, x)
my_prey_class = my_prey_class %>% 
  filter(PreySP %in% my_tree_class$tip.label)
nrow(my_prey_class);length(my_tree_class$tip.label) #okay so problem is fixed

my_prey = my_prey_class #note reassignment here - if something different required, don't run or reload from other script

# 1. A plot that colours species by class 


