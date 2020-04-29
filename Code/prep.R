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
library(viridis)
library(PNWColors)
library(RColorBrewer)
library(here)
library(cowplot)
library(gtable)
"%notin%" = Negate('%in%')
here::here()


my_tree <- read.tree(here("Data/albacore_diet_tree"))

# name a tree object
mycirc <- ggtree(my_tree, layout = "circular")

# load data
my_prey <- read_csv(here("Data/Prey_list_fo.csv"))
my_prey_traits <- read.csv(here("Data/prey_traits_phylo.csv"))


##All of the below is from task 3, this is to create the "my_prey" dataframe, in which all
#of the prey species in the table match exactly what is on the tree
my_prey$PreySP <- gsub(" ", "_", my_prey$PreySP)

my_tree_class = my_tree 
my_prey$PreySP <- gsub(" ", "_", my_prey$PreySP) #for ease of plotting take away " "
tree_names = data.frame(sort(my_tree_class$tip.label))
prey_names = data.frame(sort(my_prey$PreySP))
nrow(tree_names); nrow(prey_names) #okay there are some discrepancies
names_not_in_tree = prey_names %>% 
  filter(sort.my_prey.PreySP. %notin% tree_names$sort.my_tree_class.tip.label.) 
names_not_in_prey =  tree_names %>% 
  filter(sort.my_tree_class.tip.label. %notin% prey_names$sort.my_prey.PreySP.)

my_prey_class_keep = my_prey %>% 
  filter(PreySP %notin% names_not_in_tree$sort.my_prey.PreySP.)
my_prey_class_fix = my_prey %>% 
  filter(PreySP %in% names_not_in_tree$sort.my_prey.PreySP.)
my_prey_class_fix = my_prey_class_fix[order(my_prey_class_fix$PreySP),]
x = my_prey_class_fix #rename for easier writing this next bit
y = as.vector(names_not_in_prey$sort.my_tree_class.tip.label.) #getting this into easier format for reassigning

x[1,1] = y[1]; x[2,1] = y[2]; x[12,1] = y[3]; x[4,1] = y[4]; x[6,1] = y[5]; x[7,1] = y[6]; x[9,1] = y[7]; 
x[16,1] = y[8]; x[17,1] = y[9]; x[10,1] = y[10]; x[13,1] = y[11]; x[19,1] = y[12]; x[15,1] = y[13];  
x[18,1] = y[14]; x[3,1] = y[15]; x[21,1] = y[16]

my_prey_class = rbind(my_prey_class_keep, x)
my_prey_class = my_prey_class %>% 
  filter(PreySP %in% my_tree_class$tip.label)
nrow(my_prey_class);length(my_tree_class$tip.label) #okay so problem is fixed

my_prey = as.data.frame(my_prey_class) #note reassignment here - if something different required, don't run or reload from other script

