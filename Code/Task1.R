################# 1. A plot that colours species by class 
### Current Issues:
##### a) poorly coloured segments
##### b) not reproducbile ATM
######## Wish List
############ i) Coloured branches and segments - so as to look like 'prey_tree_classcollegend.pdf'


#apparent problem is that there are species in the tree that aren't in the data, namely these ones:
tree_names %>% 
  filter(sort.my_tree_class.tip.label. %notin% my_prey_class$PreySP)

#my_prey$PreySP <- gsub(" ", "_", my_prey$PreySP) #replacing space in the names to matches the names in my_tree

#This fixes the dimension issue -- UNSURE WHAT THIS IS DOING?
my_prey_class = my_prey %>% 
  filter(PreySP %in% #takes species values that equal the spp that were parsed to the tree
           rownames(my_prey2) <- my_tree$tip.label # relabel rows to match species-- UNSURE WHY WE'RE NAMING THE ROWS HERE? DEPRECIATED?
         my_prey2$PreySP <- my_tree$tip.label
         
         #make initial tree
         my_prey_class = my_prey %>% 
           filter(PreySP %notin% test)#takes species values that equal the spp that were parsed to the tree
         
         class_tree <- ggtree(my_tree_class, layout="fan", open.angle=0) + #can vary width with size=1.5
           geom_tiplab2(aes(fontface = 'italic', angle = angle, ),
                        align = TRUE, size = 3) +
           #geom_tiplab2()+
           ggplot2::xlim(-0.6, 1.3) 
         class_tree
         class_tree %<+% my_prey + #note that %<+% is ggtree parlence for adding elements to the existing tree
           aes(color = class) +
           scale_colour_manual(values = c("#3366FF", "#CC6633", "#663366", 
                                          "#FF9933", "#663300", "#CC0000", 'black'),
                               name = "Class") #+
         #theme(legend.position = c(0.5,0.5),
         #      legend.key.size = unit(1.75, "cm"),
         #      legend.text = element_text(size = 18),
         #      legend.title = element_text(size = 20))
         unique(my_prey$class))
         
         
         
         
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
"%notin%" = Negate('%in%')

my_prey <- read_csv("Prey_list_fo.csv")
my_tree <- read.tree("albacore_diet_tree")

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





##this is splitting the prey according to class
prey_class_info<-split(x=my_prey$PreySP,f=my_prey$class)
#using that split prey to group the species in the tree by class
my_tree_class<-groupOTU(my_tree,prey_class_info)
#creating the tree, colored by class
tree3 <- ggtree(my_tree_class, aes(color=group), layout = 'circular')





