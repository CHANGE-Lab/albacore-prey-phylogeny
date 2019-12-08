########## PHYLO TREE ###########

#### Set-up workspace ----
getwd()
setwd("/Users/natasha/Documents/POSTDOC/TUNA DIETS/TUNASTATS/data")

install.packages("taxize")
install.packages("ape")
install.packages("rotl")
install.packages("ggimage")
install.packages("ggstance")
install.packages("phangorn")
install.packages("geiger")
install.packages("phylobase")

library(devtools)
install_github("GuangchuangYu/ggtree")
install_github("liamrevell/phytools")
install_github("YuLab-SMU/treeio")


if (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")
BiocManager::install("Biostrings")

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
library(stringr)

packageVersion("ape")


#### TRIAL %FO & Class data ####
#M Savoca code trial

# DATA ----
#data contains sp list, max %FO and average %FO
my_prey <- read.csv("Prey_list_fo.csv", header = TRUE)
str(my_prey)

# TREE BUILD ----

#Building the basic tree

# Tree build 1 ----
breaks <- c(seq(1,nrow(my_prey),50),nrow(my_prey)+1)  # why are we doing this?
#looking up each of the species in dataset to a phylogeny, doing it by sets of 50
#if there's an NA (species that didn't match a value in known phylogeny), breaks the whole chunk of 50 species

for (i in 1:(length(breaks)-1)){
  taxa <- as.character(my_prey$PreySP[breaks[i]:(breaks[i+1]-1)])
  taxa <- taxa[taxa != "" & !is.na(taxa)]
  
  resolved_namest <- tnrs_match_names(taxa)                          # I think this is where all the extra species fall out
  resolved_namest <- resolved_namest[!is.na(resolved_namest$unique_name),]  # ignore an NA
  if (i==1){
    resolved_namess <- resolved_namest
  } else {
    resolved_namess <- rbind(resolved_namess, resolved_namest)
  }
}
resolved_names <- resolved_namess
resolved_names <- resolved_names[resolved_names$flags!="INCERTAE_SEDIS_INHERITED",]

# Tree 1 ----
#original tree based on simple taxon datset
my_tree <- tol_induced_subtree(ott_ids = resolved_names$ott_id, label_format = "name")

my_tree$tip.label<-gsub("_"," ",my_tree$tip.label) # removes underscore between genus and species names
my_tree$tip.label<-str_extract(my_tree$tip.label, "[A-Z][a-z]+ [a-z]+")

my_tree <- compute.brlen(my_tree, method = "Grafen", power = 1/2) #add branch lengths to my tree using the Grafen (1989) method
my_tree <- ladderize(my_tree, right = TRUE)

View(my_tree)

## NOTES ----
# A lot of troubleshooting later and I found that the species list needs to go on the far left side of the data, 
# all other data/factors need to be added to the RHS of the species list
# My new tree (with added Class & Family vectors) is skipping one species for some reason and parsing to NA...

# SAVE TREE ----
#write.tree(my_tree, "albacore_diet_tree")
#write.tree(my_tree2, "albacore_diet_tree2")

# PLOTTING TREES ----

# reload trees ----
# Problem with new tree - use old tree.
my_tree <- read.tree("albacore_diet_tree")

#checking my_prey data
str(my_prey)
View(my_tree)

# dataframe dimensions issue? ----
# according to Matt's code dataframe dimension should not be an issue when parsing data to tip labels...
# Try dimension solution
#we have a data frame dimension issue when plotting our data values onto the tree
nrow(my_prey) == nrow(my_tree$tip.label) #logical(0) = NO
nrow(my_prey) #239
#nrow(my_tree$tip.label) #233
#Need to fix nrow issue

#This fixes the dimension issue
my_prey2 = my_prey[my_tree$tip.label,] #takes the species values that equal the remaining sp that were parsed to the tree
#and just selecting the columns of data
str(my_prey2)

# Tree plot 1 ----
NH_base <- ggtree(my_tree, layout="fan", open.angle=0) + #can vary width with size=1.5
  #geom_text2(aes(label=label), hjust=-.2, size=4) +
  #geom_tiplab2()+
  ggplot2::xlim(-0.6, 1.3) 
NH_base

my_tree$tip.label <- as.factor(my_tree$tip.label)

# Base tree with thicker tree ----
NH_base2 <- ggtree(my_tree, layout="fan", open.angle=0, size=2.5) +
  #geom_text2(aes(label=label), hjust=-.2, size=4) +
  #geom_tiplab2()+
  ggplot2::xlim(-0.6, 1.3) 
NH_base2

dev.copy2pdf(file="prey_tree_basicthick.pdf", width=30, height=30)

#Basic tree with tip labels
NH_base %<+% my_prey + 
  geom_tiplab2(aes(label = paste0("italic('", label, "')"), angle = angle), 
               parse = TRUE, align = TRUE, size = 6)

dev.copy2pdf(file="prey_tree_basic.pdf", width=30, height=30)
#Fix NA manually in Illustrator?

#Basic tree with thick tree + labels
NH_base2 %<+% my_prey2 + 
  geom_tiplab2(aes(label = paste0("italic('", label, "')"), angle = angle), 
               parse = TRUE, align = TRUE, size = 6)

dev.copy2pdf(file="prey_tree_basiclabthick.pdf", width=30, height=30)
#Fix NA manually in Illustrator?

#Basic tree with tip labels + with parsed prey data
#Adds underscore to species names back in!!! WHY
NH_base %<+% my_prey + #works for my_prey & my_prey2 (subset of my_prey #233 species)
  geom_tiplab2(aes(label = paste0("italic('", label, "')"), angle = angle), 
               parse = TRUE, align = TRUE, size = 6)

dev.copy2pdf(file="prey_tree_basic.pdf", width=30, height=30)
#Fix NA manually in Illustrator

unique(my_prey2$class) #<NA> appears to have been added in here?

unique(my_prey$class)


# Colours used in Illustrator
#Actinopterygii = #0049E9
#Cephalopoda = #8600E9

# Colour by class ----
# Not working well
NH_base %<+% my_prey + 
    #aes(color = class) +
  geom_tiplab2(aes(label = paste0("italic('", label, "')"),
                   color=class, angle = angle), parse = TRUE, 
               align = TRUE, size = 6) +
  scale_colour_manual(values = c("#3366FF", "#CC6633", "#663366", 
                                 "#FF9933", "#663300", "#CC0000"),
                      name = "Class") #+
  #theme(legend.position = c(0.5,0.5),
  #      legend.key.size = unit(1.75, "cm"),
  #      legend.text = element_text(size = 18),
  #      legend.title = element_text(size = 20))

dev.copy2pdf(file="prey_tree_classcolours.pdf", width=30, height=30)

# %FO plot ----
#Matt second code
NH_base %<+% my_prey + 
  aes(color = max_fo) +
  geom_tiplab2(aes(label = paste0("italic('", label, "')"),
                   color=max_fo, angle = angle), parse = TRUE,
               size = 6, align = FALSE, hjust = -0.05) +
  geom_tippoint(aes(color = max_fo, size = max_fo)) +
  #scale_color_gradientn(colours = c("steelblue4",
  #                                  "gray40",
  #                                  "coral", "coral1",
  #                                  "firebrick2", "firebrick3", "firebrick4"),
  #                      name = "Max %FO observed") +
  scale_colour_viridis_c(option = "A", alpha = 1, begin = 0.2, end = 0.8, name = "Max %FO observed")+
  #scale_size(range = c(3, 7)) +
  scale_size_continuous(guide = FALSE, range = c(3, 7)) +
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(2.5, "cm"),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 26),
        legend.box = "horizontal") +
  guides(shape = guide_legend(override.aes = list(size = 5)))

# This plot also not working now, and we lose all the colours.

dev.copy2pdf(file="prey_tree_maxfoviridis.pdf", width=25, height=25)

# Adding trait information ----
NH_FO = NH_base %<+% my_prey + 
  aes(color = max_fo) +
  #geom_tiplab2(aes(label = paste0("italic('", label, "')"), #taking out species labels
  #                 color=max_fo, angle = angle), parse = TRUE,
  #             size = 6, align = FALSE, hjust = -0.05) +
  geom_tippoint(aes(color = max_fo, size = max_fo)) +
  #scale_color_gradientn(colours = c("steelblue4",
  #                                  "gray40",
  #                                  "coral", "coral1",
  #                                  "firebrick2", "firebrick3", "firebrick4"),
  #                      name = "Max %FO observed") +
  scale_colour_viridis_c(option = "A", alpha = 1, begin = 0.2, end = 0.8, name = "Max %FO observed")+
  #scale_size(range = c(3, 7)) +
  scale_size_continuous(guide = FALSE, range = c(3, 7)) +
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(2.5, "cm"),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 26),
        legend.box = "horizontal") +
  guides(shape = guide_legend(override.aes = list(size = 5)))

dev.copy2pdf(file="prey_tree_maxfonolabs.pdf", width=25, height=25)

# Adding trait information _inverted image
#NH_FO = 

NH_base2 %<+% my_prey2 + 
  aes(color = max_fo) +
  geom_tiplab2(aes(label = paste0("italic('", label, "')"), #taking out species labels
                   color=max_fo, angle = angle), parse = TRUE,
               size = 6, align = FALSE, hjust = -0.05) +
  geom_tippoint(aes(color = max_fo, size = max_fo)) +
  #scale_color_gradientn(colours = c("steelblue4",
  #                                  "gray40",
  #                                  "coral", "coral1",
  #                                  "firebrick2", "firebrick3", "firebrick4"),
  #                      name = "Max %FO observed") +
  scale_colour_viridis_c(option = "A", alpha = 1, begin = 0.8, end = 0.2, name = "Max %FO observed")+
  #scale_size(range = c(3, 7)) +
  scale_size_continuous(guide = FALSE, range = c(3, 7)) +
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(4, "cm"),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.box = "horizontal") +
  guides(shape = guide_legend(override.aes = list(size = 5)))

dev.copy2pdf(file="prey_tree_maxfonolabs.pdf", width=25, height=25)

# Adding trait information, currently not working...
NH_heat = NH_base + new_scale_fill()
gheatmap(NH_base, my_prey2$class, offset=.8, width=.2,
         colnames_angle=95, colnames_offset_y = .25) +
  scale_fill_viridis_d(option="D", name="discrete\nvalue")



#
p + geom_facet(panel = "Trait", data = my_prey, geom = ggstance::geom_barh, 
             aes(x = dummy_bar_value, color = location, fill = location), 
             stat = "identity", width = .6) +
  theme_tree2(legend.position=c(.05, .85))
p


# other options

# first plot try, vertical layout with data ----
p_stand <- ggtree(my_tree) + 
  #geom_text2(aes(label=label), hjust=-.2, size=4) +
  ggplot2::xlim(0, 1.2)
p_stand

# Adding data
p_stand %<+% d_sp_sum +
  aes(color = Sp_mean) +
  geom_tiplab(aes(color=Sp_mean), align = TRUE, size = 3) +
  scale_color_gradientn(colours = c("#053061" ,"#2166AC", "#4393C3", "#F4A582", "#D6604D", "#B2182B", "#67001F"), 
                        name = "Proportion with \nplastic") +
  geom_hilight(node=1, fill="gold") + 
  theme(legend.position = c(0.2,0.8))


p2 <- facet_plot(p_stand, panel = 'Prop w plastic', data = d_sp_sum, 
                 geom = ggstance::geom_barh, 
                 mapping = aes(x = Sp_mean), 
                 stat='identity') 
p2

geom_facet(
  
  inset(my_tree, d_sp_sum$Sp_mean, width=0.2, height=0.15, hjust=-1)
  
  
  legend.title = element_blank(), # no title
  legend.key = element_blank()) # no keys



geom_tippoint(aes(color=order))



geom_tiplab(aes(color = Sp_mean))


p %<+% d + geom_text(aes(color=, label=label), hjust=-0.5)


#### Other trials ####

## visualize the tree 
t <- ggtree(tuna_tree)
t

t2 <- t %<+% PreyDB3
t2

t3 <- t2 + geom_tiplab(offset = .6, hjust = .5) +
  geom_tippoint(aes(shape = Habitat, color = Habitat 
                    #, size = mass_in_kg
                    )) + 
  theme(legend.position = "right") + scale_size_continuous(range = c(3, 10))
t3


TunaT_trial <- ggtree(tuna_tree, layout="circular")
TunaT_trial

## attach the sampling information data set 
## and add symbols colored by location
Tun <- t %<+% PreyDB3 + geom_tippoint(aes(color=Habitat))
Tun

## visualize SNP and Trait data using dot and bar charts,
## and align them based on tree structure
p + geom_facet(panel = "SNP", data = snp_data, geom = geom_point, 
               mapping=aes(x = pos, color = location), shape = '|') +
  geom_facet(panel = "Trait", data = bar_data, geom = ggstance::geom_barh, 
             aes(x = dummy_bar_value, color = location, fill = location), 
             stat = "identity", width = .6) +
  theme_tree2(legend.position=c(.05, .85))
p


#### Grouping issue ----
# See section 6.4: https://yulab-smu.github.io/treedata-book/chapter6.html#group-taxa-vis


# need to extract tip.label factor as vector and do anti join with my original
# solved

# Extra code ----

#not sure what this code was for, can check with Matt
#my_tree2 = phylo4d(my_tree, d_sp_sum)

# ggtree TRY ----

#geom_tippoint()
#facet_plot()
#geom_facet() --> add %FO as bar plot that is coloured with

#scenario
#colour species by habitat + tippoint size varies by %FO