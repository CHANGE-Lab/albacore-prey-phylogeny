######################################### Tree Data Visualisation Tutorials Extras #######################################

#### all the packages ----

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

#### ggtree example 3: multiple circular matrices of data ----


## need tips = nrow(df)
#df2 <- as.data.frame(matrix(rnorm(39), ncol=3))
#rownames(df2) <- my_tree$tip.label
#colnames(df2) <- LETTERS[2:3]

#load data
nwk <- system.file("extdata", "sample.nwk", package="treeio")
tree <- read.tree(nwk)
#specify tree layout (not this is for further work on a circular tree layout)
circ <- ggtree(tree, layout = "circular")

#associated dummy data
df <- data.frame(first=c("a", "b", "a", "c", "d", "d", "a", "b", "e", "e", "f", "c", "f"),
                 second= c("z", "z", "z", "z", "y", "y", "y", "y", "x", "x", "x", "a", "a"))
rownames(df) <- tree$tip.label

# can replace this section with my species prey list + associated data, as I have row names + data

df2 <- as.data.frame(matrix(rnorm(39), ncol=3)) 
# randomly generates matrix with three cols and values from a normal distribution
rownames(df2) <- tree$tip.label # relabel rows to match species
colnames(df2) <- LETTERS[1:3] # relabel columns to what we want cols to be  
#note we don't need to do this with my data, because I select whole columns including their header
View(df2)

p1 <- gheatmap(circ, df, offset=.8, width=.2,
               colnames_angle=95, colnames_offset_y = .25) +
  scale_fill_viridis_d(option="D", name="discrete\nvalue")
p1

p2 <- p1 + new_scale_fill()
gheatmap(p2, df2, offset=15, width=.3,
         colnames_angle=90, colnames_offset_y = .25) +
  scale_fill_viridis_c(option="A", name="Continuous")
# I don't need both of these right now
# Something up with this code see tutorial for ggtree section 7.
p2

# Need my albacore prey tree
my_tree <- read.tree("albacore_diet_tree")

# name a tree object
mycirc <- ggtree(my_tree, layout = "circular")

# load my data
my_prey <- read.csv("Prey_list_fo.csv", header = TRUE)

# manipulate associated data
summary(my_prey)

#we have a data frame dimension issue when plotting our data values onto the tree
nrow(my_prey) == nrow(my_tree$tip.label) #logical(0) = NO
nrow(my_prey) #239
nrow(my_tree$tip.label) #234
#Need to fix nrow issue

#Could parse my tip.labels to an object if desired
#sp_parsed <- my_tree$tip.label
#rownames(my_tree$tip.label) #there are no row names, just a vector of data
#View(my_tree$tip.label)

#This fixes the dimension issue
my_prey2 = my_prey[my_tree$tip.label,2:3] #takes the species values that equal the remaining sp that were parsed to the tree
#and just selecting the columns of data
View(my_prey2)
nrow(my_prey2) #234 #problem partly solved!
rownames(my_prey2) <- my_tree$tip.label # relabel rows to match species
#just double-checking
rownames(my_prey2) == my_tree$tip.label # all TRUE, all good.
#colnames(my_prey2) already imported from my_prey db

#Note I need an object containing only columns of data, and species need to be assigned to row names
str(df2)
str(my_prey2)
View(df2)
View(my_prey2)
#comparing both prey + data matrix
View(tree)
View(my_tree)
#The only difference I can see in that structure is that my_tree has node labels and theirs doesn't?

# Just mapping continuous data onto my tree
p1 <- gheatmap(mycirc, my_prey2, offset=0, width=0.5,
               colnames_angle=95, colnames_offset_y = .25) #+ #I can't get the scale fill to work!!! It's turning up grey.
#scale_color_gradientn(colours = c("steelblue4","steelblue3",
#                                  "coral", "coral1",
#                                  "firebrick2", "firebrick3", "firebrick4"), 
#                      name = "average %FO")
#scale_fill_viridis_c(option="A", name="Percent FO")
#scale_color_viridis_c(option = "D", name="Percent FO", aesthetics = "fill")

p1

#modifying their second portion to see if it works
p2 <- gheatmap(circ, df2, offset=15, width=.3,
               colnames_angle=90, colnames_offset_y = .25) +
  scale_fill_viridis_c(option="A", name="Continuous")
p2

p4 <- gheatmap(mycirc, my_prey2, offset=0, width=.5,
               colnames_angle=90, colnames_offset_y = .25) +
  #scale_fill_gradientn(colours = c("steelblue4","steelblue3","coral", "coral1", "firebrick2", "firebrick3", "firebrick4"), 
  #                                 name = "average %FO")
  scale_fill_viridis_c(option="plasma", name="continuous\nvalue")
p4

dev.copy2pdf(file="preyprelim_heattree.pdf", width=30, height=30)


# WHY????