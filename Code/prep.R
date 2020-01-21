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

my_tree <- read.tree("Data/albacore_diet_tree")

# name a tree object
mycirc <- ggtree(my_tree, layout = "circular")

# load my data
my_prey <- read_csv("Data/Prey_list_fo.csv")

my_prey_traits <- read.csv("Data/prey_traits_maxfo.csv")
