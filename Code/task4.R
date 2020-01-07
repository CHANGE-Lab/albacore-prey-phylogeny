################# 4. A plot that shows categorical or other trait values on complimentary graph to 3)
### Current Issues:
##### a) data not piping into figure ?
##### b) all grey boxes where trait values should be ?
######## Wish List -- Fig. 7.4 from treedata book

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


my_tree <- read.tree("albacore_diet_tree")

# name a tree object
mycirc <- ggtree(my_tree, layout = "circular")

# load my data
my_prey <- read_csv("Prey_list_fo.csv")

my_prey_traits <- read.csv("prey_traits_maxfo.csv")

my_prey_traits$PreySP <- gsub(" ", "_", my_prey_traits$PreySP)

rownames(my_prey_traits) <- my_prey_traits$PreySP


sapply(my_prey_traits, class)
vert_habitat <- as.data.frame(my_prey_traits[ , 6])
rownames(vert_habitat) <- my_prey_traits$PreySP
names(vert_habitat) <- "Vertical Habitat"

horz_habitat<- as.data.frame(my_prey_traits[ , 7])
rownames(horz_habitat) <- my_prey_traits$PreySP
names(horz_habitat) <- "Horizontal Habitat"

diel_migrant<- as.data.frame(my_prey_traits[ , 8])
rownames(diel_migrant) <- my_prey_traits$PreySP
names(diel_migrant) <- "Diel Migrant"

refuge<- as.data.frame(my_prey_traits[ , 9])
rownames(refuge) <- my_prey_traits$PreySP
names(refuge) <- "Refuge"
refuge$Refuge <- as.factor(refuge$Refuge)

phys_def<- as.data.frame(my_prey_traits[ , 10])
rownames(phys_def) <- my_prey_traits$PreySP
names(phys_def) <- "Physical Defense"
phys_def$`Physical Defense` <- as.factor(phys_def$`Physical Defense`)

trophic_level<- as.data.frame(my_prey_traits[ , 11])
rownames(trophic_level) <- my_prey_traits$PreySP
names(trophic_level) <- "Trophic Level"

body_shape<- as.data.frame(my_prey_traits[ , 12])
rownames(body_shape) <- my_prey_traits$PreySP
names(body_shape) <- "Body Shape"

p1 <- gheatmap(mycirc, vert_habitat, offset=0, width=0.08,
         colnames_angle=95, colnames_offset_y = .25) +
  scale_fill_viridis_d(name = "Vertical Habitat", breaks = c("benthic", "demersal", "benthopelagic", 
                                                             "epipelagic", "mesopelagic", "bathypelagic"),
                       limits = c("benthic", "demersal", "benthopelagic", "epipelagic", 
                                  "mesopelagic", "bathypelagic")) 

p2 <- p1 + new_scale_fill()
p3 <- gheatmap(p2, horz_habitat, offset=0.08, width=0.08,
               colnames_angle=95, colnames_offset_y = .25) +
  scale_fill_viridis_d(name = "Horizontal Habitat", option = "C", 
                       breaks = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"),
                       limits = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"))
p4 <- p3 + new_scale_fill()

p5 <- gheatmap(p4, body_shape, offset=0.16, width=0.08,
               colnames_angle=95, colnames_offset_y = .25) +
  scale_fill_viridis_d(name = "Body Shape", option = 'magma', 
                       breaks = c("eel-like", "elongated", "fusiform ", "globiform","compressiform", "depressiform", "unique"),
                       limits = c("eel-like", "elongated", "fusiform ", "globiform","compressiform", "depressiform", "unique"))

p6 <- p5 +new_scale_fill()

p7 <- gheatmap(p6, diel_migrant, offset=0.24, width=0.08,
               colnames_angle=95, colnames_offset_y = .25) +
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B")

p8 <- gheatmap(p7, refuge, offset=0.32, width=0.08,
               colnames_angle=95, colnames_offset_y = .25)+
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B")

p9 <- gheatmap(p8, phys_def, offset=0.40, width=0.08,
               colnames_angle=95, colnames_offset_y = .25) +
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B")

p10 <- p9 + new_scale_fill()

p11 <- gheatmap(p10, trophic_level, offset=0.48, width=0.08,
               colnames_angle=95, colnames_offset_y = .25) +
  scale_fill_viridis_c(name = "Trophic Level", option = 'A', na.value = 'white')
p11





