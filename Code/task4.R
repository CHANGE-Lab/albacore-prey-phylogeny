################# 4. A plot that shows categorical or other trait values on complimentary graph to 3)
### Current Issues:
##### a) data not piping into figure ?
##### b) all grey boxes where trait values should be ?
######## Wish List -- Fig. 7.4 from treedata book

#replacing the spaces with an underscore so the prey_species in the dataset match those in the tree data
my_prey_traits$PreySP <- gsub(" ", "_", my_prey_traits$PreySP)

rownames(my_prey_traits) <- my_prey_traits$PreySP
#creating a separate dataframe for the trophic level data
#for whatever reason it does not work properly when I try to 
#create the heatmap for trophic level, even though it is the same method as I used for the other data columns
trophic_level<- as.data.frame(my_prey_traits[ , 11])
rownames(trophic_level) <- my_prey_traits$PreySP
names(trophic_level) <- "Trophic Level"
#the selection of the 4th column of data is the species, which is used to pipe the other data onto the tree
p1 <- gheatmap(mycirc, my_prey_traits[, c(4,6)], offset=-0.04, width=0.10,
               colnames_angle=95, colnames_offset_y = .25, colnames = F) +
  scale_fill_viridis_d(name = "Vertical Habitat", breaks = c("benthic", "demersal", "benthopelagic",
                                                             "epipelagic", "mesopelagic", "bathypelagic"),
                       limits = c("benthic", "demersal", "benthopelagic", "epipelagic",
                                  "mesopelagic", "bathypelagic"))
p2 <- p1 + new_scale_fill()
p3 <- gheatmap(p2, my_prey_traits[ , c(4,7)], offset=0.01, width=0.10,
               colnames_angle=95, colnames_offset_y = .25, colnames = F) +
  scale_fill_viridis_d(name = "Horizontal Habitat", option = "C",
                       breaks = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"),
                       limits = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"))
p4 <- p3 + new_scale_fill()
p5 <- gheatmap(p4, my_prey_traits[ ,c(4,12)], offset=0.06, width=0.10,
               colnames_angle=95, colnames_offset_y = .25, colnames = F) +
  scale_fill_viridis_d(name = "Body Shape", option = 'magma',
                       breaks = c("eel-like", "elongated", "fusiform ", "globiform","compressiform", "depressiform", "unique"),
                       limits = c("eel-like", "elongated", "fusiform ", "globiform","compressiform", "depressiform", "unique"))
p6 <- p5 +new_scale_fill()
p7 <- gheatmap(p6, my_prey_traits[ ,c(4,8)], offset=0.11, width=0.10,
               colnames_angle=95, colnames_offset_y = .25, colnames = F) +
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B",
                       breaks = c("0", "1", "UN"),
                       limits = c("0", "1", "UN"))
p8 <- gheatmap(p7, my_prey_traits[ ,c(4,9)], offset=0.16, width=0.10,
               colnames_angle=95, colnames_offset_y = .25, colnames = F)+
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B",
                       breaks = c("0", "1", "UN"),
                       limits = c("0", "1", "UN"))
p9 <- gheatmap(p8, my_prey_traits[ ,c(4,10)], offset=0.21, width=0.10,
               colnames_angle=95, colnames_offset_y = .25, colnames = F) +
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B",
                       breaks = c("0","1","UN"),
                       limits = c("0","1","UN"))
p10 <- p9 + new_scale_fill()
p11 <- gheatmap(p10, trophic_level, offset=0.31, width=0.05,
                colnames_angle=95, colnames_offset_y = .25, colnames = F) +
  scale_fill_gradientn(name = "Trophic Level", colours = c('grey90', 'purple4'), na.value = 'white') +
  theme(legend.key.size = unit(2,'mm'),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 5),
        legend.spacing = unit(0.02,'cm'),
        legend.position = c(0.99,0.5))
p11

