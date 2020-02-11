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
p1_4 <- gheatmap(mycirc, my_prey_traits[, c(4,6)], offset=-0.04, width=0.10,font.size=2,
               colnames_angle=95, colnames_offset_y = .25, colnames = F) +
  scale_fill_viridis_d(name = "Vertical Habitat", breaks = c("benthic", "demersal", "benthopelagic",
                                                             "epipelagic", "mesopelagic", "bathypelagic"),
                       limits = c("benthic", "demersal", "benthopelagic", "epipelagic",
                                  "mesopelagic", "bathypelagic"), na.translate = TRUE)
p2_4 <- p1_4 + new_scale_fill()
p3_4 <- gheatmap(p2_4, my_prey_traits[ , c(4,7)], offset=0.01, width=0.10,font.size=2,
               colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Horizontal Habitat", option = "C",
                       breaks = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"),
                       limits = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"),
                       na.translate = TRUE)
p4_4 <- p3_4 + new_scale_fill()
p5_4 <- gheatmap(p4_4, my_prey_traits[ ,c(4,12)], offset=0.06, width=0.10,font.size=2,
               colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Body Shape", option = 'magma',
                       breaks = c("eel-like", "elongated", "fusiform ", "globiform","compressiform", "depressiform", "unique"),
                       limits = c("eel-like", "elongated", "fusiform ", "globiform","compressiform", "depressiform", "unique"))
p6_4 <- p5_4 +new_scale_fill()
p7_4 <- gheatmap(p6_4, my_prey_traits[ ,c(4,8)], offset=0.11, width=0.10,font.size=2,
               colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B",
                       breaks = c("0", "1", "UN"),
                       limits = c("0", "1", "UN"))
p8_4 <- gheatmap(p7_4, my_prey_traits[ ,c(4,9)], offset=0.16, width=0.10,font.size=2,
               colnames_angle=-85, colnames_offset_y = 4.5, colnames = F)+
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B",
                       breaks = c("0", "1", "UN"),
                       limits = c("0", "1", "UN"))
p9_4 <- gheatmap(p8_4, my_prey_traits[ ,c(4,10)], offset=0.21, width=0.10,font.size=2,
               colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B",
                       breaks = c("0","1","UN"),
                       limits = c("0","1","UN"))
p10_4 <- p9_4 + new_scale_fill()
p11_4 <- gheatmap(p10_4, trophic_level, offset=0.31, width=0.05, font.size=2,
                colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_gradientn(name = "Trophic Level", colours = c('grey90', 'purple4'), na.value = 'white') +
  theme(legend.key.size = unit(3,'mm'),
        legend.text = element_text(size = 6.5),
        legend.title = element_text(size = 6.5),
        legend.spacing = unit(0.02,'cm'),
        legend.position = c(0.99,0.5)) +
  annotate('text', x = 1.37, y = 5.5, label = 'Trophic Level', angle = -85, size = 2)+
  annotate('text', x = 1.32, y = 5.5, label = 'Diel Migrant', angle = -85, size = 2)+
  annotate('text', x = 1.27, y = 5.5, label = 'Refuge', angle = -85, size = 2)+
  annotate('text', x = 1.22, y = 5.8, label = 'Physical Defence', angle = -85, size = 2)+
  annotate('text', x = 1.17, y = 5.5, label = 'Body Shape', angle = -85, size = 2)+
  annotate('text', x = 1.12, y = 6.9, label = 'Horizontal Habitat', angle = -85, size = 2)+
  annotate('text', x = 1.07, y = 6.5, label = 'Vertical Habitat', angle = -85, size = 2)

task4finalplot = p11_4
ggsave('categorical_trait_values.png', task4finalplot, dpi = 300, width = 10, height = 7.5)



##Habitat traits plot

p1_5 <- gheatmap(mycirc, my_prey_traits[, c(4,6)], offset=-0.04, width=0.10,font.size=2,
                 colnames_angle=95, colnames_offset_y = .25, colnames = F) +
  scale_fill_viridis_d(name = "Vertical Habitat", direction = -1, breaks = c("benthic", "demersal", "benthopelagic",
                                                             "epipelagic", "mesopelagic", "bathypelagic"),
                       limits = c("benthic", "demersal", "benthopelagic", "epipelagic",
                                  "mesopelagic", "bathypelagic"), na.translate = TRUE, option = 'D')
p2_5 <- p1_5 + new_scale_fill()
p3_5 <- gheatmap(p2_5, my_prey_traits[ , c(4,7)], offset=0.01, width=0.10,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Horizontal Habitat", option = "D", direction = -1,
                       breaks = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"),
                       limits = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"),
                       na.translate = TRUE)
p4_5 <- p3_5 + new_scale_fill()

p5_5 <- gheatmap(p4_5, my_prey_traits[ ,c(4,8)], offset=0.06, width=0.10,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge", option = "D",
                    breaks = c("0", "1", "UN"),
                    limits = c("0", "1", "UN"),
                    begin = 0.25, end = 0.95)
p6_5 <- gheatmap(p5_5, my_prey_traits[ ,c(4,9)], offset=0.11, width=0.10,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F)+
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge", option = "D",
                    breaks = c("0", "1", "UN"),
                    limits = c("0", "1", "UN"),
                    begin = 0.25, end = 0.95)+
  annotate('text', x = 1.17, y = 5.5, label = 'Diel Migrant', angle = -85, size = 2.8)+
  annotate('text', x = 1.22, y = 5.5, label = 'Refuge', angle = -85, size = 2.8)+
  annotate('text', x = 1.12, y = 6.9, label = 'Horizontal Habitat', angle = -85, size = 2.8)+
  annotate('text', x = 1.07, y = 6.5, label = 'Vertical Habitat', angle = -85, size = 2.8)
habitat_traits_finalplot = p6_5
ggsave('habitat_trait_values.png', habitat_traits_finalplot, dpi = 300, width = 10, height = 7.5)
?scale_fill_viridis_d
#morphology/trophic plot
p1_6 <- gheatmap(mycirc, my_prey_traits[ ,c(4,12)], offset=-0.04, width=0.10,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Body Shape", option = 'A',
                       breaks = c("eel-like", "elongated", "fusiform ", "globiform","compressiform", "depressiform", "unique"),
                       limits = c("eel-like", "elongated", "fusiform ", "globiform","compressiform", "depressiform", "unique"))
p2_6 <- p1_6 + new_scale_fill()
p3_6 <- gheatmap(p2_6, my_prey_traits[ ,c(4,10)], offset=0.01, width=0.10,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Physical Defense", option = "A",
                       breaks = c("0","1"),
                       limits = c("0","1"),
                       begin = 0.25, end = 0.80)
p4_6 <- p3_6 + new_scale_fill()
p5_6 <- gheatmap(p4_6, trophic_level, offset=0.11, width=0.05, font.size=2,
                  colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_gradientn(name = "Trophic Level", colours = c('grey90', 'purple4'), na.value = 'white') +
#  theme(legend.key.size = unit(3,'mm'),
#        legend.text = element_text(size = 6.5),
#        legend.title = element_text(size = 6.5),
#        legend.spacing = unit(0.02,'cm'),
#        legend.position = c(0.99,0.5)) +
  annotate('text', x = 1.17, y = 6.4, label = 'Trophic Level', angle = -85, size = 2.8)+
  annotate('text', x = 1.12, y = 6.7, label = 'Physical Defence', angle = -85, size = 2.8)+
  annotate('text', x = 1.07, y = 6.4, label = 'Body Shape', angle = -85, size = 2.8)
  

morph_troph_finalplot = p5_6
ggsave('Morphology_Trophic_values.png', morph_troph_finalplot, dpi = 300, width = 10, height = 7.5)


