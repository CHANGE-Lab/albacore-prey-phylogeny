################# 4. A plot that shows categorical or other trait values on complimentary graph to 3)
### Current Issues:
##### a) data not piping into figure ?
##### b) all grey boxes where trait values should be ?
######## Wish List -- Fig. 7.4 from treedata book

##note: make sure to run all of the prep.R as this code needs it

#replacing the spaces with an underscore so the prey_species in the dataset match those in the tree data
my_prey_traits$prey_sp <- gsub(" ", "_", my_prey_traits$prey_sp)

#######some name inconsistencies between tree and this database#######
my_prey_traits_name<-my_prey_traits %>% 
  select(prey_sp)

not_in_tree<-my_prey_traits_name %>% 
  filter(prey_sp %notin% tree_names$sort.my_tree_class.tip.label.)

not_in_prey_traits<-tree_names %>% 
  filter(sort.my_tree_class.tip.label. %notin% my_prey_traits_name$prey_sp)

##we can see the following names are synonymous

#prey_traits_phylo                    tree

#Ancistroteuthis_lichtensteini	      Ancistroteuthis_lichtensteinii  1
#Axius_stirhynchus                   	Axius_stirynchus                2
#Chiroteuthis_veranii	                Chiroteuthis_veranyi            4
#Clupea_pallasii_pallasii	            Clupea_pallasii                 5
#Electrona_rissoi	                    Electrona_risso                 6
#Liocranchia_reinhardti	              Liocranchia_reinhardtii         10
#Mullus_barbatus_barbatus	            Mullus_barbatus                 12
#Neognathophausia_gigas	              Gnathophausia_gigas             7
#Neognathophausia_ingens	            Gnathophausia_ingens            8
#Scopelogadus_mizolepis_bispinosus	  Scopelogadus_bispinosus         15

#these are also the same species, but under different names

#Leuroglossus_stilbius	              Bathylagus_stilbius             3
#Onykia_robusta	                      Moroteuthis_robusta             11
#Berryteuthis_anonychus	              Okutania_anonycha               14

##changing the names to 'z' and 'w' for ease of typing this part

z = my_prey_traits
z$prey_sp<-as.character(z$prey_sp)
w = as.vector(not_in_prey_traits$sort.my_tree_class.tip.label.)

z[186,'prey_sp'] = w[1]; z[221,'prey_sp'] =w[2]; z[2,'prey_sp'] =w[3]; z[149,'prey_sp'] =w[4]; z[20,'prey_sp'] =w[5]
z[39,'prey_sp'] =w[6]; z[238,'prey_sp'] =w[7]; z[239,'prey_sp'] =w[8]; z[165,'prey_sp'] =w[9]; z[154,'prey_sp'] =w[10]; 
z[189,'prey_sp'] =w[11]; z[83,'prey_sp'] =w[12]; z[48,'prey_sp'] =w[13]; z[159,'prey_sp'] =w[14]; z[116,'prey_sp'] =w[15]

z$prey_sp<-as.factor(z$prey_sp)
my_prey_traits = z

rownames(my_prey_traits) <- my_prey_traits$prey_sp

sapply(my_prey_traits,class)##need to make the columns of 0/1 from integer to factor for piping to tree
my_prey_traits[,c("diel_migrant","refuge","season_migrant","phys_defense","transparent",
                  "col_disrupt","countershade","photophore_PA")]<-
  lapply(my_prey_traits[,c("diel_migrant","refuge","season_migrant","phys_defense","transparent",
                           "col_disrupt","countershade","photophore_PA")],as.factor)
sapply(my_prey_traits,class)##there, should work now

 ########phylogenetic trees##########
general_phylo_iter1 <- gheatmap(mycirc, my_prey_traits[,"vert_habitat",drop=FALSE], offset=0, width=0.05,font.size=2,
               colnames_angle=95, colnames_offset_y = .25, colnames = F) +
  scale_fill_viridis_d(name = "Vertical Habitat", breaks = c("benthic", "demersal",
                                                             "epipelagic", "mesopelagic", "bathypelagic"),
                       limits = c("benthic", "demersal", "epipelagic",
                                  "mesopelagic", "bathypelagic"), na.translate = TRUE)
general_phylo_iter1.5 <- general_phylo_iter1 + new_scale_fill()
general_phylo_iter2 <- gheatmap(general_phylo_iter1.5, my_prey_traits[ , "horz_habitat",drop=FALSE], offset=0.05, width=0.05,font.size=2,
               colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Horizontal Habitat", option = "C",
                       breaks = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"),
                       limits = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"),
                       na.translate = TRUE)
general_phylo_iter2.5 <- general_phylo_iter2 + new_scale_fill()
general_phylo_iter3 <- gheatmap(general_phylo_iter2.5, my_prey_traits[ , "body_shape",drop=FALSE], offset=0.10, width=0.05,font.size=2,
               colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Body Shape", option = 'magma',
                       breaks = c("eel-like", "elongated", "fusiform", "globiform","compressiform", "depressiform", "unique"),
                       limits = c("eel-like", "elongated", "fusiform", "globiform","compressiform", "depressiform", "unique"))
general_phylo_iter3.5 <- general_phylo_iter3 +new_scale_fill()
general_phylo_iter4 <- gheatmap(general_phylo_iter3.5, my_prey_traits[ ,"diel_migrant", drop=FALSE], offset=0.15, width=0.05,font.size=2,
               colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B",begin = 0,end=0.5,
                       breaks = c("0", "1"),
                       limits = c("0", "1"))
general_phylo_iter5 <- gheatmap(general_phylo_iter4, my_prey_traits[ , "refuge",drop=FALSE], offset=0.20, width=0.05,font.size=2,
               colnames_angle=-85, colnames_offset_y = 4.5, colnames = F)+
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B",begin = 0,end=0.5,
                       breaks = c("0", "1"),
                       limits = c("0", "1"))
general_phylo_iter6 <- gheatmap(general_phylo_iter5, my_prey_traits[ ,"phys_defense",drop=FALSE], offset=0.25, width=0.05,font.size=2,
               colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Diel Migrant\nRefuge\nPhysical Defense", option = "B",begin = 0,end=0.5,
                       breaks = c("0","1"),
                       limits = c("0","1"))
general_phylo_iter6.5 <- general_phylo_iter6 + new_scale_fill()
general_phylo_final <- gheatmap(general_phylo_iter6.5, my_prey_traits[,"trophic_level",drop=FALSE], offset=0.30, width=0.05, font.size=2,
                colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_gradientn(name = "Trophic Level", colours = c('grey90', 'purple4'), na.value = 'white') +
  theme(legend.key.size = unit(3,'mm'),
        legend.text = element_text(size = 6.5),
        legend.title = element_text(size = 6.5),
        legend.spacing = unit(0.02,'cm'),
        legend.position = c(0.99,0.5))# +
#  annotate('text', x = 1.37, y = 5.5, label = 'Trophic Level', angle = -85, size = 2)+
#  annotate('text', x = 1.32, y = 5.5, label = 'Diel Migrant', angle = -85, size = 2)+
#  annotate('text', x = 1.27, y = 5.5, label = 'Refuge', angle = -85, size = 2)+
#  annotate('text', x = 1.22, y = 5.8, label = 'Physical Defence', angle = -85, size = 2)+
#  annotate('text', x = 1.17, y = 5.5, label = 'Body Shape', angle = -85, size = 2)+
#  annotate('text', x = 1.12, y = 6.9, label = 'Horizontal Habitat', angle = -85, size = 2)+
#  annotate('text', x = 1.07, y = 6.5, label = 'Vertical Habitat', angle = -85, size = 2)

#task4finalplot = p11_4
#ggsave('categorical_trait_values.png', task4finalplot, dpi = 300, width = 10, height = 7.5)


##########Habitat traits plot   With the new category of seasonal migration#########

habitat_traits_iter1 <- gheatmap(mycirc, my_prey_traits[, 'vert_habitat',drop=FALSE], offset= 0, width=0.05,font.size=2,
                 colnames_angle=95, colnames_offset_y = 4.5, colnames = FALSE) +
  scale_fill_viridis_d(name = "Vertical Habitat", direction = -1, breaks = c("benthic", "demersal",
                                                             "epipelagic", "mesopelagic", "bathypelagic"),
                       limits = c("benthic", "demersal", "epipelagic",
                                  "mesopelagic", "bathypelagic"), na.translate = TRUE, option = 'D',
                       guide = guide_legend(order = 1))+theme(legend.position = c(1,0.80))+
  scale_y_continuous(expand = c(0,4))+
  annotate('text', x = 1.03, y = -7, label = 'Vertical Habitat', angle = -85, size = 3)
habitat_traits_iter1.5 <- habitat_traits_iter1 + new_scale_fill()
habitat_traits_iter2 <- gheatmap(habitat_traits_iter1.5, my_prey_traits[ ,'horz_habitat',drop=FALSE], offset=0.05, width=0.05,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Horizontal Habitat", option = "D", direction = -1,
                       breaks = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"),
                       limits = c("reef-associated", "coastal", "continental shelf","continental slope", "oceanic"),
                       na.translate = TRUE, guide = guide_legend(order = 2))+theme(legend.position = c(1,0.683))+
  scale_y_continuous(expand = c(0,4))+
annotate('text', x = 1.08, y = -7.1, label = 'Horizontal Habitat', angle = -85, size = 3)#+
#  annotate('text', x = 1.07, y = 6.5, label = 'Vertical Habitat', angle = -85, size = 2.8)
habitat_traits_iter2.5 <- habitat_traits_iter2 + new_scale_fill()
habitat_traits_iter3 <- gheatmap(habitat_traits_iter2.5, my_prey_traits[ , 'diel_migrant',drop=FALSE], offset=0.10, width=0.05,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_manual(name = "Diel Migrant\nSeasonal Migrant",
                    breaks = c("0", "1"),
                    limits = c("0", "1"),
                    values = c("0"="#FDE725FF", "1"="#39568CFF"),
                    guide = guide_legend(order = 3))+theme(legend.position = c(1,0.5985))+
  scale_y_continuous(expand = c(0,4))+
  annotate('text', x = 1.13, y = -7.5, label = 'Diel Migrant', angle = -85, size = 3)
#  annotate('text', x = 1.12, y = 6.9, label = 'Horizontal Habitat', angle = -85, size = 2.8)+
#  annotate('text', x = 1.07, y = 6.5, label = 'Vertical Habitat', angle = -85, size = 2.8)
habitat_traits_iter4 <- gheatmap(habitat_traits_iter3, my_prey_traits[ , 'season_migrant',drop=FALSE], offset=0.15, width=0.05,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_manual(name = "Diel Migrant\nSeasonal Migrant",
                    breaks = c("0", "1"),
                    limits = c("0", "1"),
                    values = c("0"="#FDE725FF", "1"="#39568CFF"),
                    guide = guide_legend(order = 3))+theme(legend.position = c(1,0.5985))+
  scale_y_continuous(expand = c(0,4))+
  annotate('text', x = 1.18, y = -7.5, label = 'Seasonal Migrant', angle = -85, size = 3)
habitat_traits_iter4.5 <- habitat_traits_iter4 + new_scale_fill()
habitat_traits_finalplot <- gheatmap(habitat_traits_iter4.5, my_prey_traits[ ,'refuge',drop=FALSE], offset=0.20, width=0.05,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F)+
  scale_fill_manual(name = "Refuge",
                    breaks = c("0", "1"),
                    limits = c("0", "1"),
                    values = c("1"="#FDE725FF", "0"="#39568CFF"),
                    guide = guide_legend(order = 4))+theme(legend.position = c(1,0.531))+
  scale_y_continuous(expand = c(0,4))+
  annotate('text', x = 1.23, y = -7.5, label = 'Refuge', angle = -85, size = 3)#+
#  annotate('text', x = 1.17, y = 5.5, label = 'Diel Migrant', angle = -85, size = 2.8)#+
#  annotate('text', x = 1.12, y = 6.9, label = 'Horizontal Habitat', angle = -85, size = 2.8)#+
#  annotate('text', x = 1.07, y = 6.5, label = 'Vertical Habitat', angle = -85, size = 2.8)
# habitat_traits_iter1_noleg = p1_5 +
#   theme(legend.position = 'none')
# habitat_traits_iter2_noleg = p3_5 +
#   theme(legend.position = 'none')
# habitat_traits_iter3_noleg = p5_5 +
#   theme(legend.position = 'none')
# habitat_traits_finalplot_noleg = p6_5 +
#   theme(legend.position = 'none')
# habitat_traits_iter1_leg = get_legend(p1_5)
# habitat_traits_iter2_leg = get_legend(p3_5)
# habitat_traits_iter3_leg = get_legend(p5_5)
# habitat_traits_iterfinal_leg = get_legend(p6_5)
# 
# habitat_traits_iter1_leg_grid = cowplot::plot_grid(habitat_traits_iter1_leg, align = "v", nrow = 3)
# habitat_traits_legends = habitat_traits_iter1_leg_grid +
#   ggplot2::annotation_custom(
#     grob = legend2,
#     xmin = 0.5, xmax = 0.5, ymin = 0.55, ymax = 0.55
#   )
# habitat_traits_iter1 = ggdraw()+
#   draw_plot(habitat_traits_iter1_noleg) + 
#   draw_plot(habitat_traits_iter1_leg_grid, x=0.702, y=0.72, width=0.18, height=0.1)

ggsave('habitat_trait_iterative1.png', habitat_traits_iter1, dpi = 300, width = 10, height = 7.5)
ggsave('habitat_trait_iterative2.png', habitat_traits_iter2, dpi = 300, width = 10, height = 7.5)
ggsave('habitat_trait_iterative3.png', habitat_traits_iter3, dpi = 300, width = 10, height = 7.5)
ggsave('habitat_trait_iterative4.png', habitat_traits_iter4, dpi = 300, width = 10, height = 7.5)
ggsave('habitat_trait_final.png', habitat_traits_finalplot, dpi = 300, width = 10, height = 7.5)

##########morphology/trophic plot#########
morph_trophic_iter1 <- gheatmap(mycirc, my_prey_traits[ ,'body_shape',drop=FALSE], offset=0, width=0.05,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Body Shape", option = 'A',
                       breaks = c("eel-like", "elongated", "fusiform", "globiform","compressiform", "depressiform", "unique"),
                       limits = c("eel-like", "elongated", "fusiform", "globiform","compressiform", "depressiform", "unique"),
                       guide = guide_legend(order = 1))+
  theme(legend.position = c(1.1,0.6844))+
  scale_y_continuous(expand = c(0,3.5))+
  annotate('text', x = 1.03, y = -6.5, label = 'Body Shape', angle = -85, size = 3)
morph_trophic_iter1.5 <- morph_trophic_iter1 + new_scale_fill()
morph_trophic_iter2 <- gheatmap(morph_trophic_iter1.5, my_prey_traits[ ,'phys_defense',drop=FALSE], offset=0.05, width=0.05,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Physical Defense", option = "A",
                       breaks = c("0","1"),
                       limits = c("0","1"),
                       begin = 0.25, end = 0.80,
                       guide = guide_legend(order = 2))+
  theme(legend.position = c(1.1,0.616))+
  scale_y_continuous(expand = c(0,3.5))+
  annotate('text', x = 1.08, y = -6.5, label = 'Physical Defence', angle = -85, size = 2.8)
#  annotate('text', x = 1.07, y = 6.4, label = 'Body Shape', angle = -85, size = 2.8)
morph_trophic_iter2.5 <- morph_trophic_iter2 + new_scale_fill()
morph_trophic_final <- gheatmap(morph_trophic_iter2.5, my_prey_traits[,"trophic_level",drop=FALSE], offset=0.10, width=0.05, font.size=2,
                  colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_gradientn(name = "Trophic Level", colours = c('grey90', 'purple4'), na.value = 'white',
                       guide = guide_legend(order = 3))+
  theme(legend.position = c(1.1,0.50))+
  scale_y_continuous(expand = c(0,3.5))+
#  theme(legend.key.size = unit(3,'mm'),
#        legend.text = element_text(size = 6.5),
#        legend.title = element_text(size = 6.5),
#        legend.spacing = unit(0.02,'cm'),
#        legend.position = c(0.99,0.5)) +
annotate('text', x = 1.13, y = -6.5, label = 'Trophic Level', angle = -85, size = 2.8)
#  annotate('text', x = 1.12, y = 6.7, label = 'Physical Defence', angle = -85, size = 2.8)+
#  annotate('text', x = 1.07, y = 6.4, label = 'Body Shape', angle = -85, size = 2.8)
ggsave('Morphology_Trophic_iterative1.png', morph_trophic_iter1, dpi = 300, width = 10, height = 7.5)
ggsave('Morphology_Trophic_iterative2.png', morph_trophic_iter2, dpi = 300, width = 10, height = 7.5)
ggsave('Morphology_Trophic_values.png', morph_trophic_final, dpi = 300, width = 10, height = 7.5)

########morphology comparison plots##########
morphology_comparison_iter1 <- gheatmap(mycirc, my_prey_traits[ ,'body_shape',drop=FALSE], offset=0, width=0.05,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Body Shape", option = 'C',
                       breaks = c("eel-like", "elongated", "fusiform", "compressiform", "depressiform","globiform", "unique"),
                       limits = c("eel-like", "elongated", "fusiform", "compressiform", "depressiform","globiform", "unique"),
                       guide = guide_legend(order = 1))+
  theme(legend.position = c(1.05,0.71))+
  scale_y_continuous(expand = c(0,3.5))+
  annotate('text', x = 1.04, y = -6.5, label = 'Body Shape', angle = -85, size = 3)
morphology_comparison_iter1.5 <- morphology_comparison_iter1 + new_scale_fill()
morphology_comparison_iter2 <- gheatmap(morphology_comparison_iter1.5, my_prey_traits[ ,'b_shape_r',drop=FALSE], offset=0.05, width=0.05,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_c(name = "Body Shape Ratio", option = 'C', limits=c(0,40),direction=-1)+
  theme(legend.position = c(1.05,0.605))+
  scale_y_continuous(expand = c(0,3.5))+
  annotate('text', x = 1.095, y = -6.5, label = 'Body Shape Ratio', angle = -85, size = 3)
morphology_comparison_iter2.5 <- morphology_comparison_iter2 + new_scale_fill()
morphology_comparison_final <- gheatmap(morphology_comparison_iter2.5, my_prey_traits[ ,'eye_body_r',drop=FALSE], offset=0.10, width=0.05,font.size=2,
                 colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_c(name = "Eye-Body Ratio", option = 'D',limits=c(0,0.15),breaks=c(0.00,0.05,0.10,0.15))+
  theme(legend.position = c(1.05,0.499))+
  scale_y_continuous(expand = c(0,3.5))+
annotate('text', x = 1.15, y = -6.5, label = 'Eye-Body Ratio', angle = -85, size = 3)



ggsave('morphology_comparison_iterative1.png', morphology_comparison_iter1, dpi = 300, width = 10, height = 7.5)
ggsave('morphology_comparison_iterative2.png', morphology_comparison_iter2, dpi = 300, width = 10, height = 7.5)
ggsave('morphology_comparison_final.png', morphology_comparison_final, dpi = 300, width = 10, height = 7.5)

######### Morphology binary traits ##########

morphology_binary_iter1 <- gheatmap(mycirc, my_prey_traits[ ,'countershade',drop=FALSE], offset=0, width=0.05,font.size=2,
                                        colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Countershaded\nPhysical Defence\nPhotophore presence\nDisruptive colouration", option = 'A',
                       breaks = c("1", "0"),
                       limits = c("1", "0"),begin=0.5,end=0,
                       guide = guide_legend(order = 1))+
  scale_y_continuous(expand = c(0,4.5))+
  annotate('text', x = 1.04, y = -8.5, label = 'Countershaded', angle = -85, size = 3)
morphology_binary_iter2 <- gheatmap(morphology_binary_iter1, my_prey_traits[ ,'phys_defense',drop=FALSE], offset=0.05, width=0.05,font.size=2,
                                        colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Countershaded\nPhysical Defence\nPhotophore presence\nDisruptive colouration", option = 'A',
                       breaks = c("1", "0"),
                       limits = c("1", "0"),begin=0.5,end=0,
                       guide = guide_legend(order = 1))+
  scale_y_continuous(expand = c(0,4.5))+
  annotate('text', x = 1.09, y = -8.5, label = 'Physical Defense', angle = -85, size = 3)
morphology_binary_iter3 <- gheatmap(morphology_binary_iter2, my_prey_traits[ ,'photophore_PA',drop=FALSE], offset=0.10, width=0.05,font.size=2,
                                        colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Countershaded\nPhysical Defence\nPhotophore presence\nDisruptive colouration", option = 'A',
                       breaks = c("1", "0"),
                       limits = c("1", "0"),begin=0.5,end=0,
                       guide = guide_legend(order = 1))+
  scale_y_continuous(expand = c(0,4.5))+
  annotate('text', x = 1.14, y = -8.5, label = 'Photophore Presence', angle = -85, size = 3)
morphology_binary_final <- gheatmap(morphology_binary_iter3, my_prey_traits[ ,'col_disrupt',drop=FALSE], offset=0.15, width=0.05,font.size=2,
                                colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Countershaded\nPhysical Defence\nPhotophore presence\nDisruptive colouration", option = 'A',
                       breaks = c("1", "0"),
                       limits = c("1", "0"),begin=0.5,end=0,
                       guide = guide_legend(order = 1))+
  scale_y_continuous(expand = c(0,4.5))+
  annotate('text', x = 1.19, y = -8.5, label = 'Disruptive Colouration', angle = -85, size = 3)

ggsave('morphology_binary_final.png', morphology_binary_final, dpi = 300, width = 10, height = 7.5)

###########Behavior and Trophic Traits############

behav_trophic_iter1 <- gheatmap(mycirc, my_prey_traits[,'trophic_level',drop=FALSE], offset=0, width=0.05, font.size=2,
                                colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_gradientn(name = "Trophic Level", colours = c('grey90', 'purple4'), na.value = 'white')+
  scale_y_continuous(expand = c(0,3,5))+
  annotate('text', x = 1.04, y = -6.5, label = 'Trophic Level', angle = -85, size = 3)
behav_trophic_iter1.5 <- behav_trophic_iter1 + new_scale_fill()
behav_trophic_final <- gheatmap(behav_trophic_iter1.5, my_prey_traits[,"gregarious_primary",drop=FALSE], offset=0.05, width=0.05, font.size=2,
                                colnames_angle=-85, colnames_offset_y = 4.5, colnames = F) +
  scale_fill_viridis_d(name = "Gregariousness", direction = -1, breaks = c("solitary", "shoaling","schooling"),
                       limits = c("solitary", "shoaling","schooling"), na.translate = TRUE, option = 'magma',begin = 0.2,end = 0.8,
                       guide = guide_legend(order = 1))+
  scale_y_continuous(expand = c(0,3.5))+
  annotate('text', x = 1.09, y = -6.5, label = 'Gregariousness', angle = -85, size = 3)

ggsave('behaviour_trophic_final.png', behav_trophic_final, dpi = 300, width = 10, height = 7.5)
