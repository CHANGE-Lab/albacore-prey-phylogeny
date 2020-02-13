################# 1. A plot that colours species by class 
### Current Issues:
##### a) poorly coloured segments
##### b) not reproducbile ATM
######## Wish List
############ i) Coloured branches and segments - so as to look like 'prey_tree_classcollegend.pdf'


#apparent problem is that there are species in the tree that aren't in the data, namely these ones:
tree_names %>% 
  filter(sort.my_tree_class.tip.label. %notin% my_prey_class$PreySP)

##this is splitting the prey according to class
my_prey_t1 = my_prey
my_prey_t1$PreySP <- gsub("_", " ", my_prey_t1$PreySP)
rownames(my_prey_t1) = my_prey_t1$PreySP
prey_class_info<-split(x=my_prey_t1$PreySP,f=my_prey_t1$class)
#using that split prey to group the species in the tree by class
my_tree_t1 = my_tree
my_tree_t1$tip.label = gsub("_", " ", my_tree_t1$tip.label)
my_tree_class<-groupOTU(my_tree_t1,prey_class_info)

#creating the tree, colored by class
brewer.pal(n = 11, name = "Spectral") 
pal = c()
pal[1] = 'grey40'; pal[2] = '#0047ab';pal[3] = '#751308'
pal[4] = '#4B0082'; pal[5] = '#F05E23';pal[6] = '#013220';pal[7] = '#B80F0A'
tree3_black <- ggtree(my_tree_class, aes(color=group), layout = 'circular') +
  scale_colour_manual('Class', aesthetics = c('colour', 'fill'), values = pal,
                      breaks = c("Actinopterygii","Branchiopoda","Cephalopoda","Gastropoda","Hexanauplia","Malacostraca"),
                      labels = c("Actinopterygii","Branchiopoda","Cephalopoda","Gastropoda","Hexanauplia","Malacostraca")) + 
  geom_tiplab(size = 2, show.legend = FALSE) +
  theme(panel.background = element_rect(fill = 'black', colour = NA),
        plot.background = element_rect(fill = 'black', colour = 'black'),
        panel.border = element_blank(),
        legend.background = element_rect(fill = 'black'),
        legend.text = element_text(colour = 'white'),
        legend.key = element_rect(fill = 'black'))
  #scale_size(range=c(1, 2), guide=FALSE) +
  #theme(legend.key = element_rect(colour = fill, fill = fill, size = 0.5, linetype='solid'), 
        #legend.key.height = unit(.2, 'cm'))+
  #guides(linetype = guide_legend(override.aes = list(size = 0.2)))
tree3_black
task1finalplot_black = tree3_black
ggsave('species_by_class_black.png', task1finalplot_black, dpi = 500, width = 10, height = 7.5)

tree3_white <- ggtree(my_tree_class, aes(color=group), layout = 'circular') +
  scale_colour_manual('Class', aesthetics = c('colour', 'fill'), values = pal,
                      breaks = c("Actinopterygii","Branchiopoda","Cephalopoda","Gastropoda","Hexanauplia","Malacostraca"),
                      labels = c("Actinopterygii","Branchiopoda","Cephalopoda","Gastropoda","Hexanauplia","Malacostraca")) + 
  geom_tiplab(size = 2, show.legend = FALSE) +
  theme(panel.background = element_rect(fill = 'white', colour = NA),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_blank(),
        legend.background = element_rect(fill = 'white'),
        legend.text = element_text(colour = 'black'),
        legend.key = element_rect(fill = 'white'))
#scale_size(range=c(1, 2), guide=FALSE) +
#theme(legend.key = element_rect(colour = fill, fill = fill, size = 0.5, linetype='solid'), 
#legend.key.height = unit(.2, 'cm'))+
#guides(linetype = guide_legend(override.aes = list(size = 0.2)))
tree3_white
task1finalplot_white = tree3_white
ggsave('species_by_class_white.png', task1finalplot_white, dpi = 500, width = 10, height = 7.5)






