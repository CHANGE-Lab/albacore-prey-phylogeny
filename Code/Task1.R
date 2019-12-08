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
         unique(my_prey$class)
         