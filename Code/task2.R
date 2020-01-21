################# 2. A plot that colours species by maximum frequency of occurence - largely done
### Current Issues:
##### a) Nothing explicit, but needs some tweeks, to be re-visited at some point
######## Wish List -- TBD

my_tree <- read.tree("albacore_diet_tree")

# name a tree object
mycirc <- ggtree(my_tree, layout = "circular")

my_prey_maxfo <- as.data.frame(my_prey$max_fo)
rownames(my_prey_maxfo) <- my_prey$PreySP

prey_tree_maxfo <- gheatmap(mycirc, my_prey_maxfo, offset=0, width=0.15,
         colnames_angle=95, colnames_offset_y = .25,colnames = F) + 
  scale_fill_viridis_c(name='Maximum Frequency\nof Occurance(%)')


