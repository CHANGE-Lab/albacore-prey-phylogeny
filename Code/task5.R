################# 5. Get representative graphics of the species groups and plop them on the tree
### NOTE: data is from the phylogpic.com(?) website, there will be species missing 
### NOTE: the 'how-to' is from the manual that Tash sent before 
### NOTE: do it by order 

mycircs<-ggtree(my_tree)##not circular 
mycirc<-ggtree(my_tree, layout='circular')##circular 

##I chose class for now, to see how it works then will scale up to orders
##these codes are from the website url
#for example: http://phylopic.org/image/44a3628d-aafd-45cc-97a6-1cb74bd43dec/
#the .png code from the URL is the .png code I used for Malacostraca below

#Malacostraca = 44a3628d-aafd-45cc-97a6-1cb74bd43dec
#Cephalopoda = fb420ad9-4d60-401f-92f1-d340d8cd8a95
#Actinopterygii = bba1800a-dd86-451d-a79b-c5944cfe5231
#Gastropoda = 9da7781e-48eb-407d-a13f-d6de8954dde2
#Hexanauplia = c5dbd85a-c4be-4990-a369-c830ad23cb22
#Branchiopoda = 51ba7d73-24fb-4bf5-9839-f2b3257ee2f3


##right now the pics are at arbitary nodes, I just wanted to see if I could get it to work
#creating the datapoints with the image codes on it, anda node associated to each
phylopic_infor <- data.frame(node = c(270,300,320,340,350,360),
                            phylopic = c('44a3628d-aafd-45cc-97a6-1cb74bd43dec',
                                         "fb420ad9-4d60-401f-92f1-d340d8cd8a95",
                                         "bba1800a-dd86-451d-a79b-c5944cfe5231",
                                         "9da7781e-48eb-407d-a13f-d6de8954dde2",
                                         "c5dbd85a-c4be-4990-a369-c830ad23cb22",
                                         "51ba7d73-24fb-4bf5-9839-f2b3257ee2f3"))
##piping the phylopic silhouette onto the try
mycircs %<+% phylopic_infor + 
  geom_nodelab(aes(image=phylopic),geom="phylopic", alpha=1, color='steelblue')

mycirc %<+% phylopic_infor + 
  geom_nodelab2(aes(image=phylopic),geom="phylopic", alpha=1, color='steelblue')
## I cannot seem to get the silhouettes to pipe onto the tree if it is a circular layour
## geom_node_lab seems to be for a normal tree whereas geom_node_lab2 seems to be for circular trees
##but they silhouettes still do not show up


