################# 3. A plot that shows the different frequency of occurence of prey from ocean basins
### Current Issues:
##### a) data not piping into figure
##### b) all grey boxes where trait values should be
######## Wish List -- Fig. 7.4 from treedata book
# name a tree object
mycirc <- ggtree(my_tree, layout = "circular")

my_tree_class = my_tree 
my_prey$PreySP <- gsub(" ", "_", my_prey$PreySP) #for ease of plotting take away " "
tree_names = data.frame(sort(my_tree_class$tip.label))
prey_names = data.frame(sort(my_prey$PreySP))
nrow(tree_names); nrow(prey_names) #okay there are some discrepancies
names_not_in_tree = prey_names %>% 
  filter(sort.my_prey.PreySP. %notin% tree_names$sort.my_tree_class.tip.label.) 
names_not_in_prey =  tree_names %>% 
  filter(sort.my_tree_class.tip.label. %notin% prey_names$sort.my_prey.PreySP.)

my_prey_class_keep = my_prey %>% 
  filter(PreySP %notin% names_not_in_tree$sort.my_prey.PreySP.)
my_prey_class_fix = my_prey %>% 
  filter(PreySP %in% names_not_in_tree$sort.my_prey.PreySP.)
my_prey_class_fix = my_prey_class_fix[order(my_prey_class_fix$PreySP),]
x = my_prey_class_fix #rename for easier writing this next bit
y = as.vector(names_not_in_prey$sort.my_tree_class.tip.label.) #getting this into easier format for reassigning

x[1,1] = y[1]; x[2,1] = y[2]; x[12,1] = y[3]; x[4,1] = y[4]; x[6,1] = y[5]; x[7,1] = y[6]; x[9,1] = y[7]; 
x[16,1] = y[8]; x[17,1] = y[9]; x[10,1] = y[10]; x[13,1] = y[11]; x[19,1] = y[12]; x[15,1] = y[13];  
x[18,1] = y[14]; x[3,1] = y[15]; x[21,1] = y[16]

my_prey_class = rbind(my_prey_class_keep, x)
my_prey_class = my_prey_class %>% 
  filter(PreySP %in% my_tree_class$tip.label)
nrow(my_prey_class);length(my_tree_class$tip.label) #okay so problem is fixed

my_prey = as.data.frame(my_prey_class) #note reassignment here - if something different required, don't run or reload from other script

rownames(my_prey) <- my_prey$PreySP


#the heatmap now has some colour on it. There are the blanks where missing species are, but I think this is a start
prey_tree_fo <- gheatmap(mycirc, my_prey[,2:3], offset=0, width=0.5,
               colnames_angle=95, colnames_offset_y = .25)

prey_tree_fo



##Now I will use a different database to create the same tree, but rows for each ocean basin
my_prey_basin <- read_csv(here("Data/Albacore_tuna_diet_ocean_basin.csv"))

my_prey_basin$PreySP <- gsub(" ", "_", my_prey_basin$PreySP)
#there are 3 additional columns which are unneccessary for this
my_prey_basin <-my_prey_basin[,1:2]
#From the goggle sheet, it had totals as rows. I am removing those
my_prey_basin <- my_prey_basin[complete.cases(my_prey_basin),]
#now we have the same issue as above where the species do not match
#this a bit more of an extreme case, as we have all of the invertebrates as well
#First I need to find the species which have misspelling in the database and replace those to match what is in the tree
#I will change my_prey_basin wit the spelling established in "y" earlier
#there are some species found in multiple ocean basins, so keep that in mind
#below are the synonyms for the species in the database which I am changing
#Gonatus_fabricii = Gonatus steenstrupi
#Bathylagus_stilbius = Leuroglossus stilbius 
#Moroteuthis_robusta = Onykia robusta
#Notoscopelus_elongatus = Notoscopelus kroyeri
#Okutania_anonycha = Berryteuthis anonychus
my_prey_basin[4,2] = y[1]; my_prey_basin[41,2] = y[1]; my_prey_basin[47,2] = y[2]; 
my_prey_basin[194,2] = y[3]; my_prey_basin[8,2] = y[4];
my_prey_basin[61,2] = y[4]; my_prey_basin[273,2] = y[4]; my_prey_basin[59,2] = y[4]; 
my_prey_basin[160,2] = y[5]; my_prey_basin[163,2] = y[6]; my_prey_basin[67,2] = y[7]; 
my_prey_basin[208,2] = y[8]; my_prey_basin[101,2] = y[9]; 
my_prey_basin[76,2] = y[10]; my_prey_basin[184,2] = y[10]; my_prey_basin[91,2] = y[11];
my_prey_basin[217,2] = y[12]; my_prey_basin[98,2] = y[13]; my_prey_basin[102,2] = y[14];
my_prey_basin[148,2] = y[15]; my_prey_basin[232,2] = y[16]

#these are the species that are in the tree that need to be added with ocean basin data
my_prey_add_basin = my_prey %>% 
  filter(my_prey$PreySP %notin% my_prey_basin$PreySP )

#Below is the species and its given ocean basin as from the sheet "albacore_diet_review_data"
#This is because the document didn't contain this species with its ocean basin
#Ceratoscopelus_maderensis = NE Atlantic
my_prey_basin[296,] = c("NE Atlantic", "Ceratoscopelus_maderensis")

#Now I am going to merge the two datasets together so it has all of the species and the oceanbasin and FO data
#in the merge, the max and avg FO was repeated when there was one species in two ocean basins

my_prey_basin_fo <- merge(my_prey, my_prey_basin, by = "PreySP")
nrow(my_prey_basin_fo)
#There is one repeated species, so I removed that
my_prey_basin_fo <- my_prey_basin_fo[-59,]

#There is more than 234 rows because some species are in multiple ocean basins so they will be counted twice.
#Now I need to separate the above dataframe, into one for each basin, so that I can properly pipe the data onto the tree
#I did a few things here first splitting the data for the given ocean basin
#Then made a separte dataframe which will be used to pipe to the tree, of just the max.fo
#then renamed the rows in the my_tree_... with that respective species name column in my_prey_...
#then changed the column names, so it will look better in the final product
#This seems like a lot of steps with the unecessary creation of extra dataframes
#but without doing this the data was not properly piping onto the tree and this 
# was the solution I found to get everything properly piped
my_prey_sw_pacific <- split(my_prey_basin_fo, my_prey_basin_fo$OceanBasinQ)[['SW Pacific']]
my_tree_sw_pacific <- as.data.frame(my_prey_sw_pacific[,2])
rownames(my_tree_sw_pacific) <- my_prey_sw_pacific[,1]
names(my_tree_sw_pacific) <- "SW Pacific"
my_prey_sw_atlantic <- split(my_prey_basin_fo, my_prey_basin_fo$OceanBasinQ)[['SW Atlantic']]
my_tree_sw_atlantic <- as.data.frame(my_prey_sw_atlantic[,2])
rownames(my_tree_sw_atlantic) <- my_prey_sw_atlantic[,1]
names(my_tree_sw_atlantic) <- "SW Atlantic"
my_prey_ne_pacific <- split(my_prey_basin_fo, my_prey_basin_fo$OceanBasinQ)[['NE Pacific']]
my_tree_ne_pacific <- as.data.frame(my_prey_ne_pacific[,2])
rownames(my_tree_ne_pacific) <- my_prey_ne_pacific[,1]
names(my_tree_ne_pacific) <- "NE Pacific"
my_prey_ne_atlantic <- split(my_prey_basin_fo, my_prey_basin_fo$OceanBasinQ)[['NE Atlantic']]
my_tree_ne_atlantic <- as.data.frame(my_prey_ne_atlantic[,2])
rownames(my_tree_ne_atlantic) <- my_prey_ne_atlantic[,1]
names(my_tree_ne_atlantic) <- "NE Atlantic"
my_prey_mediterranean <- split(my_prey_basin_fo, my_prey_basin_fo$OceanBasinQ)[['Mediterranean']]
my_tree_mediterranean <- as.data.frame(my_prey_mediterranean[,2])
rownames(my_tree_mediterranean) <- my_prey_mediterranean[,1]
names(my_tree_mediterranean) <- "Mediterranean"
my_prey_nw_atlantic <- split(my_prey_basin_fo, my_prey_basin_fo$OceanBasinQ)[['NW Atlantic']]
my_tree_nw_atlantic <- as.data.frame(my_prey_nw_atlantic[,2])
rownames(my_tree_nw_atlantic) <- my_prey_nw_atlantic[,1]
names(my_tree_nw_atlantic) <- "NW Atlantic"
my_prey_ne_indian <- split(my_prey_basin_fo, my_prey_basin_fo$OceanBasinQ)[['NE Indian']]
my_tree_ne_indian <- as.data.frame(my_prey_ne_indian[,2])
rownames(my_tree_ne_indian) <- my_prey_ne_indian[,1]
names(my_tree_ne_indian) <- "NE Indian"

#Now that we have all of the datasets, we can create the tree.( I am choosing the ocean basin with most species to least)
#the first one is very straight forward, with the empty tree and ne_pacific info and an offset of 0
#for the rest of the ocean basin additions, we use the tree with the heatmap on it, then the tree
#with to heatmaps on it and so on and so forth. for each choosing another ocean basin
#then increasing the offset by 0.15, because that is the width we specified.
#we repeat until all of the ocean basin are created

brewer.pal(n = 8, name = "BrBG") 
p1_3 <- gheatmap(mycirc, my_tree_ne_pacific, offset=0, width=0.15, colnames = FALSE, #oranges
         colnames_angle=95, colnames_offset_y = .25) +
  scale_fill_gradient2('Max. FO - NE Pacific (1)',
    low = '#FFF5EB', high = '#8C2D04', mid = '#FDAE6B', na.value = 'grey90', limits= c(0.0000001, 100),
    midpoint = 50,
    guide = guide_colorbar(title.position = 'top')
  )
p1_3 <- p1_3 + new_scale_fill() 
p2_3 <- gheatmap(p1_3, my_tree_ne_atlantic, offset=0.15, width=0.15, colnames = FALSE, #blues
           colnames_angle=95, colnames_offset_y = .16)+
  scale_fill_gradient2('Max. FO - NE Atlantic (2)',
                       low = '#F7FBFF', high = '#084594', mid = '#9ECAE1', na.value = 'grey90', limits= c(0.0000001, 100),
                       midpoint = 50,
                       guide = guide_colorbar(title.position = 'top')
  )
p2_3 <- p2_3 + new_scale_fill()
p3_3 <-  gheatmap(p2_3, my_tree_mediterranean, offset=0.3, width=0.15, colnames = FALSE, #purples
           colnames_angle=95, colnames_offset_y = .15)+
  scale_fill_gradient2('Max. FO - Mediterranean (3)',
                       low = '#FCFBFD', high = '#4A1486', mid = '#BCBDDC', na.value = 'grey90', limits= c(0.0000001, 100),
                       midpoint = 50,
                       guide = guide_colorbar(title.position = 'top')
  )
p3_3 <- p3_3 + new_scale_fill()
p4_3 <-  gheatmap(p3_3, my_tree_nw_atlantic, offset=0.45, width=0.15, colnames = FALSE, #greens
           colnames_angle=95, colnames_offset_y = .15)+
  scale_fill_gradient2('Max. FO - NW Atlantic (4)',
                       low = '#F7FCF5', high = '#005A32', mid = '#A1D99B', na.value = 'grey90', limits= c(0.0000001, 100),
                       midpoint = 50,
                       guide = guide_colorbar(title.position = 'top')
  )
p4_3 <- p4_3 + new_scale_fill() 
p5_3 <-  gheatmap(p4_3, my_tree_sw_atlantic, offset=0.6, width=0.15, colnames = FALSE, #reds
           colnames_angle=95, colnames_offset_y = .15)+
  scale_fill_gradient2('Max. FO - SW Atlantic (5)',
                       low = '#FFF5F0', high = '#99000D', mid = '#FC9272', na.value = 'grey90', limits= c(0.0000001, 100),
                       midpoint = 50,
                       guide = guide_colorbar(title.position = 'top')
  )
p5_3 <- p5_3 + new_scale_fill()
p6_3 <-  gheatmap(p5_3, my_tree_sw_pacific, offset=0.75, width=0.15, colnames = FALSE, #greys
           colnames_angle=95, colnames_offset_y = .15)+
  scale_fill_gradient2('Max. FO - SW Pacific (6)',
                       low = '#FFFFFF', high = '#252525', mid = '#BDBDBD', na.value = 'grey90', limits= c(0.0000001, 100),
                       midpoint = 50,
                       guide = guide_colorbar(title.position = 'top')
  )
p6_3 <- p6_3 + new_scale_fill()
p7_3 <-  gheatmap(p6_3, my_tree_ne_indian, offset=0.9, width=0.15, colnames = FALSE, #browns
           colnames_angle=95, colnames_offset_y = .15)+
  scale_fill_gradient2('Max. FO - NE Indian (7)',
                       low = '#C7EAE5', high = '#8C510A', mid = '#DFC27D', na.value = 'grey90', limits= c(0.0000001, 100),
                       midpoint = 50,
                       guide = guide_colorbar(title.position = 'top'))+
  theme(legend.key.size = unit(2,'mm'),
         legend.text = element_text(size = 5),
         legend.title = element_text(size = 5),
         legend.spacing = unit(0.02,'cm'),
         legend.position = c(0.99,0.5)) +
  annotate('text', x = 1.15, y = 5.5, label = '1', angle = 0, size = 3)+
  annotate('text', x = 1.3, y = 5.5, label = '2', angle = 0, size = 3)+
  annotate('text', x = 1.45, y = 5.5, label = '3', angle = 0, size = 3)+
  annotate('text', x = 1.6, y = 5.5, label = '4', angle = 0, size = 3)+
  annotate('text', x = 1.75, y = 5.5, label = '5', angle = 0, size = 3)+
  annotate('text', x = 1.9, y = 5.5, label = '6', angle = 0, size = 3)+
  annotate('text', x = 2.05, y = 5.5, label = '7', angle = 0, size = 3)
p7_3 <- p7_3 + new_scale_fill()
task3finalplot = p7_3
#Now we have the final product(for now) with each of the ocean basins and the 
#species max fo. I would like to point out when one species was in multiple ocean basins
#the fo is the same for both and do not have different FOs for each basin
#This is what I have so far. But we will need to discuss how this should be changed to look better








