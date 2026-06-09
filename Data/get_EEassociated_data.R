#Date created: 8 June 2026

setwd("~/Desktop/Manucripts/E6_diversitydynamics")

# TODO: write script or add details about getting Phanerozoic occs
# and generating other inputs

#==== Data input ====#
load('Data/Phanerozoic_clean_final.RData') #phanerozoic data
load('Data/Reef_Ecosystem_Engineers_final.RData') #reef-builders
load('Data/Bioturbators_data.RData') #bioturbators

#===== get crinoid meadows =====#



#==== parse through data ====#

#Get formations 
reef_formations <- unique(all_reef_builders$formation)
bioturbator_formations <- unique(bioturbators_data$formation)
EE_formations <- unique(c(reef_formations, bioturbator_formations))

#Get genera 
reef_genera <- unique(all_reef_builders$genus)
bioturbator_genera <- unique(bioturbators_data$genus)
EE_genera <- unique(reef_genera, bioturbator_genera)

#Pull out data
non_EE_data <- subset(all_data, !(genus %in% EE_genera)) #non-EE taxa...
EE_associated_data <- subset(non_EE_data, formation %in% EE_formations) #...which are in EE formations

non_EE_associated_data <- subset(non_EE_data, !(formation %in% EE_formations)) #and taxa which are NEVEr associated with ecosystem engineers 





     