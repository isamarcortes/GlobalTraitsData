library('BIEN')
library('dplyr')
file <- BIEN_list_all()###all species in BIEN dataset

GlobalData <- read.csv('C:/Users/cenv1124/OneDrive - Nexus365/GlobalTraitsData/SpeciesTraitsData/MasterList.csv')
names(file)[1]<-'AccSpeciesName'###renaming one dataset to join

result <- inner_join(file, GlobalData, by = "AccSpeciesName")####Here's the join only keeping the overlapping species

trait_list <- BIEN_trait_list()

trait_vector<-c('leaf nitrogen content per leaf dry mass', 'leaf phosphorus content per leaf dry mass',
                'leaf area','whole plant vegetative phenology','leaf thickness','maximum whole plant height',
                'stem wood density')

species_vector<-c(result$AccSpeciesName)
traitTable<- BIEN_trait_traitbyspecies(trait=trait_vector,species=species_vector)
 
write.csv(traitTable,'C:/Users/cenv1124/OneDrive - Nexus365/GlobalTraitsData/SpeciesTraitsData/TraitTableBIEN.csv')
