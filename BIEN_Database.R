library('BIEN')
library('dplyr')
library('rtry')
library('readr')

fileBIEN <- BIEN_list_all()###all species in BIEN dataset

GlobalData <- read.csv('C:/Users/cenv1124/OneDrive - Nexus365/GlobalTraitsData/SpeciesTraitsData/MasterList.csv')
names(fileBIEN)[1]<-'AccSpeciesName'###renaming one dataset to join

result <- inner_join(fileBIEN, GlobalData, by = "AccSpeciesName")####Here's the join only keeping the overlapping species

###trait_list <- BIEN_trait_list()###gets traits in BIEN data

trait_vector<-c('leaf nitrogen content per leaf dry mass', 'leaf phosphorus content per leaf dry mass',
                'leaf area','whole plant vegetative phenology','leaf thickness','maximum whole plant height',
                'stem wood density')

species_vector <-c(result$AccSpeciesName)
traitTableBIEN <- BIEN_trait_traitbyspecies(trait=trait_vector,species=species_vector)

fileTRY <- rtry_import('C:/Users/cenv1124/Downloads/42228_17062025043509/42228.txt')
result_TRY <- semi_join(fileTRY, GlobalData, by = "AccSpeciesName")####Here's the join only keeping the overlapping species

traitBIENCleaned <- select(traitTableBIEN,scrubbed_species_binomial,trait_name,trait_value,unit)
traitTRYCleaned <- select(result_TRY,AccSpeciesName,TraitName,OrigValueStr,OrigUnitStr)
traitTRYCleaned <- traitTRYCleaned[!apply(traitTRYCleaned == "", 1, any), ]


names(traitBIENCleaned)[1] <- 'AccSpeciesName'
names(traitTRYCleaned)[2]<- 'trait_name'
names(traitTRYCleaned)[3]<- 'trait_value'
names(traitTRYCleaned)[4]<- 'unit'

mergedData <- rbind(traitBIENCleaned,traitTRYCleaned)


#####
Afr <- 'C:/Users/cenv1124/Downloads/Africa_abM_v4.csv'
Asia <- 'C:/Users/cenv1124/Downloads/Asia_abM_v4.csv'
Canada <- 'C:/Users/cenv1124/Downloads/Canada_abM_v4_7.csv'
Eur <- 'C:/Users/cenv1124/Downloads/Europe_abM_v4.csv'
N_Amer <- 'C:/Users/cenv1124/Downloads/NorthAmerica_RestOfArea_abM_v4_7.csv'
Oceania <- 'C:/Users/cenv1124/Downloads/Oceania_abM_v4.csv'
S_Amer <- 'C:/Users/cenv1124/Downloads/SouthAmerica_abM_v4.csv'
US <- 'C:/Users/cenv1124/Downloads/US_abM_v4_7.csv'

# Define a callback function that processes each chunk
process_chunk <- function(df, pos) {
  df %>%
    filter(Yr >= 1975)  # Adjust 'date' column as needed
}

# Read in chunks and process
Afr <- read_csv_chunked(
  Afr,
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 100000  # Tune this based on your system
)

Asia <- read_csv_chunked(
  Asia,
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 100000  # Tune this based on your system
) 

Canada <- read_csv_chunked(
  Canada,
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 100000  # Tune this based on your system
) 

Eur <- read_csv_chunked(
  Eur,
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 100000  # Tune this based on your system
) 

N_Amer <- read_csv_chunked(
  N_Amer,
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 100000  # Tune this based on your system
) 

Oceania <- read_csv_chunked(
  Oceania,
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 100000  # Tune this based on your system
) 

S_Amer <- read_csv_chunked(
  S_Amer,
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 100000  # Tune this based on your system
) 

US <- read_csv_chunked(
  US,
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 100000  # Tune this based on your system
) 

