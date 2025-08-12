###### This code takes the merged trait data 
###### and fixes the units so everything that is related is 
###### uniform with respect to units. This is a preprocessing
###### step before merging with abundance data

library(dplyr)
library(ggplot2)

split_data <- split(mergedData, mergedData$trait_name)
split_data[[2]]$unit[split_data[[2]]$unit == "kg/m/s/Mpa"] <- "kg.m-1.MPa-1.s-1"
split_data[[2]]$unit[split_data[[2]]$unit == "[kg/(m s Mpa)]"] <- "kg.m-1.MPa-1.s-1"
split_data[[6]]$trait_name[split_data[[6]]$trait_name == "Leaf"] <- "LeafWaterContent"
split_data[[6]]$unit[split_data[[6]]$unit == "Water"] <- "grams"
split_data[[8]]$unit[split_data[[8]]$unit == "cm^2"] <- "cm2"
split_data[[8]]$unit[split_data[[8]]$unit == "cm²"] <- "cm2"

split_data[[8]] <- split_data[[8]] %>%
  mutate(
    trait_value = case_when(
      unit == "m2 leaf plant-1" ~ trait_value * 10000,  # Multiply by 0.1 for mg·g???¹
      unit == "m2" ~ trait_value * 10000, 
      TRUE ~ trait_value  # Keep original value for other units or NA values
    )
  )

split_data[[8]]$unit[split_data[[8]]$unit == "m2 leaf plant-1"] <- "cm2"
split_data[[8]]$unit[split_data[[8]]$unit == "m2"] <- "cm2"

split_data[[10]]$unit[split_data[[10]]$unit == "% dry mass"] <- "percent"
split_data[[10]]$unit[split_data[[10]]$unit == "%"] <- "percent"
split_data[[10]]$unit[split_data[[10]]$unit == "%DM"] <- "percent"
split_data[[10]]$unit[split_data[[10]]$unit == "mg/g"] <- "mg.g-1"
split_data[[10]]$unit[split_data[[10]]$unit == "mg / g"] <- "mg.g-1"
split_data[[10]]$unit[split_data[[10]]$unit == "mg_g-1"] <- "mg.g-1"
split_data[[10]]$unit[split_data[[10]]$unit == "mg/g dry mass"] <- "mg.g-1"
split_data[[10]]$unit[split_data[[10]]$unit == "g/kg"] <- "g·kg-1"
split_data[[10]]$unit[split_data[[10]]$unit == "g kg-1"] <- "g·kg-1"
split_data[[10]]$unit[split_data[[10]]$unit == "(mg g-1)"] <- "mg g-1"
split_data[[10]]$unit[split_data[[10]]$unit == "mg N g-1"] <- "mg g-1"
split_data[[10]]$unit[split_data[[10]]$unit == "mg g-1"] <- "mg.g-1"
split_data[[10]]$unit[split_data[[10]]$unit == "% mass/mass"] <- "percent"
split_data[[10]]$unit[split_data[[10]]$unit == "mg/mg *100"] <- "percent"
split_data[[10]]$unit[split_data[[10]]$unit == "% (100 * g g-1 )"] <- "percent"
split_data[[10]]$unit[split_data[[10]]$unit == "mgN gDM-1"] <- "mg.g-1"
split_data[[10]]$unit[split_data[[10]]$unit == "mg/mg *100"] <- "percent"
split_data[[10]]$unit[split_data[[10]]$unit == "g N g-1 DW"] <- "g/g"
split_data[[10]]$unit[split_data[[10]]$unit == "g/100g"] <- "percent"

split_data[[10]] <- split_data[[10]] %>%
  mutate(
    trait_value = case_when(
      unit == "mg g-1" ~ trait_value * 0.1,  # Multiply by 0.1 for mg·g???¹
      unit == "g·kg-1" ~ trait_value * 0.1, 
      unit == "g/g"   ~ trait_value * 100,   # g/g to percent
      unit == "kg/kg"   ~ trait_value * 100,   
      unit == "mmol/g" ~ trait_value * 14.01 * 100,
      TRUE ~ trait_value  # Keep original value for other units or NA values
    )
  )

split_data[[10]]$unit[split_data[[10]]$unit == "mg.g-1"] <- "percent"
split_data[[10]]$unit[split_data[[10]]$unit == "g/g"] <- "percent"
split_data[[10]]$unit[split_data[[10]]$unit == "mmol/g"] <- "percent"
split_data[[10]]$unit[split_data[[10]]$unit == "g·kg-1"] <- "percent"
split_data[[10]]$unit[split_data[[10]]$unit == "kg/kg"] <- "percent"

molar_mass_P <- 30.97
split_data[[11]] <- split_data[[11]] %>%
  mutate(
    trait_value = case_when(
      unit %in% c("mg.g-1", "mg/g", "mg g-1", "mg_g-1", "(mg g-1)", "mgP gDM-1") ~ trait_value * 0.1,
      unit %in% c("g P g-1 DW", "g/g") ~ trait_value * 100,
      unit == "g/100g" ~ trait_value,
      unit %in% c("g/kg", "g·kg-1") ~ trait_value * 0.1,
      unit == "mg/10g" ~ trait_value * 0.01,
      unit == "ppm" ~ trait_value * 0.0001,
      unit %in% c("micro g(P)/g(DM)", "micro g g-1") ~ trait_value * 0.0001,
      unit == "mmol/kg" ~ trait_value * molar_mass_P * 0.1,
      unit %in% c("percent", "%", "% mass/mass") ~ trait_value,
      TRUE ~ NA_real_  # For unknown units
    ),
    unit = "percent"
  )

split_data[[12]] <- split_data[[12]] %>%
  mutate(
    trait_value = case_when(
      unit %in% c("cm") ~ trait_value,
      unit %in% c("mm", "millimeter") ~ trait_value * 0.1,
      unit == "0.1mm" ~ trait_value * 0.01,
      unit %in% c("micro m", "um", "microm", "micrometer", "micron") ~ trait_value * 0.0001,
      TRUE ~ trait_value
    ),
    unit = case_when(
      unit %in% c("cm", "mm", "millimeter", "0.1mm", "micro m", "um", "microm", "micrometer", "micron") ~ "cm",
      TRUE ~ unit
    )
  )

split_data[[20]]$unit[split_data[[20]]$unit == "kg s?1 MPa?1"] <- "kg s-1 MPa-1"

FixedDF <- bind_rows(split_data)
FixedDF <- na.omit(FixedDF)


rm(filtered_data,filtered_df,filtered_df1,GlobalData,JesusData,
   mergedData,result,result_TRY,subset_data,t,test,traitBIENCleaned,
   traitTableBIEN,traitTRYCleaned)