######################## Extract species taxonomic information #####################
## Script created 01 August 2022 by A.J. Turbelin - Updated 19 August 2026

#update.packages()
#set working directory
#setwd("~/R_projects/")

#load packages required 
library(dplyr)
library(tidyr)
library(taxize)
library(magrittr)

options(stringsAsFactors=FALSE)
rm(list=ls())


get_gbif_taxonomy <- function(sp_list) {
  
  records <- lapply(sp_list, function(sp) {
    
    tax_detail <- gbif_name_usage(name = sp)
    
    if (length(tax_detail$results) == 0) {
      return(data.frame(
        Species = sp,
        kingdom = NA,
        phylum = NA,
        class = NA,
        order = NA,
        family = NA,
        genus = NA,
        species = NA,
        # scientificName = NA,
        canonicalName = NA,

        vernacularName = NA,
        accepted = NA
      ))
    }
    
    res <- tax_detail$results[[1]]
    
    data.frame(
      Species = sp,
      kingdom = res$kingdom %||% NA,
      phylum = res$phylum %||% NA,
      class = res$class %||% NA,
      order = res$order %||% NA,
      family = res$family %||% NA,
      genus = res$genus %||% NA,
      species = res$species %||% NA,
      # scientificName = res$scientificName %||% NA,
      canonicalName = res$canonicalName %||% NA,
      vernacularName = res$vernacularName %||% NA,
      accepted = res$accepted %||% NA
    )
  })
  
  dplyr::bind_rows(records)
}


###############################################################################
## apply function

sp_list <- c("Erica tetralix",
  "Erica cinerea",
  "Juncus effusus",
  "Juncus squarrosus",
  "Crataegus monogyna",
  "Rhododendron ponticum",
  "Nardus stricta",
  "Anthoxanthum odoratum", 
  "Molinia caerulea",
  "Narthecium ossifragum",
  "Drosera rotundifolia",
  "Eriophorum angustifolium",
  "Deschampsia cespitosa",
  "Digitalis purpurea",
  "Pteridium aquilinum",
  "Deschampia flexuosa",
  "Ranunculus acris",
  "Succisa pratensis",
  "Vaccinium myrtillus",
  "Galium saxatile",
  "Rubus fruticosus",
  "Senecio jacobaea",
  "Potentilla erecta",
  "Taxus baccata",
  "Campanula rotundifolia",
  "Betula pubescens",
  "Sphagnum spp.",
  "Carex flacca",
  "Sorbus aucuparia",
  "Ulex europaeus",
  "Iris pseudacorus",
  "Betula pendula",
  "Ilex aquifolium",
  "Urtica dioica",
  "Agrostis capillaris",
  "Cynosurus cristatus",
  "Cirsium palustre",
  "Lotus corniculatus",
  "Sambucus nigra",
  "Fraxinus excelsior",
  "Calluna vulgaris",
  "Potentilla simplex",
  "Hydrocotyle vulgaris",
  "Erica tetralix")

#sp_list <- c("Wuchereria bancrofti")

df <- get_gbif_taxonomy(sp_list)
df
#write.csv(df,"C:/Users/Ross Noble/UofE EIA Files/VegGroup/VegGroupTaxonomy.csv")

df2 <- get_gbif_taxonomy(c("Deschampsia flexuosa","Sphagnum affine"))
write.csv(df2, "C:/Users/Ross Noble/UofE EIA Files/VegGroup/VegGroupTaxonomy_extra.csv" )
