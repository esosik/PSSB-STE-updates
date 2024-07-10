
library(plyr)
library(openxlsx)
library(lubridate)
library(stringr)
library(tidyverse)
library(dplyr)
getwd()
setwd("G:/GreenWQA/Biota/PSP/2023_indicator_update")


taxaBind <- function(file.path) {
  
  path.files <- list.files(file.path)
  # read in files
  list.with.each.file <- lapply(paste(file.path, list.files(file.path), sep = ''), function(y) read.delim(y, header=TRUE))
  taxa<-do.call("rbind.data.frame", list.with.each.file)
  return(taxa)
  
  
}

file.path="./taxonomy data/"
raw<-taxaBind(file.path)

names(raw)
PSSB_taxa<-unique(raw[,c(28, 29, 48:69)])
##there are some repeat entries that somewhere in the hierarchy have an NA instead of "". This yields multiples of the same taxa. Fix this.
PSSB_taxa[is.na(PSSB_taxa)]<-""
PSSB_taxa<-unique(PSSB_taxa)

###########BCG TRANSLATION##########
setwd("C:/Users/esosik/King County/DNRP - Science TCA Unit - Bug Monitoring/PSSB issues/STE_attrib_updates")
# lookup<-openxlsx::read.xlsx("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/BCG translation table as of 2023_03_15.xlsx", detectDates = T)
# lookup<-openxlsx::read.xlsx("ORWA_TaxaTranslator_20240204.xlsx", detectDates = T)
lookup<-read.csv("ORWA_TaxaTranslator_20240417.csv")
##Translate PSSB taxa into BCG harmonized taxa
OTU<-merge(PSSB_taxa, subset(lookup, select=c(Taxon, OTU_MetricCalc)), by.x="Taxon", by.y="Taxon", all.x=T)

##NOTE: These are PSSB taxa that are missing from the BCG translation table. Need Sean to update. 
missing<-(OTU[which(is.na(OTU$OTU_MetricCalc)), "Taxon"])
# write.csv(missing, "missing_from_BCG_04022024.csv")

##Where taxa are missing from BCG translation table, just use the PSSB Taxon value
OTU[which(is.na(OTU$OTU_MetricCalc)), "OTU_MetricCalc"] <-OTU[which(is.na(OTU$OTU_MetricCalc)), "Taxon"]
OTU[which(is.na(OTU$OTU_MetricCalc)), "OTU_MetricCalc"] <-OTU[which(is.na(OTU$OTU_MetricCalc)), "Taxon"]

DNI<-OTU[which(OTU$OTU=="DNI"),"Taxon"]###These are marked as "DNI" in BCG translation table, but they aren't necessarily on B-IBI exclusion list. Adding back in for now.
OTU[which(OTU$OTU_MetricCalc=="DNI"),"OTU_MetricCalc"]<-OTU[which(OTU$OTU_MetricCalc=="DNI"),"Taxon"]###These are marked as "DNI" in BCG translation table, but they aren't on B-IBI exclusion list. Adding back in for now.

names(lookup)
names(OTU)
names(PSSB_taxa)

test<-unique(PSSB_taxa[,c(1, 2:24)])
test[which(duplicated(test[,2:24])),] #Rhyacophila Ecosa Group is in PSSB with two TSNs

##Append correct hierarchy by matching OTU to Taxon in the PSSB taxa table. 
##NOTE: This assumes hierarchies in PSSB are correct, AND in correct order-- Discuss with Kate!
##SIDE NOTE: Do we need levels like infraorder? Do we believe labs are using these levels uniformly?
OTU_correct_hier<-merge(OTU[,c("OTU_MetricCalc", "Taxon","Taxon.Serial.Number")], unique(PSSB_taxa[,c(1:24)]), by.x="OTU_MetricCalc", by.y="Taxon", all.x=T)
names(OTU_correct_hier)[3]<-"Taxon.Serial.Number"
names(OTU_correct_hier)[4]<-"Mapped.Taxon.Serial.Number"
##For Taxa that aren't in PSSB taxa table, split out, append BCG hierarchy, and re-join
##NOTE: we should add these to PSSB
fix<-OTU_correct_hier[which(is.na(OTU_correct_hier$Phylum)),]
OTU_correct_hier<-OTU_correct_hier[which(!is.na(OTU_correct_hier$Phylum)),]
write.csv(fix[,c("OTU_MetricCalc", "Taxon","Taxon.Serial.Number")], "missing_from_PSSB_05102024.csv")
BCG_atts<-read.csv("ORWA_Attributes_20240417.csv")
names(BCG_atts)
BCG_atts[,c(1, 24:40)]
# fix<-merge(fix[,c("OTU_MetricCalc", "Taxon","Taxon.Serial.Number")], unique(lookup[,c(2, 11:27)]), by.x="OTU_MetricCalc", by.y="Taxon_orig", all.x=T)
fix<-merge(fix[,c("OTU_MetricCalc", "Taxon","Taxon.Serial.Number")], unique(BCG_atts[,c(1, 24:40)]), by.x="OTU_MetricCalc", by.y="Taxon", all.x=T)

names(OTU_correct_hier)
names(fix)
##the BCG hierarchy isn't the same as PSSB. Need to consolidate some levels into Species Group, then rename columns

fix[which(fix$SpeciesComplex!=""&fix$SpeciesGroup==""),"SpeciesGroup"]<-fix[which(fix$SpeciesComplex!=""&fix$SpeciesGroup==""),"SpeciesComplex"]
fix[which(fix$SpeciesSubGroup!=""&fix$SpeciesGroup==""),"SpeciesGroup"]<-fix[which(fix$SpeciesSubGroup!=""&fix$SpeciesGroup==""),"SpeciesSubGroup"]

fix<-fix[,c(1:17, 20)]
fix$Superclass<-NA
fix$Infraclass<-NA
fix$Superorder<-NA
fix$Infraorder<-NA
fix$Custom.Subfamily<-NA
fix$Subtribe<-NA
fix$Subspecies<-NA

names(OTU_correct_hier)
names(fix)
names(fix[,c(1:5,19, 6:7, 20:21,8:9, 22,10:12,23,13:14,24,15, 17, 16,18,25)])
fix<-fix[,c(1:5,19, 6:7, 20:21,8:9, 22,10:12,23,13:14,24,15, 17, 16,18,25)]
fix$Mapped.Taxon.Serial.Number<-NA
fix<-fix[,c(1:3, 26, 4:25)]
names(fix)<-names(OTU_correct_hier)
OTU_correct_hier<-rbind(OTU_correct_hier, fix)
OTU_correct_hier[is.na(OTU_correct_hier)]<-""

names(OTU_correct_hier)

###################Screen out exclusions#######################

##read in PSSB exclusion table
exlude<-read.xlsx("G:/GreenWQA/Biota/PSSB Puget Sound Stream Benthos/PSSB_exclusions.xlsx")
exlude<-subset(exlude, select=c(Taxon.Name, Excluded))


##Perform a series of rolling lookups to find a match in taxa hierarchies in the Taxon Name column of the exclusion table, starting with direct match, then from lowest hierarchy to highest

exlude2<-data.frame(Taxon.Name=character(), Excluded=character())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(5:26, 1)]
for (i in 1:ncol(test)){
  k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
  attribs<-merge(exlude, test[, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
  # names(attribs)<-str_replace(names(attribs), "2012.", "")
  names(attribs)<-str_replace(names(attribs), "OTU_MetricCalc.1", "OTU_MetricCalc")
  exlude2<-rbind(attribs, exlude2)
  test<-subset(test, !OTU_MetricCalc %in% exlude2$OTU_MetricCalc)
}

exlude2<-unique(exlude2)
names(exlude2)[1]<-"Exclusion_reason"
OTU_correct_hier<-merge(OTU_correct_hier, exlude2, all.x=T)

exlude_no_match2<-exlude[!exlude$Taxon.Name %in% OTU_correct_hier$Exclusion_reason,]
# PSSB_taxa2<-PSSB_taxa
# 
# ##remove excluded taxa from the master list
# PSSB_taxa<-subset(PSSB_taxa, Excluded!=TRUE|is.na(Excluded), select=c(-Exclusion_reason, -Excluded))

######################## Coarse STE roll-up########################
OTU_correct_hier[which(OTU_correct_hier$Subclass=="Oligochaeta"),"Coarse_STE"]<-"Oligochaeta"
OTU_correct_hier[which(OTU_correct_hier$Subclass=="Oligochaeta"),"Coarse_STE_rank"]<-"Subclass"

OTU_correct_hier[which(OTU_correct_hier$Subclass=="Acari"),"Coarse_STE"]<-"Acari"
OTU_correct_hier[which(OTU_correct_hier$Subclass=="Acari"),"Coarse_STE_rank"]<-"Subclass"

OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),"Coarse_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),], 
                                                                             ifelse(Family!="", Family,
                                                                              ifelse(Superfamily!="", Superfamily,
                                                                                ifelse(Infraorder!="", Infraorder,
                                                                                  ifelse(Suborder!="", Suborder,
                                                                                    ifelse(Order!="", Order,
                                                                                      ifelse(Superorder!="", Superorder,
                                                                                        ifelse(Infraclass!="", Infraclass,
                                                                                          ifelse(Subclass!="", Subclass,
                                                                                            ifelse(Class!="", Class, NA))))))))))

OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),"Coarse_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),], 
                                                                             ifelse(Family==Coarse_STE, "Family",
                                                                                    ifelse(Superfamily==Coarse_STE, "Superfamily",
                                                                                           ifelse(Infraorder==Coarse_STE, "Infraorder",
                                                                                                  ifelse(Suborder==Coarse_STE, "Suborder",
                                                                                                         ifelse(Order==Coarse_STE, "Order",
                                                                                                                ifelse(Superorder==Coarse_STE, "Superorder",
                                                                                                                       ifelse(Infraclass==Coarse_STE, "Infraclass",
                                                                                                                              ifelse(Subclass==Coarse_STE, "Subclass",
                                                                                                                                     ifelse(Class==Coarse_STE, "Class", NA))))))))))

OTU_correct_hier[which(OTU_correct_hier$Family=="Dytiscidae"),"Coarse_STE"]<-"Dytiscidae"
OTU_correct_hier[which(OTU_correct_hier$Family=="Dytiscidae"),"Coarse_STE_rank"]<-"Family"

OTU_correct_hier[which(OTU_correct_hier$Family=="Simuliidae"),"Coarse_STE"]<-"Simuliidae"
OTU_correct_hier[which(OTU_correct_hier$Family=="Simuliidae"),"Coarse_STE_rank"]<-"Family"

OTU_correct_hier[which(OTU_correct_hier$Family=="Chironomidae"),"Coarse_STE"]<-"Chironomidae"
OTU_correct_hier[which(OTU_correct_hier$Family=="Chironomidae"),"Coarse_STE_rank"]<-"Family"

OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),"Coarse_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),], 
                                                                              ifelse(Genus!="", Genus,
                                                                                     ifelse(Subtribe!="", Subtribe,
                                                                                            ifelse(Genus.Group!="", Genus.Group,
                                                                                              
                                                                                                          ifelse(Tribe!="", Tribe,
                                                                                                                 ifelse(Custom.Subfamily!="", Custom.Subfamily,
                                                                                                                        ifelse(Subfamily!="", Subfamily,     
                                                                              ifelse(Family!="", Family,
                                                                                     ifelse(Superfamily!="", Superfamily,
                                                                                            ifelse(Infraorder!="", Infraorder,
                                                                                                   ifelse(Suborder!="", Suborder,
                                                                                                          ifelse(Order!="", Order, NA))))))))))))
OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),"Coarse_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),], 
                                                                              ifelse(Genus==Coarse_STE, "Genus",
                                                                                     ifelse(Subtribe==Coarse_STE, "Subtribe",
                                                                                            ifelse(Genus.Group==Coarse_STE, "Genus.Group",
                                                                                                          ifelse(Tribe==Coarse_STE, "Tribe",
                                                                                                                 ifelse(Custom.Subfamily==Coarse_STE, "Custom.Subfamily",
                                                                                                                        ifelse(Subfamily==Coarse_STE, "Subfamily",     
                                                                                                                               ifelse(Family==Coarse_STE, "Family",
                                                                                                                                      ifelse(Superfamily==Coarse_STE, "Superfamily",
                                                                                                                                             ifelse(Infraorder==Coarse_STE, "Infraorder",
                                                                                                                                                    ifelse(Suborder==Coarse_STE, "Suborder",
                                                                                                                                                           ifelse(Order==Coarse_STE, "Order", NA))))))))))))

OTU_correct_hier[which(is.na(OTU_correct_hier$Coarse_STE)),"Coarse_STE"]<-OTU_correct_hier[which(is.na(OTU_correct_hier$Coarse_STE)),"OTU_MetricCalc"]
OTU_correct_hier[OTU_correct_hier==""]<-"ZZZ"

OTU_correct_hier[which(is.na(OTU_correct_hier$Coarse_STE_rank)),"Coarse_STE_rank"]<-with(OTU_correct_hier[which(is.na(OTU_correct_hier$Coarse_STE_rank)),], 
                                                                                     ifelse(str_detect(Coarse_STE, coll(Subspecies)), "Subspecies",
                                                                                        ifelse( str_detect(Coarse_STE, coll(Species)), "Species",
                                                                                                ifelse(str_detect(Coarse_STE,coll(Subgenus)), "Subgenus",
                                                                                                       ifelse(str_detect(Coarse_STE, coll(Species.Group )), "Species.Group",
                                                                                 ifelse(Genus==Coarse_STE, "Genus",
                                                                                       ifelse(Subtribe==Coarse_STE, "Subtribe",
                                                                                              ifelse(Genus.Group==Coarse_STE, "Genus.Group",
                                                                                                     ifelse(Tribe==Coarse_STE, "Tribe",
                                                                                                            ifelse(Custom.Subfamily==Coarse_STE, "Custom.Subfamily",
                                                                                                                   ifelse(Subfamily==Coarse_STE, "Subfamily",     
                                                                                                                          ifelse(Family==Coarse_STE, "Family",
                                                                                                                                 ifelse(Superfamily==Coarse_STE, "Superfamily",
                                                                                                                                        ifelse(Infraorder==Coarse_STE, "Infraorder",
                                                                                                                                               ifelse(Suborder==Coarse_STE, "Suborder",
                                                                                                                                                      ifelse(Order==Coarse_STE, "Order", 
                                                                                                                                                                    ifelse(Superorder==Coarse_STE, "Superorder", 
                                                                                                                                                                           ifelse(Infraclass==Coarse_STE, "Infraclass", 
                                                                                                                                                                                  ifelse(Subclass==Coarse_STE, "Subclass", 
                                                                                                                                                                                         ifelse(Class==Coarse_STE, "Class", 
                                                                                                                                                                                                ifelse(Superclass==Coarse_STE, "Superclass", 
                                                                                                                                                                                                       ifelse(Subphylum==Coarse_STE, "Subphylum", 
                                                                                                                                                                                                              ifelse(Phylum==Coarse_STE, "Phylum",
                                                                                                                                                             
                                                                                                                                                             NA)))))))))))))))))))))))
##Note to Sean, need to change species level for Perlinodes aureus and Matriella teresa in BCG table. Need something for Genus group Neaviperla/Suwallia besides tribe
test<-OTU_correct_hier[which(is.na(OTU_correct_hier$Coarse_STE_rank)),]
# write.csv(test, "BCG_hierarchy_change_04152024.csv")
# OTU_correct_hier[which(OTU_correct_hier$Coarse_STE=="Matriella teresa"),"Coarse_STE_rank"]<-"Species"
# OTU_correct_hier[which(OTU_correct_hier$Coarse_STE=="Matriella teresa"),"Species"]<-"teresa"
# 
# OTU_correct_hier[which(OTU_correct_hier$Coarse_STE=="Neaviperla/Suwallia"),"Coarse_STE_rank"]<-"Species.Group"
# OTU_correct_hier[which(OTU_correct_hier$Coarse_STE=="Neaviperla/Suwallia"),"Species.Group"]<-"Neaviperla/Suwallia"
# 
# OTU_correct_hier[which(OTU_correct_hier$Coarse_STE=="Perlinodes aureus"),"Coarse_STE_rank"]<-"Species"
# OTU_correct_hier[which(OTU_correct_hier$Coarse_STE=="Perlinodes aureus"),"Species"]<-"aureus"
# 
# OTU_correct_hier[which(OTU_correct_hier$Coarse_STE=="Parathyas"),"Coarse_STE_rank"]<-"Genus"
# OTU_correct_hier[which(OTU_correct_hier$Coarse_STE=="Parathyas"),"Genus"]<-"Parathyas"

#############################################
######################## Medium STE roll-up###############
OTU_correct_hier[which(OTU_correct_hier$Subclass=="Oligochaeta"),"Medium_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Subclass=="Oligochaeta"),], 
                                                                                     ifelse(Family!="ZZZ", Family,
                                                                                            ifelse(Superfamily!="ZZZ", Superfamily,
                                                                                                   ifelse(Infraorder!="ZZZ", Infraorder,
                                                                                                          ifelse(Suborder!="ZZZ", Suborder,
                                                                                                                 ifelse(Order!="ZZZ", Order,
                                                                                                                        ifelse(Superorder!="ZZZ", Superorder,
                                                                                                                               ifelse(Infraclass!="ZZZ", Infraclass,
                                                                                                                                      ifelse(Subclass!="ZZZ", Subclass,NA)))))))))
OTU_correct_hier[which(OTU_correct_hier$Subclass=="Oligochaeta"),"Medium_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Subclass=="Oligochaeta"),], 
                                                                                          ifelse(Family==Medium_STE, "Family",
                                                                                                 ifelse(Superfamily==Medium_STE, "Superfamily",
                                                                                                        ifelse(Infraorder==Medium_STE, "Infraorder",
                                                                                                               ifelse(Suborder==Medium_STE, "Suborder",
                                                                                                                      ifelse(Order==Medium_STE, "Order",
                                                                                                                             ifelse(Superorder==Medium_STE, "Superorder",
                                                                                                                                    ifelse(Infraclass==Medium_STE, "Infraclass",
                                                                                                                                           ifelse(Subclass==Medium_STE, "Subclass",NA)))))))))

OTU_correct_hier[which(OTU_correct_hier$Subclass=="Acari"),"Medium_STE"]<-"Acari"
OTU_correct_hier[which(OTU_correct_hier$Subclass=="Acari"),"Medium_STE_rank"]<-"Subclass"

OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),"Medium_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),], 
                                                                                      ifelse(Genus!="ZZZ", Genus,
                                                                                             ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                                    ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                           
                                                                                                           ifelse(Tribe!="ZZZ", Tribe,
                                                                                                                  ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                         ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                      ifelse(Family!="ZZZ", Family,
                                                                                        ifelse(Superfamily!="ZZZ", Superfamily,
                                                                                               ifelse(Infraorder!="ZZZ", Infraorder,
                                                                                                      ifelse(Suborder!="ZZZ", Suborder,
                                                                                                             ifelse(Order!="ZZZ", Order,
                                                                                                                    ifelse(Superorder!="ZZZ", Superorder,
                                                                                                                           ifelse(Infraclass!="ZZZ", Infraclass,
                                                                                                                                  ifelse(Subclass!="ZZZ", Subclass,
                                                                                                                                         ifelse(Class!="ZZZ", Class, NA))))))))))))))))

OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),"Medium_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),], 
                                                                                      ifelse(Genus==Medium_STE, "Genus",
                                                                                             ifelse(Subtribe==Medium_STE, "Subtribe",
                                                                                                    ifelse(Genus.Group==Medium_STE, "Genus.Group",
                                                                                                           
                                                                                                           ifelse(Tribe==Medium_STE, "Tribe",
                                                                                                                  ifelse(Custom.Subfamily==Medium_STE, "Custom.Subfamily",
                                                                                                                         ifelse(Subfamily==Medium_STE, "Subfamily",  
                                                                                      ifelse(Family==Medium_STE, "Family",
                                                                                             ifelse(Superfamily==Medium_STE, "Superfamily",
                                                                                                    ifelse(Infraorder==Medium_STE, "Infraorder",
                                                                                                           ifelse(Suborder==Medium_STE, "Suborder",
                                                                                                                  ifelse(Order==Medium_STE, "Order",
                                                                                                                         ifelse(Superorder==Medium_STE, "Superorder",
                                                                                                                                ifelse(Infraclass==Medium_STE, "Infraclass",
                                                                                                                                       ifelse(Subclass==Medium_STE, "Subclass",
                                                                                                                                              ifelse(Class==Medium_STE, "Class", NA))))))))))))))))


OTU_correct_hier[which(OTU_correct_hier$Family=="Dytiscidae"),"Medium_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Dytiscidae"),], 
                                                                                 ifelse(Genus!="ZZZ", Genus,
                                                                                        ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                               ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                      
                                                                                                      ifelse(Tribe!="ZZZ", Tribe,
                                                                                                             ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                    ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                                           ifelse(Family!="ZZZ", Family,
                                                                                                                                   NA))))))))

OTU_correct_hier[which(OTU_correct_hier$Family=="Dytiscidae"),"Medium_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Dytiscidae"),], 
                                                                                      ifelse(Genus==Medium_STE, "Genus",
                                                                                             ifelse(Subtribe==Medium_STE, "Subtribe",
                                                                                                    ifelse(Genus.Group==Medium_STE, "Genus.Group",
                                                                                                           
                                                                                                           ifelse(Tribe==Medium_STE, "Tribe",
                                                                                                                  ifelse(Custom.Subfamily==Medium_STE, "Custom.Subfamily",
                                                                                                                         ifelse(Subfamily==Medium_STE, "Subfamily",  
                                                                                                                                ifelse(Family==Medium_STE, "Family", NA))))))))

OTU_correct_hier[which(OTU_correct_hier$Family=="Simuliidae"),"Medium_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Simuliidae"),], 
                                                                                  ifelse(Genus!="ZZZ", Genus,
                                                                                         ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                                ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                       
                                                                                                       ifelse(Tribe!="ZZZ", Tribe,
                                                                                                              ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                     ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                                            ifelse(Family!="ZZZ", Family,
                                                                                                                                   NA))))))))

OTU_correct_hier[which(OTU_correct_hier$Family=="Simuliidae"),"Medium_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Simuliidae"),], 
                                                                                       ifelse(Genus==Medium_STE, "Genus",
                                                                                              ifelse(Subtribe==Medium_STE, "Subtribe",
                                                                                                     ifelse(Genus.Group==Medium_STE, "Genus.Group",
                                                                                                            
                                                                                                            ifelse(Tribe==Medium_STE, "Tribe",
                                                                                                                   ifelse(Custom.Subfamily==Medium_STE, "Custom.Subfamily",
                                                                                                                          ifelse(Subfamily==Medium_STE, "Subfamily",  
                                                                                                                                 ifelse(Family==Medium_STE, "Family", NA))))))))




OTU_correct_hier[which(OTU_correct_hier$Family=="Chironomidae"),"Medium_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Chironomidae"),], 
                                                                                  ifelse(Tribe!="ZZZ", Tribe,
                                                                                                              ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                     ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                                            ifelse(Family!="ZZZ", Family,
                                                                                                                                   NA)))))

OTU_correct_hier[which(OTU_correct_hier$Family=="Chironomidae"),"Medium_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Chironomidae"),], 
                                                                                       ifelse(Tribe==Medium_STE, "Tribe",
                                                                                                                   ifelse(Custom.Subfamily==Medium_STE, "Custom.Subfamily",
                                                                                                                          ifelse(Subfamily==Medium_STE, "Subfamily",  
                                                                                                                                 ifelse(Family==Medium_STE, "Family", NA)))))




OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),"Medium_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),], 
                                                                                  ifelse(Genus!="ZZZ", Genus,
                                                                                         ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                                ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                       
                                                                                                       ifelse(Tribe!="ZZZ", Tribe,
                                                                                                              ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                     ifelse(Subfamily!="ZZZ", Subfamily,     
                                                                                                                            ifelse(Family!="ZZZ", Family,
                                                                                                                                   ifelse(Superfamily!="ZZZ", Superfamily,
                                                                                                                                          ifelse(Infraorder!="ZZZ", Infraorder,
                                                                                                                                                 ifelse(Suborder!="ZZZ", Suborder,
                                                                                                                                                        ifelse(Order!="ZZZ", Order, NA))))))))))))
OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),"Medium_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),], 
                                                                                       ifelse(Genus==Medium_STE, "Genus",
                                                                                              ifelse(Subtribe==Medium_STE, "Subtribe",
                                                                                                     ifelse(Genus.Group==Medium_STE, "Genus.Group",
                                                                                                            ifelse(Tribe==Medium_STE, "Tribe",
                                                                                                                   ifelse(Custom.Subfamily==Medium_STE, "Custom.Subfamily",
                                                                                                                          ifelse(Subfamily==Medium_STE, "Subfamily",     
                                                                                                                                 ifelse(Family==Medium_STE, "Family",
                                                                                                                                        ifelse(Superfamily==Medium_STE, "Superfamily",
                                                                                                                                               ifelse(Infraorder==Medium_STE, "Infraorder",
                                                                                                                                                      ifelse(Suborder==Medium_STE, "Suborder",
                                                                                                                                                             ifelse(Order==Medium_STE, "Order", NA))))))))))))

OTU_correct_hier[which(is.na(OTU_correct_hier$Medium_STE)),"Medium_STE"]<-OTU_correct_hier[which(is.na(OTU_correct_hier$Medium_STE)),"OTU_MetricCalc"]


OTU_correct_hier[which(is.na(OTU_correct_hier$Medium_STE_rank)),"Medium_STE_rank"]<-with(OTU_correct_hier[which(is.na(OTU_correct_hier$Medium_STE_rank)),], 
                                                                                         ifelse(str_detect(Medium_STE, coll(Subspecies)), "Subspecies",
                                                                                                ifelse( str_detect(Medium_STE, coll(Species)), "Species",
                                                                                                        ifelse(str_detect(Medium_STE,coll(Subgenus)), "Subgenus",
                                                                                                               ifelse(str_detect(Medium_STE, coll(Species.Group )), "Species.Group",
                                                                                                                      ifelse(Genus==Medium_STE, "Genus",
                                                                                                                             ifelse(Subtribe==Medium_STE, "Subtribe",
                                                                                                                                    ifelse(Genus.Group==Medium_STE, "Genus.Group",
                                                                                                                                           ifelse(Tribe==Medium_STE, "Tribe",
                                                                                                                                                  ifelse(Custom.Subfamily==Medium_STE, "Custom.Subfamily",
                                                                                                                                                         ifelse(Subfamily==Medium_STE, "Subfamily",     
                                                                                                                                                                ifelse(Family==Medium_STE, "Family",
                                                                                                                                                                       ifelse(Superfamily==Medium_STE, "Superfamily",
                                                                                                                                                                              ifelse(Infraorder==Medium_STE, "Infraorder",
                                                                                                                                                                                     ifelse(Suborder==Medium_STE, "Suborder",
                                                                                                                                                                                            ifelse(Order==Medium_STE, "Order", 
                                                                                                                                                                                                   ifelse(Superorder==Medium_STE, "Superorder", 
                                                                                                                                                                                                          ifelse(Infraclass==Medium_STE, "Infraclass", 
                                                                                                                                                                                                                 ifelse(Subclass==Medium_STE, "Subclass", 
                                                                                                                                                                                                                        ifelse(Class==Medium_STE, "Class", 
                                                                                                                                                                                                                               ifelse(Superclass==Medium_STE, "Superclass", 
                                                                                                                                                                                                                                      ifelse(Subphylum==Medium_STE, "Subphylum", 
                                                                                                                                                                                                                                             ifelse(Phylum==Medium_STE, "Phylum",
                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                    NA)))))))))))))))))))))))
##Note to Sean, need to change species level for Perlinodes aureus and Matriella teresa in BCG table. Need something for Genus group Neaviperla/Suwallia besides tribe
test<-OTU_correct_hier[which(is.na(OTU_correct_hier$Medium_STE_rank)),]
###############################################
######################## Fine STE roll-up###############
OTU_correct_hier[which(OTU_correct_hier$Subclass=="Oligochaeta"),"Fine_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Subclass=="Oligochaeta"),], 
                                                                                   ifelse(Genus!="ZZZ", Genus,
                                                                                          ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                                 ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                        
                                                                                                        ifelse(Tribe!="ZZZ", Tribe,
                                                                                                               ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                      ifelse(Subfamily!="ZZZ", Subfamily,   
                                                                                   ifelse(Family!="ZZZ", Family,
                                                                                            ifelse(Superfamily!="ZZZ", Superfamily,
                                                                                                   ifelse(Infraorder!="ZZZ", Infraorder,
                                                                                                          ifelse(Suborder!="ZZZ", Suborder,
                                                                                                                 ifelse(Order!="ZZZ", Order,
                                                                                                                        ifelse(Superorder!="ZZZ", Superorder,
                                                                                                                               ifelse(Infraclass!="ZZZ", Infraclass,
                                                                                                                                      ifelse(Subclass!="ZZZ", Subclass,NA)))))))))))))))

OTU_correct_hier[which(OTU_correct_hier$Subclass=="Oligochaeta"),"Fine_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Subclass=="Oligochaeta"),], 
                                                                                        ifelse(Genus==Fine_STE, "Genus",
                                                                                               ifelse(Subtribe==Fine_STE, "Subtribe",
                                                                                                      ifelse(Genus.Group==Fine_STE, "Genus.Group",
                                                                                                             
                                                                                                             ifelse(Tribe==Fine_STE, "Tribe",
                                                                                                                    ifelse(Custom.Subfamily==Fine_STE, "Custom.Subfamily",
                                                                                                                           ifelse(Subfamily==Fine_STE, "Subfamily",
                                                                                          ifelse(Family==Fine_STE, "Family",
                                                                                                 ifelse(Superfamily==Fine_STE, "Superfamily",
                                                                                                        ifelse(Infraorder==Fine_STE, "Infraorder",
                                                                                                               ifelse(Suborder==Fine_STE, "Suborder",
                                                                                                                      ifelse(Order==Fine_STE, "Order",
                                                                                                                             ifelse(Superorder==Fine_STE, "Superorder",
                                                                                                                                    ifelse(Infraclass==Fine_STE, "Infraclass",
                                                                                                                                           ifelse(Subclass==Fine_STE, "Subclass",NA)))))))))))))))

OTU_correct_hier[which(OTU_correct_hier$Subclass=="Acari"),"Fine_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Subclass=="Acari"),], 
                                                                                   ifelse(Genus!="ZZZ", Genus,
                                                                                          ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                                 ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                        
                                                                                                        ifelse(Tribe!="ZZZ", Tribe,
                                                                                                               ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                      ifelse(Subfamily!="ZZZ", Subfamily,   
                                                                                                                             ifelse(Family!="ZZZ", Family,
                                                                                                                                    ifelse(Superfamily!="ZZZ", Superfamily,
                                                                                                                                           ifelse(Infraorder!="ZZZ", Infraorder,
                                                                                                                                                  ifelse(Suborder!="ZZZ", Suborder,
                                                                                                                                                         ifelse(Order!="ZZZ", Order,
                                                                                                                                                                ifelse(Superorder!="ZZZ", Superorder,
                                                                                                                                                                       ifelse(Infraclass!="ZZZ", Infraclass,
                                                                                                                                                                              ifelse(Subclass!="ZZZ", Subclass,NA)))))))))))))))

OTU_correct_hier[which(OTU_correct_hier$Subclass=="Acari"),"Fine_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Subclass=="Acari"),], 
                                                                                        ifelse(Genus==Fine_STE, "Genus",
                                                                                               ifelse(Subtribe==Fine_STE, "Subtribe",
                                                                                                      ifelse(Genus.Group==Fine_STE, "Genus.Group",
                                                                                                             
                                                                                                             ifelse(Tribe==Fine_STE, "Tribe",
                                                                                                                    ifelse(Custom.Subfamily==Fine_STE, "Custom.Subfamily",
                                                                                                                           ifelse(Subfamily==Fine_STE, "Subfamily",
                                                                                                                                  ifelse(Family==Fine_STE, "Family",
                                                                                                                                         ifelse(Superfamily==Fine_STE, "Superfamily",
                                                                                                                                                ifelse(Infraorder==Fine_STE, "Infraorder",
                                                                                                                                                       ifelse(Suborder==Fine_STE, "Suborder",
                                                                                                                                                              ifelse(Order==Fine_STE, "Order",
                                                                                                                                                                     ifelse(Superorder==Fine_STE, "Superorder",
                                                                                                                                                                            ifelse(Infraclass==Fine_STE, "Infraclass",
                                                                                                                                                                                   ifelse(Subclass==Fine_STE, "Subclass",NA)))))))))))))))





OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),"Fine_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),], 
                                                                                 ifelse(Genus!="ZZZ", Genus,
                                                                                        ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                               ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                      
                                                                                                      ifelse(Tribe!="ZZZ", Tribe,
                                                                                                             ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                    ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                                           ifelse(Family!="ZZZ", Family,
                                                                                                                                  ifelse(Superfamily!="ZZZ", Superfamily,
                                                                                                                                         ifelse(Infraorder!="ZZZ", Infraorder,
                                                                                                                                                ifelse(Suborder!="ZZZ", Suborder,
                                                                                                                                                       ifelse(Order!="ZZZ", Order,
                                                                                                                                                              ifelse(Superorder!="ZZZ", Superorder,
                                                                                                                                                                     ifelse(Infraclass!="ZZZ", Infraclass,
                                                                                                                                                                            ifelse(Subclass!="ZZZ", Subclass,
                                                                                                                                                                                   ifelse(Class!="ZZZ", Class, NA))))))))))))))))

OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),"Fine_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Class=="Gastropoda"),], 
                                                                                      ifelse(Genus==Fine_STE, "Genus",
                                                                                             ifelse(Subtribe==Fine_STE, "Subtribe",
                                                                                                    ifelse(Genus.Group==Fine_STE, "Genus.Group",
                                                                                                           
                                                                                                           ifelse(Tribe==Fine_STE, "Tribe",
                                                                                                                  ifelse(Custom.Subfamily==Fine_STE, "Custom.Subfamily",
                                                                                                                         ifelse(Subfamily==Fine_STE, "Subfamily",  
                                                                                                                                ifelse(Family==Fine_STE, "Family",
                                                                                                                                       ifelse(Superfamily==Fine_STE, "Superfamily",
                                                                                                                                              ifelse(Infraorder==Fine_STE, "Infraorder",
                                                                                                                                                     ifelse(Suborder==Fine_STE, "Suborder",
                                                                                                                                                            ifelse(Order==Fine_STE, "Order",
                                                                                                                                                                   ifelse(Superorder==Fine_STE, "Superorder",
                                                                                                                                                                          ifelse(Infraclass==Fine_STE, "Infraclass",
                                                                                                                                                                                 ifelse(Subclass==Fine_STE, "Subclass",
                                                                                                                                                                                        ifelse(Class==Fine_STE, "Class", NA))))))))))))))))


OTU_correct_hier[which(OTU_correct_hier$Family=="Dytiscidae"),"Fine_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Dytiscidae"),], 
                                                                                  ifelse(Genus!="ZZZ", Genus,
                                                                                         ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                                ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                       
                                                                                                       ifelse(Tribe!="ZZZ", Tribe,
                                                                                                              ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                     ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                                            ifelse(Family!="ZZZ", Family,
                                                                                                                                   NA))))))))

OTU_correct_hier[which(OTU_correct_hier$Family=="Dytiscidae"),"Fine_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Dytiscidae"),], 
                                                                                       ifelse(Genus==Fine_STE, "Genus",
                                                                                              ifelse(Subtribe==Fine_STE, "Subtribe",
                                                                                                     ifelse(Genus.Group==Fine_STE, "Genus.Group",
                                                                                                            
                                                                                                            ifelse(Tribe==Fine_STE, "Tribe",
                                                                                                                   ifelse(Custom.Subfamily==Fine_STE, "Custom.Subfamily",
                                                                                                                          ifelse(Subfamily==Fine_STE, "Subfamily",  
                                                                                                                                 ifelse(Family==Fine_STE, "Family", NA))))))))

OTU_correct_hier[which(OTU_correct_hier$Family=="Simuliidae"),"Fine_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Simuliidae"),], 
                                                                                  ifelse(Genus!="ZZZ", Genus,
                                                                                         ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                                ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                       
                                                                                                       ifelse(Tribe!="ZZZ", Tribe,
                                                                                                              ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                     ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                                            ifelse(Family!="ZZZ", Family,
                                                                                                                                   NA))))))))

OTU_correct_hier[which(OTU_correct_hier$Family=="Simuliidae"),"Fine_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Simuliidae"),], 
                                                                                       ifelse(Genus==Fine_STE, "Genus",
                                                                                              ifelse(Subtribe==Fine_STE, "Subtribe",
                                                                                                     ifelse(Genus.Group==Fine_STE, "Genus.Group",
                                                                                                            
                                                                                                            ifelse(Tribe==Fine_STE, "Tribe",
                                                                                                                   ifelse(Custom.Subfamily==Fine_STE, "Custom.Subfamily",
                                                                                                                          ifelse(Subfamily==Fine_STE, "Subfamily",  
                                                                                                                                 ifelse(Family==Fine_STE, "Family", NA))))))))




OTU_correct_hier[which(OTU_correct_hier$Family=="Chironomidae"),"Fine_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Chironomidae"),], 
                                                                                  ifelse(Subspecies!="ZZZ", Subspecies,
                                                                                         ifelse( Species!="ZZZ", Species,
                                                                                                 ifelse(Subgenus!="ZZZ", Subgenus,
                                                                                                        ifelse(Species.Group!="ZZZ", Species.Group,
                                                                                                               ifelse(Genus!="ZZZ", Genus,
                                                                                                                      ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                                                             ifelse(Genus.Group!="ZZZ", Genus.Group,  
                                                                                  ifelse(Tribe!="ZZZ", Tribe,
                                                                                           ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                  ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                         ifelse(Family!="ZZZ", Family,
                                                                                                                NA))))))))))))

OTU_correct_hier[which(OTU_correct_hier$Family=="Chironomidae"),"Fine_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Family=="Chironomidae"),], 
                                                                                       ifelse(Subspecies==Fine_STE, "Subspecies",
                                                                                              ifelse(Species==Fine_STE, "Species",
                                                                                                     ifelse(Subgenus==Fine_STE, "Subgenus",  
                                                                                                            ifelse(Species.Group==Fine_STE, "Species.Group",
                                                                                                                   ifelse(Genus==Fine_STE, "Genus",
                                                                                                                          ifelse(Subtribe==Fine_STE, "Subtribe",
                                                                                                                                 ifelse(Genus.Group==Fine_STE, "Genus.Group",
                                                                                         ifelse(Tribe==Fine_STE, "Tribe",
                                                                                                ifelse(Custom.Subfamily==Fine_STE, "Custom.Subfamily",
                                                                                                       ifelse(Subfamily==Fine_STE, "Subfamily",  
                                                                                                              ifelse(Family==Fine_STE, "Family", NA))))))))))))




OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),"Fine_STE"]<-with(OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),], 
                                                                                ifelse(Subspecies!="ZZZ", Subspecies,
                                                                                       ifelse( Species!="ZZZ", Species,
                                                                                               ifelse(Subgenus!="ZZZ", Subgenus,
                                                                                                      ifelse(Species.Group!="ZZZ", Species.Group,
                                                                                  ifelse(Genus!="ZZZ", Genus,
                                                                                         ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                                ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                       
                                                                                                       ifelse(Tribe!="ZZZ", Tribe,
                                                                                                              ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                     ifelse(Subfamily!="ZZZ", Subfamily,     
                                                                                                                            ifelse(Family!="ZZZ", Family,
                                                                                                                                   ifelse(Superfamily!="ZZZ", Superfamily,
                                                                                                                                          ifelse(Infraorder!="ZZZ", Infraorder,
                                                                                                                                                 ifelse(Suborder!="ZZZ", Suborder,
                                                                                                                                                        ifelse(Order!="ZZZ", Order, NA))))))))))))))))
OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),"Fine_STE_rank"]<-with(OTU_correct_hier[which(OTU_correct_hier$Order=="Trichoptera"),], 
                                                                                     ifelse(Subspecies==Fine_STE, "Subspecies",
                                                                                            ifelse(Species==Fine_STE, "Species",
                                                                                                   ifelse(Subgenus==Fine_STE, "Subgenus",  
                                                                                                          ifelse(Species.Group==Fine_STE, "Species.Group",
                                                                                       ifelse(Genus==Fine_STE, "Genus",
                                                                                              ifelse(Subtribe==Fine_STE, "Subtribe",
                                                                                                     ifelse(Genus.Group==Fine_STE, "Genus.Group",
                                                                                                            ifelse(Tribe==Fine_STE, "Tribe",
                                                                                                                   ifelse(Custom.Subfamily==Fine_STE, "Custom.Subfamily",
                                                                                                                          ifelse(Subfamily==Fine_STE, "Subfamily",     
                                                                                                                                 ifelse(Family==Fine_STE, "Family",
                                                                                                                                        ifelse(Superfamily==Fine_STE, "Superfamily",
                                                                                                                                               ifelse(Infraorder==Fine_STE, "Infraorder",
                                                                                                                                                      ifelse(Suborder==Fine_STE, "Suborder",
                                                                                                                                                             ifelse(Order==Fine_STE, "Order", NA))))))))))))))))

OTU_correct_hier[which(is.na(OTU_correct_hier$Fine_STE)),"Fine_STE"]<-OTU_correct_hier[which(is.na(OTU_correct_hier$Fine_STE)),"OTU_MetricCalc"]


OTU_correct_hier[which(is.na(OTU_correct_hier$Fine_STE_rank)),"Fine_STE_rank"]<-with(OTU_correct_hier[which(is.na(OTU_correct_hier$Fine_STE_rank)),], 
                                                                                         ifelse(str_detect(Fine_STE, coll(Subspecies)), "Subspecies",
                                                                                                ifelse( str_detect(Fine_STE, coll(Species)), "Species",
                                                                                                        ifelse(str_detect(Fine_STE,coll(Subgenus)), "Subgenus",
                                                                                                               ifelse(str_detect(Fine_STE, coll(Species.Group )), "Species.Group",
                                                                                                                      ifelse(Genus==Fine_STE, "Genus",
                                                                                                                             ifelse(Subtribe==Fine_STE, "Subtribe",
                                                                                                                                    ifelse(Genus.Group==Fine_STE, "Genus.Group",
                                                                                                                                           ifelse(Tribe==Fine_STE, "Tribe",
                                                                                                                                                  ifelse(Custom.Subfamily==Fine_STE, "Custom.Subfamily",
                                                                                                                                                         ifelse(Subfamily==Fine_STE, "Subfamily",     
                                                                                                                                                                ifelse(Family==Fine_STE, "Family",
                                                                                                                                                                       ifelse(Superfamily==Fine_STE, "Superfamily",
                                                                                                                                                                              ifelse(Infraorder==Fine_STE, "Infraorder",
                                                                                                                                                                                     ifelse(Suborder==Fine_STE, "Suborder",
                                                                                                                                                                                            ifelse(Order==Fine_STE, "Order", 
                                                                                                                                                                                                   ifelse(Superorder==Fine_STE, "Superorder", 
                                                                                                                                                                                                          ifelse(Infraclass==Fine_STE, "Infraclass", 
                                                                                                                                                                                                                 ifelse(Subclass==Fine_STE, "Subclass", 
                                                                                                                                                                                                                        ifelse(Class==Fine_STE, "Class", 
                                                                                                                                                                                                                               ifelse(Superclass==Fine_STE, "Superclass", 
                                                                                                                                                                                                                                      ifelse(Subphylum==Fine_STE, "Subphylum", 
                                                                                                                                                                                                                                             ifelse(Phylum==Fine_STE, "Phylum",
                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                    NA)))))))))))))))))))))))
##Note to Sean, need to change species level for Perlinodes aureus and Matriella teresa in BCG table. Need something for Genus group Neaviperla/Suwallia besides tribe
test<-OTU_correct_hier[which(is.na(OTU_correct_hier$Fine_STE_rank)),"Fine_STE"]
#############################################
### where the Fine STE resolves to species through Species.Group, adjust the names to they match attribute tables correctly
OTU_correct_hier[OTU_correct_hier$Fine_STE_rank=="Species"& OTU_correct_hier$Fine_STE!=OTU_correct_hier$OTU_MetricCalc,"Fine_STE"]<-OTU_correct_hier[OTU_correct_hier$Fine_STE_rank=="Species"& OTU_correct_hier$Fine_STE!=OTU_correct_hier$OTU_MetricCalc,"OTU_MetricCalc"]
OTU_correct_hier[OTU_correct_hier$Fine_STE_rank=="Subspecies"& OTU_correct_hier$Fine_STE!=OTU_correct_hier$OTU_MetricCalc,"Fine_STE"]<-OTU_correct_hier[OTU_correct_hier$Fine_STE_rank=="Subpecies"& OTU_correct_hier$Fine_STE!=OTU_correct_hier$OTU_MetricCalc,"OTU_MetricCalc"]
OTU_correct_hier[OTU_correct_hier$Fine_STE_rank=="Subgenus"& OTU_correct_hier$Fine_STE!=OTU_correct_hier$OTU_MetricCalc,"Fine_STE"]<-OTU_correct_hier[OTU_correct_hier$Fine_STE_rank=="Subgenus"& OTU_correct_hier$Fine_STE!=OTU_correct_hier$OTU_MetricCalc,"OTU_MetricCalc"]
OTU_correct_hier[OTU_correct_hier$Fine_STE_rank=="Species.Group"& OTU_correct_hier$Fine_STE!=OTU_correct_hier$OTU_MetricCalc,"Fine_STE"]<-OTU_correct_hier[OTU_correct_hier$Fine_STE_rank=="Species.Group"& OTU_correct_hier$Fine_STE!=OTU_correct_hier$OTU_MetricCalc,"OTU_MetricCalc"]

##########Match Attributes to STEs#######

##read in PSSB attribute table
atts<-read.xlsx("G:/GreenWQA/Biota/Contracts/Bellevue/2023 Report/for trends analysis/2012_taxa_attributes.xlsx")
atts<-subset(atts, select=c(Taxon.Name, `2012.Clinger`, `2012.Intolerant`, `2012.Long.Lived`, `2012.Predator`, `2012.Tolerant`))


##Perform a series of rolling lookups to find a match in taxa hierarchies in the Taxon Name column of the 2012 attribute table, starting with direct match, then from lowest hierarchy to highest relative to the appropriate STE

attribs2<-data.frame(Taxon.Name=character(), Predator=character(), Long.Lived=character(), Tolerant=character(), Intolerant=character(), Clinger=character())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(5:26, 29, 30)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Coarse_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
for (i in 1:ncol(test)){
  k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
  if(k==24) next ## skip the rank column
  if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
  attribs<-merge(subset(atts, select=c("Taxon.Name", "2012.Clinger", "2012.Intolerant", "2012.Long.Lived", "2012.Predator", "2012.Tolerant")), test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
  names(attribs)<-str_replace(names(attribs), "2012.", "")
  names(attribs)<-str_replace(names(attribs), "Coarse_STE.1", "Coarse_STE")
  attribs2<-rbind(attribs, attribs2)
  test<-subset(test, !Coarse_STE %in% attribs2$Coarse_STE)
  idx<-which(names(test)[j]==test$Coarse_STE_rank)
}
  }

missing_coarse_attribs<-test
write.csv(missing_coarse_attribs, "missing_coarse_attribs_05102024.csv")
coarse_attributes<-attribs2
coarse_attributes<-unique(coarse_attributes)
names(coarse_attributes)[1]<-"Coarse_STE_attribute_use"


attribs2<-data.frame(Taxon.Name=character(), Predator=character(), Long.Lived=character(), Tolerant=character(), Intolerant=character(), Clinger=character())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(5:26, 31, 32)]


for (j in length(names(test)):1){
  if(j==24) next
  idx<-which(names(test)[j]==test$Medium_STE_rank)
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i
    if(k==24) next
    if(k>j&k<23) next
    attribs<-merge(subset(atts, select=c("Taxon.Name", "2012.Clinger", "2012.Intolerant", "2012.Long.Lived", "2012.Predator", "2012.Tolerant")), test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "2012.", "")
    names(attribs)<-str_replace(names(attribs), "Medium_STE.1", "Medium_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Medium_STE %in% attribs2$Medium_STE)
    idx<-which(names(test)[j]==test$Medium_STE_rank)
  }
}

missing_medium_attribs<-test
write.csv(missing_medium_attribs, "missing_medium_attribs_05102024.csv")
medium_attributes<-attribs2
medium_attributes<-unique(medium_attributes)
names(medium_attributes)[1]<-"Medium_STE_attribute_use"

attribs2<-data.frame(Taxon.Name=character(), Predator=character(), Long.Lived=character(), Tolerant=character(), Intolerant=character(), Clinger=character())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(5:26, 33, 34)]

for (j in length(names(test)):1){
  if(j==24) next
  idx<-which(names(test)[j]==test$Fine_STE_rank)
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i
    if(k==24) next
    if(k>j&k<23) next
    attribs<-merge(subset(atts, select=c("Taxon.Name", "2012.Clinger", "2012.Intolerant", "2012.Long.Lived", "2012.Predator", "2012.Tolerant")), test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "2012.", "")
    names(attribs)<-str_replace(names(attribs), "Fine_STE.1", "Fine_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Fine_STE %in% attribs2$Fine_STE)
    idx<-which(names(test)[j]==test$Fine_STE_rank)
  }
}

missing_Fine_attribs<-test
write.csv(missing_Fine_attribs, "missing_Fine_attribs_05102024.csv")
Fine_attributes<-attribs2
Fine_attributes<-unique(Fine_attributes)
names(Fine_attributes)[1]<-"Fine_STE_attribute_use"

OTU_correct_hier<-merge(OTU_correct_hier, subset(coarse_attributes, select=c(Coarse_STE_attribute_use, Coarse_STE)), all.x=T)
OTU_correct_hier<-merge(OTU_correct_hier, subset(medium_attributes, select=c(Medium_STE_attribute_use, Medium_STE)), all.x=T)
OTU_correct_hier<-merge(OTU_correct_hier, subset(Fine_attributes, select=c(Fine_STE_attribute_use, Fine_STE)), all.x=T)

att_no_match<-atts[!atts$Taxon.Name %in% OTU_correct_hier$Fine_STE_attribute_use,]
att_no_match<-att_no_match[!att_no_match$Taxon.Name %in% OTU_correct_hier$Medium_STE_attribute_use,]
att_no_match<-att_no_match[!att_no_match$Taxon.Name %in% OTU_correct_hier$Coarse_STE_attribute_use,]
write.csv(att_no_match, "attributes_wo_match_05102024.csv")
################## add HBI attributes############


  ##read in PSSB HBI attribute table
hbi<-read.xlsx("G:/GreenWQA/Biota/PSSB Puget Sound Stream Benthos/PSSB_HSBI.xlsx")
hbi<-subset(hbi, select=c(Taxon.Name, Hilsenhoff.Tolerance))


##Perform a series of rolling lookups to find a match in taxa hierarchies in the Taxon Name column of the HBI table, starting with direct match, then from lowest hierarchy to highest relative to the appropriate STE

attribs2<-data.frame(Taxon.Name=character(), HBI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 3, 32)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Coarse_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(hbi, test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Coarse_STE.1", "Coarse_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Coarse_STE %in% attribs2$Coarse_STE)
    idx<-which(names(test)[j]==test$Coarse_STE_rank)
  }
}

missing_coarse_attribs<-test
write.csv(missing_coarse_attribs, "missing_coarse_hbi_05102024.csv")
coarse_attributes<-attribs2
coarse_attributes<-unique(coarse_attributes)
names(coarse_attributes)[1]<-"Coarse_STE_HBI_use"

attribs2<-data.frame(Taxon.Name=character(), HBI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 2, 33)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Medium_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(hbi, test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Medium_STE.1", "Medium_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Medium_STE %in% attribs2$Medium_STE)
    idx<-which(names(test)[j]==test$Medium_STE_rank)
  }
}

missing_Medium_attribs<-test
write.csv(missing_Medium_attribs, "missing_medium_hbi_05102024.csv")
Medium_attributes<-attribs2
Medium_attributes<-unique(Medium_attributes)
names(Medium_attributes)[1]<-"Medium_STE_HBI_use"

attribs2<-data.frame(Taxon.Name=character(), HBI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 1, 34)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Fine_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(hbi, test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Fine_STE.1", "Fine_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Fine_STE %in% attribs2$Fine_STE)
    idx<-which(names(test)[j]==test$Fine_STE_rank)
  }
}

missing_Fine_attribs<-test
write.csv(missing_Fine_attribs, "missing_fine_hbi_05102024.csv")
Fine_attributes<-attribs2
Fine_attributes<-unique(Fine_attributes)
names(Fine_attributes)[1]<-"Fine_STE_HBI_use"

OTU_correct_hier<-merge(OTU_correct_hier, subset(coarse_attributes, select=c(Coarse_STE_HBI_use, Coarse_STE)), all.x=T)
OTU_correct_hier<-merge(OTU_correct_hier, subset(Medium_attributes, select=c(Medium_STE_HBI_use, Medium_STE)), all.x=T)
OTU_correct_hier<-merge(OTU_correct_hier, subset(Fine_attributes, select=c(Fine_STE_HBI_use, Fine_STE)), all.x=T)

hbi_no_match<-hbi[!hbi$Taxon.Name %in% OTU_correct_hier$Fine_STE_HBI_use,]
hbi_no_match<-hbi_no_match[!hbi_no_match$Taxon.Name %in% OTU_correct_hier$Medium_STE_HBI_use,]
hbi_no_match<-hbi_no_match[!hbi_no_match$Taxon.Name %in% OTU_correct_hier$Coarse_STE_HBI_use,]
write.csv(hbi_no_match, "HBIT_wo_match_05102024.csv")
################## add MTI attributes############


##read in PSSB MTI attribute table
mti<-read.xlsx("G:/GreenWQA/Biota/PSSB Puget Sound Stream Benthos/PSSB_MTI.xlsx")
mti<-subset(mti, select=c(Taxon.Name, Metals.Tolerance))


##Perform a series of rolling lookups to find a match in taxa hierarchies in the Taxon Name column of the HBI table, starting with direct match, then from lowest hierarchy to highest relative to the appropriate STE

attribs2<-data.frame(Taxon.Name=character(), MTI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 3, 32)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Coarse_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(mti, test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Coarse_STE.1", "Coarse_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Coarse_STE %in% attribs2$Coarse_STE)
    idx<-which(names(test)[j]==test$Coarse_STE_rank)
  }
}

missing_coarse_attribs<-test
write.csv(missing_coarse_attribs, "missing_coarse_MTI_05102024.csv")
coarse_attributes<-attribs2
coarse_attributes<-unique(coarse_attributes)
names(coarse_attributes)[1]<-"Coarse_STE_MTI_use"

attribs2<-data.frame(Taxon.Name=character(), MTI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 2, 33)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Medium_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(mti, test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Medium_STE.1", "Medium_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Medium_STE %in% attribs2$Medium_STE)
    idx<-which(names(test)[j]==test$Medium_STE_rank)
  }
}

missing_Medium_attribs<-test
write.csv(missing_Medium_attribs, "missing_medium_MTI_05102024.csv")
Medium_attributes<-attribs2
Medium_attributes<-unique(Medium_attributes)
names(Medium_attributes)[1]<-"Medium_STE_MTI_use"

attribs2<-data.frame(Taxon.Name=character(), MTI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 1, 34)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Fine_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(mti, test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Fine_STE.1", "Fine_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Fine_STE %in% attribs2$Fine_STE)
    idx<-which(names(test)[j]==test$Fine_STE_rank)
  }
}

missing_Fine_attribs<-test
write.csv(missing_Fine_attribs, "missing_fine_MTI_05102024.csv")
Fine_attributes<-attribs2
Fine_attributes<-unique(Fine_attributes)
names(Fine_attributes)[1]<-"Fine_STE_MTI_use"

OTU_correct_hier<-merge(OTU_correct_hier, subset(coarse_attributes, select=c(Coarse_STE_MTI_use, Coarse_STE)), all.x=T)
OTU_correct_hier<-merge(OTU_correct_hier, subset(Medium_attributes, select=c(Medium_STE_MTI_use, Medium_STE)), all.x=T)
OTU_correct_hier<-merge(OTU_correct_hier, subset(Fine_attributes, select=c(Fine_STE_MTI_use, Fine_STE)), all.x=T)

mti_no_match<-mti[!mti$Taxon.Name %in% OTU_correct_hier$Fine_STE_MTI_use,]
mti_no_match<-mti_no_match[!mti_no_match$Taxon.Name %in% OTU_correct_hier$Medium_STE_MTI_use,]
mti_no_match<-mti_no_match[!mti_no_match$Taxon.Name %in% OTU_correct_hier$Coarse_STE_MTI_use,]
write.csv(mti_no_match, "MTI_wo_match_05102024.csv")
################## add FSBI attributes############


##read in PSSB MTI attribute table
fsbi<-read.xlsx("G:/GreenWQA/Biota/PSSB Puget Sound Stream Benthos/PSSB_FSBI.xlsx")
fsbi<-subset(fsbi, select=c(Taxon.Name, Fine.Sediment.Sensitivity))


##Perform a series of rolling lookups to find a match in taxa hierarchies in the Taxon Name column of the HBI table, starting with direct match, then from lowest hierarchy to highest relative to the appropriate STE

attribs2<-data.frame(Taxon.Name=character(), FSBI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 3, 32)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Coarse_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(fsbi, test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Coarse_STE.1", "Coarse_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Coarse_STE %in% attribs2$Coarse_STE)
    idx<-which(names(test)[j]==test$Coarse_STE_rank)
  }
}

missing_coarse_attribs<-test
write.csv(missing_coarse_attribs, "missing_coarse_FSBI_05102024.csv")
coarse_attributes<-attribs2
coarse_attributes<-unique(coarse_attributes)
names(coarse_attributes)[1]<-"Coarse_STE_FSBI_use"

attribs2<-data.frame(Taxon.Name=character(), FSBI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 2, 33)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Medium_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(fsbi, test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Medium_STE.1", "Medium_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Medium_STE %in% attribs2$Medium_STE)
    idx<-which(names(test)[j]==test$Medium_STE_rank)
  }
}

missing_Medium_attribs<-test
write.csv(missing_Medium_attribs, "missing_medium_FSBI_05102024.csv")
Medium_attributes<-attribs2
Medium_attributes<-unique(Medium_attributes)
names(Medium_attributes)[1]<-"Medium_STE_FSBI_use"

attribs2<-data.frame(Taxon.Name=character(), FSBI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 1, 34)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Fine_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(fsbi, test[idx, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Fine_STE.1", "Fine_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Fine_STE %in% attribs2$Fine_STE)
    idx<-which(names(test)[j]==test$Fine_STE_rank)
  }
}

missing_Fine_attribs<-test
write.csv(missing_Fine_attribs, "missing_fine_FSBI_05102024.csv")
Fine_attributes<-attribs2
Fine_attributes<-unique(Fine_attributes)
names(Fine_attributes)[1]<-"Fine_STE_FSBI_use"

OTU_correct_hier<-merge(OTU_correct_hier, subset(coarse_attributes, select=c(Coarse_STE_FSBI_use, Coarse_STE)), all.x=T)
OTU_correct_hier<-merge(OTU_correct_hier, subset(Medium_attributes, select=c(Medium_STE_FSBI_use, Medium_STE)), all.x=T)
OTU_correct_hier<-merge(OTU_correct_hier, subset(Fine_attributes, select=c(Fine_STE_FSBI_use, Fine_STE)), all.x=T)

fsbi_no_match<-fsbi[!fsbi$Taxon.Name %in% OTU_correct_hier$Fine_STE_FSBI_use,]
fsbi_no_match<-fsbi_no_match[!fsbi_no_match$Taxon.Name %in% OTU_correct_hier$Medium_STE_FSBI_use,]
fsbi_no_match<-fsbi_no_match[!fsbi_no_match$Taxon.Name %in% OTU_correct_hier$Coarse_STE_FSBI_use,]
write.csv(fsbi_no_match, "FSBI_wo_match_05102024.csv")
################## add FSBI attributes############


##read in PSSB FSBI attribute table based on Relyea 2012 directly
fsbi<-read.xlsx("C:/Users/esosik/King County/DNRP - Science TCA Unit - Bug Monitoring/Sediment Index Exploration/Dianes_Exploration_FSBI_Bio.Infer_SpringWinter2022/Project Excels/FSBI_Index.xlsx")
fsbi<-subset(fsbi, select=c(Taxon, FSBI_Score))


##Perform a series of rolling lookups to find a match in taxa hierarchies in the Taxon Name column of the HBI table, starting with direct match, then from lowest hierarchy to highest relative to the appropriate STE

attribs2<-data.frame(Taxon=character(), FSBI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 3, 32)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Coarse_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(fsbi, test[idx, c(k, 23)], by.x="Taxon", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Coarse_STE.1", "Coarse_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Coarse_STE %in% attribs2$Coarse_STE)
    idx<-which(names(test)[j]==test$Coarse_STE_rank)
  }
}

missing_coarse_attribs<-test
write.csv(missing_coarse_attribs, "missing_coarse_Relyea_FSBI_05102024.csv")
coarse_attributes<-attribs2
coarse_attributes<-unique(coarse_attributes)
names(coarse_attributes)[1]<-"Coarse_STE_FSBI_use2"

attribs2<-data.frame(Taxon.Name=character(), FSBI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 2, 33)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Medium_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(fsbi, test[idx, c(k, 23)], by.x="Taxon", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Medium_STE.1", "Medium_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Medium_STE %in% attribs2$Medium_STE)
    idx<-which(names(test)[j]==test$Medium_STE_rank)
  }
}

missing_Medium_attribs<-test
write.csv(missing_Medium_attribs, "missing_medium_Relyea_FSBI_05102024.csv")
Medium_attributes<-attribs2
Medium_attributes<-unique(Medium_attributes)
names(Medium_attributes)[1]<-"Medium_STE_FSBI_use2"

attribs2<-data.frame(Taxon.Name=character(), FSBI=numeric())
names(OTU_correct_hier)
test<-OTU_correct_hier[,c(8:29, 1, 34)]


for (j in length(names(test)):1){
  if(j==24) next## skip the rank column
  idx<-which(names(test)[j]==test$Fine_STE_rank) ##get the index numbers for rows where the "XX_STE_rank" is equal to the j-th hierarchy name (i.e. pull all rows where STE rank is "Genus")
  for (i in 1:ncol(test)){
    k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
    if(k==24) next ## skip the rank column
    if(k>j&k<23) next ##skip levels that are finer than the "rank to use", but don't skip "XX_STE". Since "Species", "SubSpecies", "subgenus" and "Species.group" only provide partial names, matching by "XX_STE" for these provides complete names
    attribs<-merge(fsbi, test[idx, c(k, 23)], by.x="Taxon", by.y=names(test[k]))
    names(attribs)<-str_replace(names(attribs), "Fine_STE.1", "Fine_STE")
    attribs2<-rbind(attribs, attribs2)
    test<-subset(test, !Fine_STE %in% attribs2$Fine_STE)
    idx<-which(names(test)[j]==test$Fine_STE_rank)
  }
}

missing_Fine_attribs<-test
write.csv(missing_Fine_attribs, "missing_fine_Relyea_FSBI_05102024.csv")
Fine_attributes<-attribs2
Fine_attributes<-unique(Fine_attributes)
names(Fine_attributes)[1]<-"Fine_STE_FSBI_use2"

OTU_correct_hier<-merge(OTU_correct_hier, subset(coarse_attributes, select=c(Coarse_STE_FSBI_use2, Coarse_STE)), all.x=T)
OTU_correct_hier<-merge(OTU_correct_hier, subset(Medium_attributes, select=c(Medium_STE_FSBI_use2, Medium_STE)), all.x=T)
OTU_correct_hier<-merge(OTU_correct_hier, subset(Fine_attributes, select=c(Fine_STE_FSBI_use2, Fine_STE)), all.x=T)

fsbi_no_match2<-fsbi[!fsbi$Taxon %in% OTU_correct_hier$Fine_STE_FSBI_use2,]
fsbi_no_match2<-fsbi_no_match2[!fsbi_no_match2$Taxon %in% OTU_correct_hier$Medium_STE_FSBI_use2,]
fsbi_no_match2<-fsbi_no_match2[!fsbi_no_match2$Taxon %in% OTU_correct_hier$Coarse_STE_FSBI_use2,]
write.csv(fsbi_no_match2, "Relyea_FSBI_wo_match_05102024.csv")
###PSSB FSBI list seems to be missing some taxa, and at least one taxa has FSBI attribute in PSSB that does not appear in Relyea et al 2012.
##Need to update some names in the Relyea 2012 list to match names more exactly (i.e. ... Zapada oregonensis group)
fsbi_no_match2[!fsbi_no_match2$Taxon %in% fsbi_no_match$Taxon.Name,]
fsbi_no_match[!fsbi_no_match$Taxon.Name %in% fsbi_no_match2$Taxon,]
write.csv(fsbi_no_match[!fsbi_no_match$Taxon.Name %in% fsbi_no_match2$Taxon,], "PSSB_FSBI_not_in_Relyea_05102024.csv")

fsbi_no_match2[!fsbi_no_match2$Taxon %in% fsbi_no_match$Taxon.Name& fsbi_no_match2$FSBI_Score!=0,]
write.csv(fsbi_no_match2[!fsbi_no_match2$Taxon %in% fsbi_no_match$Taxon.Name& fsbi_no_match2$FSBI_Score!=0,], "Relyea_FSBI_not_in_PSSB_05102024.csv")

getwd()
setwd("C:/Users/esosik/King County/DNRP - Science TCA Unit - Bug Monitoring/PSSB issues/STE_attrib_updates")
OTU_correct_hier[OTU_correct_hier=="ZZZ"]<-""
write.csv(OTU_correct_hier, "updated_master_list_draft_05102024.csv")
