library(openxlsx)
library(stringr)

setwd("C:/Users/esosik/King County/DNRP - Science TCA Unit - Bug Monitoring/PSSB issues/STE_attrib_updates")


PSSBmapping<-read.xlsx("STE_Mapping_Atts_Draft.xlsx", detectDates = T, sheet="Draft Mapping")#load the mapping used by PSSB

PSSBSTE<-read.xlsx("STE_Mapping_Atts_Draft.xlsx", detectDates = T, sheet="Draft STE Combined")#load the STE used by PSSB

##this function binds the textfiles from the taxa downloads from PSSB into one dataframe
taxaBind <- function(file.path) {
  
  path.files <- list.files(file.path)
  # read in files
  list.with.each.file <- lapply(paste(file.path, list.files(file.path), sep = ''), function(y) read.delim(y, header=TRUE))
  taxa<-do.call("rbind.data.frame", list.with.each.file)
  return(taxa)
  
  
}

file.path="./PSSB_all_data/" ##data downloaded and current as of 5/15/2024-- all PSSB rivers and streams data
raw<-taxaBind(file.path)

names(raw)
PSSB_taxa<-unique(raw[,c(28, 29, 48:69)])
##there are some repeat entries that somewhere in the hierarchy have an NA instead of "". This yields multiples of the same taxa. Fix this.
PSSB_taxa[is.na(PSSB_taxa)]<-""
PSSB_taxa<-unique(PSSB_taxa) #we're generating a list of all taxa in PSSB samples


###we want to see if any of the mapped taxa names need an STE update too
PSSBmapping<-merge(PSSBmapping, PSSB_taxa, by.x=c("Taxon"), by.y=c("Taxon")) #merge the PSSB taxa list iwth the mapping dataframe
names(PSSBmapping)
PSSBmapping<-PSSBmapping[,c(4, 3, 8:29)]
missingmappedSTE<-PSSBmapping[!PSSBmapping$OTU_MetricCalc %in% PSSBSTE$Taxon,]#look for mapped taxa that aren't in the STE lookup
missingmappedSTE<-subset(missingmappedSTE, !is.na(OTU_MetricCalc))#screen out taxa that don't have a map assigned.

missingSTE<-PSSB_taxa[!PSSB_taxa$Taxon %in% PSSBSTE$Taxon,]##look for taxa in PSSB samples that are not in the STE lookup table. We'll need to check these again after the STE rules are manually applied-- if a higher taxonomic level applying the correct rules is in the STE table, no modifications to the STE lookup in PSSB are needed. 

##in the code below, we'll screen out excluded taxa, and apply the rules of Coarse, Medium and Fine STEs. Then we'll look if the "rolled up" taxa are in the STE table. 
names(missingSTE)
names(missingmappedSTE)
names(missingmappedSTE)<-names(missingSTE)
missingSTE<-rbind(missingSTE, missingmappedSTE)
###################Screen out exclusions#######################

##read in PSSB exclusion table
exlude<-read.xlsx("G:/GreenWQA/Biota/PSSB Puget Sound Stream Benthos/PSSB_exclusions.xlsx")
exlude<-subset(exlude, select=c(Taxon.Name, Excluded))


##Perform a series of rolling lookups to find a match in taxa hierarchies in the Taxon Name column of the exclusion table, starting with direct match, then from lowest hierarchy to highest

exlude2<-data.frame(Taxon.Name=character(), Excluded=character())
names(missingSTE)
test<-missingSTE[,c(3:24, 2)]
for (i in 1:ncol(test)){
  k<-(ncol(test)+1)-i ##work backwards through the hierarchy columns
  attribs<-merge(exlude, test[, c(k, 23)], by.x="Taxon.Name", by.y=names(test[k]))
  # names(attribs)<-str_replace(names(attribs), "2012.", "")
  names(attribs)<-str_replace(names(attribs), "Taxon_1", "Taxon")
  exlude2<-rbind(attribs, exlude2)
  test<-subset(test, !Taxon %in% exlude2$Taxon)
}

exlude2<-unique(exlude2)
names(exlude2)[1]<-"Exclusion_reason"
missingSTE<-merge(missingSTE, exlude2, all.x=T)
missingSTE<-subset(missingSTE, is.na(Excluded)|Excluded==F)

######################## Coarse STE roll-up########################
missingSTE[which(missingSTE$Subclass=="Oligochaeta"),"Coarse_STE"]<-"Oligochaeta"
missingSTE[which(missingSTE$Subclass=="Oligochaeta"),"Coarse_STE_rank"]<-"Subclass"

missingSTE[which(missingSTE$Subclass=="Acari"),"Coarse_STE"]<-"Acari"
missingSTE[which(missingSTE$Subclass=="Acari"),"Coarse_STE_rank"]<-"Subclass"

missingSTE[which(missingSTE$Class=="Gastropoda"),"Coarse_STE"]<-with(missingSTE[which(missingSTE$Class=="Gastropoda"),], 
                                                                                 ifelse(Family!="", Family,
                                                                                        ifelse(Superfamily!="", Superfamily,
                                                                                               ifelse(Infraorder!="", Infraorder,
                                                                                                      ifelse(Suborder!="", Suborder,
                                                                                                             ifelse(Order!="", Order,
                                                                                                                    ifelse(Superorder!="", Superorder,
                                                                                                                           ifelse(Infraclass!="", Infraclass,
                                                                                                                                  ifelse(Subclass!="", Subclass,
                                                                                                                                         ifelse(Class!="", Class, NA))))))))))

missingSTE[which(missingSTE$Class=="Gastropoda"),"Coarse_STE_rank"]<-with(missingSTE[which(missingSTE$Class=="Gastropoda"),], 
                                                                                      ifelse(Family==Coarse_STE, "Family",
                                                                                             ifelse(Superfamily==Coarse_STE, "Superfamily",
                                                                                                    ifelse(Infraorder==Coarse_STE, "Infraorder",
                                                                                                           ifelse(Suborder==Coarse_STE, "Suborder",
                                                                                                                  ifelse(Order==Coarse_STE, "Order",
                                                                                                                         ifelse(Superorder==Coarse_STE, "Superorder",
                                                                                                                                ifelse(Infraclass==Coarse_STE, "Infraclass",
                                                                                                                                       ifelse(Subclass==Coarse_STE, "Subclass",
                                                                                                                                              ifelse(Class==Coarse_STE, "Class", NA))))))))))

missingSTE[which(missingSTE$Family=="Dytiscidae"),"Coarse_STE"]<-"Dytiscidae"
missingSTE[which(missingSTE$Family=="Dytiscidae"),"Coarse_STE_rank"]<-"Family"

missingSTE[which(missingSTE$Family=="Simuliidae"),"Coarse_STE"]<-"Simuliidae"
missingSTE[which(missingSTE$Family=="Simuliidae"),"Coarse_STE_rank"]<-"Family"

missingSTE[which(missingSTE$Family=="Chironomidae"),"Coarse_STE"]<-"Chironomidae"
missingSTE[which(missingSTE$Family=="Chironomidae"),"Coarse_STE_rank"]<-"Family"

missingSTE[which(missingSTE$Order=="Trichoptera"),"Coarse_STE"]<-with(missingSTE[which(missingSTE$Order=="Trichoptera"),], 
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
missingSTE[which(missingSTE$Order=="Trichoptera"),"Coarse_STE_rank"]<-with(missingSTE[which(missingSTE$Order=="Trichoptera"),], 
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

missingSTE[which(is.na(missingSTE$Coarse_STE)),"Coarse_STE"]<-missingSTE[which(is.na(missingSTE$Coarse_STE)),"Taxon"]
missingSTE[missingSTE==""]<-"ZZZ"

missingSTE[which(is.na(missingSTE$Coarse_STE_rank)),"Coarse_STE_rank"]<-with(missingSTE[which(is.na(missingSTE$Coarse_STE_rank)),], 
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

test<-missingSTE[which(is.na(missingSTE$Coarse_STE_rank)),]


#############################################
######################## Medium STE roll-up###############
missingSTE[which(missingSTE$Subclass=="Oligochaeta"),"Medium_STE"]<-with(missingSTE[which(missingSTE$Subclass=="Oligochaeta"),], 
                                                                                     ifelse(Family!="ZZZ", Family,
                                                                                            ifelse(Superfamily!="ZZZ", Superfamily,
                                                                                                   ifelse(Infraorder!="ZZZ", Infraorder,
                                                                                                          ifelse(Suborder!="ZZZ", Suborder,
                                                                                                                 ifelse(Order!="ZZZ", Order,
                                                                                                                        ifelse(Superorder!="ZZZ", Superorder,
                                                                                                                               ifelse(Infraclass!="ZZZ", Infraclass,
                                                                                                                                      ifelse(Subclass!="ZZZ", Subclass,NA)))))))))
missingSTE[which(missingSTE$Subclass=="Oligochaeta"),"Medium_STE_rank"]<-with(missingSTE[which(missingSTE$Subclass=="Oligochaeta"),], 
                                                                                          ifelse(Family==Medium_STE, "Family",
                                                                                                 ifelse(Superfamily==Medium_STE, "Superfamily",
                                                                                                        ifelse(Infraorder==Medium_STE, "Infraorder",
                                                                                                               ifelse(Suborder==Medium_STE, "Suborder",
                                                                                                                      ifelse(Order==Medium_STE, "Order",
                                                                                                                             ifelse(Superorder==Medium_STE, "Superorder",
                                                                                                                                    ifelse(Infraclass==Medium_STE, "Infraclass",
                                                                                                                                           ifelse(Subclass==Medium_STE, "Subclass",NA)))))))))

missingSTE[which(missingSTE$Subclass=="Acari"),"Medium_STE"]<-"Acari"
missingSTE[which(missingSTE$Subclass=="Acari"),"Medium_STE_rank"]<-"Subclass"

missingSTE[which(missingSTE$Class=="Gastropoda"),"Medium_STE"]<-with(missingSTE[which(missingSTE$Class=="Gastropoda"),], 
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

missingSTE[which(missingSTE$Class=="Gastropoda"),"Medium_STE_rank"]<-with(missingSTE[which(missingSTE$Class=="Gastropoda"),], 
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


missingSTE[which(missingSTE$Family=="Dytiscidae"),"Medium_STE"]<-with(missingSTE[which(missingSTE$Family=="Dytiscidae"),], 
                                                                                  ifelse(Genus!="ZZZ", Genus,
                                                                                         ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                                ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                       
                                                                                                       ifelse(Tribe!="ZZZ", Tribe,
                                                                                                              ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                     ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                                            ifelse(Family!="ZZZ", Family,
                                                                                                                                   NA))))))))

missingSTE[which(missingSTE$Family=="Dytiscidae"),"Medium_STE_rank"]<-with(missingSTE[which(missingSTE$Family=="Dytiscidae"),], 
                                                                                       ifelse(Genus==Medium_STE, "Genus",
                                                                                              ifelse(Subtribe==Medium_STE, "Subtribe",
                                                                                                     ifelse(Genus.Group==Medium_STE, "Genus.Group",
                                                                                                            
                                                                                                            ifelse(Tribe==Medium_STE, "Tribe",
                                                                                                                   ifelse(Custom.Subfamily==Medium_STE, "Custom.Subfamily",
                                                                                                                          ifelse(Subfamily==Medium_STE, "Subfamily",  
                                                                                                                                 ifelse(Family==Medium_STE, "Family", NA))))))))

missingSTE[which(missingSTE$Family=="Simuliidae"),"Medium_STE"]<-with(missingSTE[which(missingSTE$Family=="Simuliidae"),], 
                                                                                  ifelse(Genus!="ZZZ", Genus,
                                                                                         ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                                ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                       
                                                                                                       ifelse(Tribe!="ZZZ", Tribe,
                                                                                                              ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                     ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                                            ifelse(Family!="ZZZ", Family,
                                                                                                                                   NA))))))))

missingSTE[which(missingSTE$Family=="Simuliidae"),"Medium_STE_rank"]<-with(missingSTE[which(missingSTE$Family=="Simuliidae"),], 
                                                                                       ifelse(Genus==Medium_STE, "Genus",
                                                                                              ifelse(Subtribe==Medium_STE, "Subtribe",
                                                                                                     ifelse(Genus.Group==Medium_STE, "Genus.Group",
                                                                                                            
                                                                                                            ifelse(Tribe==Medium_STE, "Tribe",
                                                                                                                   ifelse(Custom.Subfamily==Medium_STE, "Custom.Subfamily",
                                                                                                                          ifelse(Subfamily==Medium_STE, "Subfamily",  
                                                                                                                                 ifelse(Family==Medium_STE, "Family", NA))))))))




missingSTE[which(missingSTE$Family=="Chironomidae"),"Medium_STE"]<-with(missingSTE[which(missingSTE$Family=="Chironomidae"),], 
                                                                                    ifelse(Tribe!="ZZZ", Tribe,
                                                                                           ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                  ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                         ifelse(Family!="ZZZ", Family,
                                                                                                                NA)))))

missingSTE[which(missingSTE$Family=="Chironomidae"),"Medium_STE_rank"]<-with(missingSTE[which(missingSTE$Family=="Chironomidae"),], 
                                                                                         ifelse(Tribe==Medium_STE, "Tribe",
                                                                                                ifelse(Custom.Subfamily==Medium_STE, "Custom.Subfamily",
                                                                                                       ifelse(Subfamily==Medium_STE, "Subfamily",  
                                                                                                              ifelse(Family==Medium_STE, "Family", NA)))))




missingSTE[which(missingSTE$Order=="Trichoptera"),"Medium_STE"]<-with(missingSTE[which(missingSTE$Order=="Trichoptera"),], 
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
missingSTE[which(missingSTE$Order=="Trichoptera"),"Medium_STE_rank"]<-with(missingSTE[which(missingSTE$Order=="Trichoptera"),], 
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

missingSTE[which(is.na(missingSTE$Medium_STE)),"Medium_STE"]<-missingSTE[which(is.na(missingSTE$Medium_STE)),"Taxon"]


missingSTE[which(is.na(missingSTE$Medium_STE_rank)),"Medium_STE_rank"]<-with(missingSTE[which(is.na(missingSTE$Medium_STE_rank)),], 
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

test<-missingSTE[which(is.na(missingSTE$Medium_STE_rank)),]
###############################################
######################## Fine STE roll-up###############
missingSTE[which(missingSTE$Subclass=="Oligochaeta"),"Fine_STE"]<-with(missingSTE[which(missingSTE$Subclass=="Oligochaeta"),], 
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

missingSTE[which(missingSTE$Subclass=="Oligochaeta"),"Fine_STE_rank"]<-with(missingSTE[which(missingSTE$Subclass=="Oligochaeta"),], 
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

missingSTE[which(missingSTE$Subclass=="Acari"),"Fine_STE"]<-with(missingSTE[which(missingSTE$Subclass=="Acari"),], 
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

missingSTE[which(missingSTE$Subclass=="Acari"),"Fine_STE_rank"]<-with(missingSTE[which(missingSTE$Subclass=="Acari"),], 
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





missingSTE[which(missingSTE$Class=="Gastropoda"),"Fine_STE"]<-with(missingSTE[which(missingSTE$Class=="Gastropoda"),], 
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

missingSTE[which(missingSTE$Class=="Gastropoda"),"Fine_STE_rank"]<-with(missingSTE[which(missingSTE$Class=="Gastropoda"),], 
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


missingSTE[which(missingSTE$Family=="Dytiscidae"),"Fine_STE"]<-with(missingSTE[which(missingSTE$Family=="Dytiscidae"),], 
                                                                                ifelse(Genus!="ZZZ", Genus,
                                                                                       ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                              ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                     
                                                                                                     ifelse(Tribe!="ZZZ", Tribe,
                                                                                                            ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                   ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                                          ifelse(Family!="ZZZ", Family,
                                                                                                                                 NA))))))))

missingSTE[which(missingSTE$Family=="Dytiscidae"),"Fine_STE_rank"]<-with(missingSTE[which(missingSTE$Family=="Dytiscidae"),], 
                                                                                     ifelse(Genus==Fine_STE, "Genus",
                                                                                            ifelse(Subtribe==Fine_STE, "Subtribe",
                                                                                                   ifelse(Genus.Group==Fine_STE, "Genus.Group",
                                                                                                          
                                                                                                          ifelse(Tribe==Fine_STE, "Tribe",
                                                                                                                 ifelse(Custom.Subfamily==Fine_STE, "Custom.Subfamily",
                                                                                                                        ifelse(Subfamily==Fine_STE, "Subfamily",  
                                                                                                                               ifelse(Family==Fine_STE, "Family", NA))))))))

missingSTE[which(missingSTE$Family=="Simuliidae"),"Fine_STE"]<-with(missingSTE[which(missingSTE$Family=="Simuliidae"),], 
                                                                                ifelse(Genus!="ZZZ", Genus,
                                                                                       ifelse(Subtribe!="ZZZ", Subtribe,
                                                                                              ifelse(Genus.Group!="ZZZ", Genus.Group,
                                                                                                     
                                                                                                     ifelse(Tribe!="ZZZ", Tribe,
                                                                                                            ifelse(Custom.Subfamily!="ZZZ", Custom.Subfamily,
                                                                                                                   ifelse(Subfamily!="ZZZ", Subfamily,  
                                                                                                                          ifelse(Family!="ZZZ", Family,
                                                                                                                                 NA))))))))

missingSTE[which(missingSTE$Family=="Simuliidae"),"Fine_STE_rank"]<-with(missingSTE[which(missingSTE$Family=="Simuliidae"),], 
                                                                                     ifelse(Genus==Fine_STE, "Genus",
                                                                                            ifelse(Subtribe==Fine_STE, "Subtribe",
                                                                                                   ifelse(Genus.Group==Fine_STE, "Genus.Group",
                                                                                                          
                                                                                                          ifelse(Tribe==Fine_STE, "Tribe",
                                                                                                                 ifelse(Custom.Subfamily==Fine_STE, "Custom.Subfamily",
                                                                                                                        ifelse(Subfamily==Fine_STE, "Subfamily",  
                                                                                                                               ifelse(Family==Fine_STE, "Family", NA))))))))




missingSTE[which(missingSTE$Family=="Chironomidae"),"Fine_STE"]<-with(missingSTE[which(missingSTE$Family=="Chironomidae"),], 
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

missingSTE[which(missingSTE$Family=="Chironomidae"),"Fine_STE_rank"]<-with(missingSTE[which(missingSTE$Family=="Chironomidae"),], 
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




missingSTE[which(missingSTE$Order=="Trichoptera"),"Fine_STE"]<-with(missingSTE[which(missingSTE$Order=="Trichoptera"),], 
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
missingSTE[which(missingSTE$Order=="Trichoptera"),"Fine_STE_rank"]<-with(missingSTE[which(missingSTE$Order=="Trichoptera"),], 
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

missingSTE[which(is.na(missingSTE$Fine_STE)),"Fine_STE"]<-missingSTE[which(is.na(missingSTE$Fine_STE)),"Taxon"]


missingSTE[which(is.na(missingSTE$Fine_STE_rank)),"Fine_STE_rank"]<-with(missingSTE[which(is.na(missingSTE$Fine_STE_rank)),], 
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

test<-missingSTE[which(is.na(missingSTE$Fine_STE_rank)),"Fine_STE"]
#############################################
### where the Fine STE resolves to species through Species.Group, adjust the names to they match attribute tables correctly
missingSTE[missingSTE$Fine_STE_rank=="Species"& missingSTE$Fine_STE!=missingSTE$Taxon,"Fine_STE"]<-missingSTE[missingSTE$Fine_STE_rank=="Species"& missingSTE$Fine_STE!=missingSTE$Taxon,"Taxon"]
missingSTE[missingSTE$Fine_STE_rank=="Subspecies"& missingSTE$Fine_STE!=missingSTE$Taxon,"Fine_STE"]<-missingSTE[missingSTE$Fine_STE_rank=="Subpecies"& missingSTE$Fine_STE!=missingSTE$Taxon,"Taxon"]
missingSTE[missingSTE$Fine_STE_rank=="Subgenus"& missingSTE$Fine_STE!=missingSTE$Taxon,"Fine_STE"]<-missingSTE[missingSTE$Fine_STE_rank=="Subgenus"& missingSTE$Fine_STE!=missingSTE$Taxon,"Taxon"]
missingSTE[missingSTE$Fine_STE_rank=="Species.Group"& missingSTE$Fine_STE!=missingSTE$Taxon,"Fine_STE"]<-missingSTE[missingSTE$Fine_STE_rank=="Species.Group"& missingSTE$Fine_STE!=missingSTE$Taxon,"Taxon"]

##########look to see if the rolled up taxa are in the STE table

missingSTE<-merge(missingSTE, unique(subset(raw, select=c("Taxon", "Rank"))), by="Taxon", all.x=T)

missingSTE[missingSTE$Fine_STE %in% PSSBSTE$Taxon,]
merge(subset(missingSTE, select=c(Taxon, Fine_STE, Fine_STE_rank)), PSSBSTE, by.x="Fine_STE", by.y="Taxon")##where a rolled up taxa is in the STE table, make sure the "rank to use" assigned in PSSB matched the appropriate STE rank (fine rank to fine rank). 
missingSTE[!missingSTE$Fine_STE %in% PSSBSTE$Taxon,c("Taxon", "Taxon.Serial.Number", "Rank","Coarse_STE_rank", "Medium_STE_rank", "Fine_STE_rank")]## these are taxa without an appropriate STE. Need to update the STE table to include. 

missingSTE[missingSTE$Medium_STE %in% PSSBSTE$Taxon,]
merge(subset(missingSTE, select=c(Taxon, Medium_STE, Medium_STE_rank)), PSSBSTE, by.x="Medium_STE", by.y="Taxon") ##where a rolled up taxa is in the STE table, make sure the "rank to use" assigned in PSSB matched the appropriate STE rank (medium rank to medium rank). 
missingSTE[!missingSTE$Medium_STE %in% PSSBSTE$Taxon,]## these are taxa without an appropriate STE

missingSTE[missingSTE$Coarse_STE %in% PSSBSTE$Taxon,]
merge(subset(missingSTE, select=c(Taxon, Coarse_STE, Coarse_STE_rank)), PSSBSTE, by.x="Coarse_STE", by.y="Taxon")##where a rolled up taxa is in the STE table, make sure the "rank to use" assigned in PSSB matched the appropriate STE rank (coarse rank to coarse rank). 
missingSTE[!missingSTE$Coarse_STE %in% PSSBSTE$Taxon,]## these are taxa without an appropriate STE

