library(openxlsx)

setwd("C:/Users/esosik/King County/DNRP - Science TCA Unit - Bug Monitoring/PSSB issues/STE_attrib_updates")

# PSSBmapping<-read.xlsx("STE_Mapping_Atts_Draft.xlsx", detectDates = T, sheet="Draft Mapping")#load the mapping used by PSSB
PSSBmapping<-read.xlsx("PSSB_mapping.xlsx")#load the mapping used by PSSB

BCGmapping<-read.csv("ORWA_TaxaTranslator_20240619.csv")# load the latest BCG mapping

mapping<-merge(PSSBmapping, BCGmapping, by.x=c("Alternate.Name"), by.y=c("Taxon_orig"), all=T) #merge the two mappings together, keep all items in both dataframes

subset(mapping, is.na(OTU_MetricCalc))
test<-subset(mapping, is.na(Preferred.Name)&Alternate.Name!=OTU_MetricCalc&OTU_MetricCalc!="DNI")

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

mapping<-merge(mapping, PSSB_taxa, by.x=c("Alternate.Name"), by.y=c("Taxon"), all.y=T) #merge the PSSB taxa list iwth the mapping dataframe, and limit the output to only taxa in the PSSB taxa list.

missingBCGmapping<-subset(mapping, is.na(OTU_MetricCalc), select="Alternate.Name")##these are taxa in PSSB samples that have not been translated by the BCG working group. 
write.csv(missingBCGmapping, "Missing_from_BCG_taxa_translator.csv")

missingPSSBmapping<-subset(mapping, is.na(Preferred.Name)&((Alternate.Name!=OTU_MetricCalc&OTU_MetricCalc!="DNI")|is.na(OTU_MetricCalc)))##these are taxa in PSSB samples that don't have a mapping assigned in PSSB. 



write.csv(missingPSSBmapping, "Missing_from_PSSB_mapping_table.csv")

###because there may be overlap between the last two, this next section will parse out the differences.

missingPSSBmappingBCGexists<-missingPSSBmapping$Alternate.Name[!missingPSSBmapping$Alternate.Name %in% missingBCGmapping$Alternate.Name] ##these are taxa in PSSB samples that have a BCG translation, but do not have a mapping assigned in PSSB. Update the PSSB mapping to include these. 

missingBCG_PSSBmappingexists<-missingBCGmapping$Alternate.Name[missingBCGmapping$Alternate.Name %in% PSSBmapping$Alternate.Name]##these are taxa in PSSB samples that do not have a BCG translation, but do have a mapping assigned in PSSB. Probably a result of BAS editing some taxa names in PSSB for clarity, formatting and accuracy (i.e. Rhabdomastix (Rhabdomastix (Rhabdomastix)) was edited in PSSB taxonomy to simply Rhabdomastix (Rhabdomastix)). Let Sean know about these name changes in PSSB so he can update the BCG translation table with the edited names.

missingBCGPSSB<-missingBCGmapping$Alternate.Name[missingBCGmapping$Alternate.Name %in% missingPSSBmapping$Alternate.Name]##these are taxa in PSSB samples that do not have a BCG translation, and do not have a mapping assigned in PSSB. Send these to Sean Sullivan once a year for updating the BCG translation tables.

missingBCGmapping$Alternate.Name[missingBCGmapping$Alternate.Name %in% missingPSSBmapping$Alternate.Name] ###this output should be identical to the object missingBCGPSSB

###this next section checks to see if existing mappings have changed since the last version of the BCG table.

changedmapping<-mapping[which(mapping$Preferred.Name !=mapping$OTU_MetricCalc),c("Alternate.Name", "Preferred.Name", "OTU_MetricCalc")] ##in many cases, the BCG translates taxa to "DNI" meaning "Do Not Include". But since these taxa are not explicitly excluded in the PSSB calculations, we created mappings for these in PSSB based simply on copying the taxa name into the mapping column. BAS also did not wish to enter "Polycentropus sensu lato" as a new taxa in PSSB since "Polycentropus" already existed (and truly implementing "sensu lato" qualifications in PSSB isn't feasible). So BAS kept the mapping to "Polycentropus". Any other differences should be examined, and the PSSB mappings should be updated to reflect the current BCG translations. Check taxa attributes as well to see if new taxa names should have attributes assigned from old taxa names. 
