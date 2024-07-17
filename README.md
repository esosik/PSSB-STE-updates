# PSSB-STE-updates

Created by Beth A. Sosik, spring/summer 2024.

Project to update/correct Standard Taxonomic Effort (STE) and taxa attribute assignments in the Puget Sound Stream Benthos (PSSB) database, and to create taxa mappings based on the taxa translator produced by the regional biological condition gradient (BCG) workgroup. This effort was done in tandem with a related project to update the backend of PSSB to allow admins to add/modify STE and attribute tables, and to allow admins to specify taxa mappings. Taxa mapping involves assigning indiscernible taxonomic groups to operational taxonomic units (OTUs). This will facilitate score comparisons and trend analyses of samples collected in different years and identified by different labs. 


In July 2024, BAS created scripts to be used as tools for future PSSB admins to help keep STE and taxa mapping assignments up to date in the PSSB database. 

The mapping_update_screening_tool will use the latest BCG translation table and the most current PSSB taxa data download to see which taxa need mappings updated in PSSB based on the BCG translator, and what taxa names need to be sent to the BCG working group for addition into the next iteration of the translator table. 

The STE_update_screening_tool uses the latest combined STE lookup table downloaded from PSSB, the latest BCG translation table and the latest BCG attribute table to see which taxa need to be added to the STE lookup tables in PSSB. 

I did not create a screening tool to check the PSSB attribute table for updates. This is because any substantial updates to the attributes should occur only as part of a comprehensive B-IBI recalibration effort. Instead, admins should use the mapping_update_screening_tool to note if new or changed OTU names should be added to the attributes table, with attributes assigned from old taxa names. Work with Sean Sullivan from Rhithron to confirm attributes can be copied from old taxon name to new OTU name without issue. 
