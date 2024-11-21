# PSSB-STE-updates

Created by Beth A. Sosik, spring/summer 2024.

Project to update/correct Standard Taxonomic Effort (STE) and taxa attribute assignments in the Puget Sound Stream Benthos (PSSB) database, and to create taxa mappings based on the taxa translator produced by the regional biological condition gradient (BCG) workgroup. This effort was done in tandem with a related project to update the backend of PSSB to allow admins to add/modify STE and attribute tables, and to allow admins to specify taxa mappings. Taxa mapping involves assigning indiscernible taxonomic groups to operational taxonomic units (OTUs). This will facilitate score comparisons and trend analyses of samples collected in different years and identified by different labs. 

In the taxa STE-attribs updates.R script, I applied taxa mapping based on the BCG translation tables, and I looked for taxa that were not in the original PSSB STE, attributes, FSBI, HBI and MTI tables at any STE level.

In taxa STE-attribs updates_Nomapping.R script, I looked for taxa that were not in the original PSSB STE, attributes, FSBI, HBI and MTI tables at any STE level.

Using the outputs from those two scripts, I worked with Sean Sullivan to rectify name changes ("Sean taxa requests.xlsx" and "Copy of Copy of Sean taxa request_KING COUNTY ATTRIBUTES.xlsx"), and created draft STE, mappings and attribute tables. 

In the taxa STE-attribs updates_check.R script, I checked the draft STE, mapping and attribute tables to make sure they populated through the data correctly. The result is a "Master list" of all taxa in PSSB samples, their hierarchy, how they should roll up based on STE, and what taxa in attribute lookup tables they correspond to based on the STE used. The corresponding taxa in FSBI/HBI/MTI tables are also provided where applicable, but I put no effort into chasing down name changes affecting these tables. 

Later, I found that the STE table can be greatly simplified, and so the draft STE tables are not to be used for updating PSSB. 

"PSSB STE and attributes update work list.docx" documents the outline of work for steps and checks done in this drafting process. Lines in gray are tasks that did not get done. I did not put any effort into drafting updated HBI, FSBI and MTI lists. That would be work for a future date.