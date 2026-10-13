########################
#   PREP THE WORKSPACE
########################



##Load libraries 
library(lubridate)
library(tidyverse)
library(httr)
library(keyring)
library(dplyr)
library(readxl)


## set your wd filepath - where do you want this work to live?
setwd("FILE_PATH")



########################
#   IMPORT THE KRAKEN 
########################

## API for data details output --  CSV
#fieldlist <- list("Date", "TriageNotesOrig", "ClinicalImpression", "ChiefComplaintUpdates", "Diagnosis_Combo", "CCDD", "Procedure_Combo", 
                  "HospitalName", "C_Processed_BioSense_ID", "AgeGroup")

#field <- paste(paste0("&field=",fieldlist), collapse="")

url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?endDate=30Nov2020&percentParam=noPercent&datasource=va_hosp&startDate=1Apr2020&medicalGroupingSystem=essencesyndromes&userId=611&site=934&hospFacilityType=emergency%20care&hospFacilityType=urgent%20care&aqtTarget=DataDetails&ccddCategory=cdc%20firearm%20injury%20v2&geographySystem=hospital&detector=nodetectordetector&timeResolution=monthly&refValues=true"

api_response <- GET(url, authenticate(key_list("essence")[1,2], 
                                      key_get("essence", 
                                              key_list("essence")[1,2]))) 
api_response_csv <- content(api_response, by= "csv/text")

firearm_visits <- read_csv(api_response_csv)

# preserve the dataset in the the secure drive 
write.csv(firearm_visits, "FILE_PATH\\Amanda\\FASTER\\firearm_visits.csv") 


########################
#   DATA CLEANING 
########################

# view the data
view(firearm_visits)


### subsetting to only the columns I actually want

names(firearm_visits)

firearm_visits = subset (firearm_visits, select = c(Date, TriageNotesOrig, ClinicalImpression, ChiefComplaintUpdates, Diagnosis_Combo, CCDD, 
                                                    Procedure_Combo, HospitalName, C_Processed_BioSense_ID, AgeGroup))



### drop observations when both triage notes AND clinical impression are empty ###
# this is to maximise the number of observations possible from which to take a random sample
firearm_visits_complete = subset(firearm_visits, (firearm_visits$TriageNotesOrig != "none") | 
                                   (firearm_visits$ClinicalImpression != "none"))


# check it worked
firearm_visits_complete$TriageNotesOrig %>% 
  sort %>% head  

firearm_visits_complete$TriageNotesOrig %>% 
  sort %>% tail

firearm_visits_complete$ClinicalImpression %>% 
  sort %>% head  

firearm_visits_complete$ClinicalImpression %>% 
  sort %>% tail


## keep those dataframes tidy
firearm_visits <- firearm_visits_complete

rm(firearm_visits_complete)




########################
# RANDOM SAMPLE TIME
########################


# random sample of 500 without replacement
firearm_visits_sample <- sample_n(firearm_visits, size = 500, replace = F)

view(firearm_visits_sample)


########################
#     INDEXING
########################


# import an MFT to the thing
MFT <- read_csv("FILE_PATH\\Amanda\\MFT.csv")

# MFT <- MFT %>%
#  select(Facility_Display_Name, Parent_Organization) %>%
#  rename(HospitalName = Facility_Display_Name)


# firearm_visits_join <- left_join(firearm_visits, MFT, by = "HospitalName")


# set the thing

firearm_visits_join <- firearm_visits_sample %>%
  add_column(Parent_Organization = NA)

firearm_visits_join$Parent_Organization <- MFT$Parent_Organization[match(firearm_visits_join$HospitalName, MFT$Facility_Display_Name)]



########################
#   SAVING DATASET
########################


# write the sample like a professional
write.csv(firearm_visits_join, "FILE_PATH\\Amanda\\FASTER\\firearm_visits_sample_MASTER.csv") 


########################
#  IMPORT THE KRAKEN 2
########################


code_extract <- read_excel ("FILE_PATH\\Amanda\\FASTER\\Coding_Extract\\Coding_Summary_By_File_Extract_06.xlsx")


########################
#   DATA CLEANING 2
########################

names(code_extract)

# fix those awful NVivo variable names and drop the extras
code_extract <- code_extract %>%
  rename(Parent_Org = `Hierarchical Name...1`) %>%
  rename(Parent_Node = `Hierarchical Name...4`) %>%
  select(-`File Type`, -Description, -`Code Type`, -Aggregate, -Classification, -`Folder Location`, -`List Order`, -Coverage, 
         -`Number Of Coding References`, -`Reference Number`, -`Coded By Initials`)


# the formatting of the parent node content makes R angry --> thanks to Cody for the help here!
code_extract$Parent_Node <- gsub(
  pattern = ('\\'),
  replacement = '',
  x = code_extract_2$Parent_Node,
  fixed = T
)


#  check how these are structured to do some top class copy/paste
table(code_extract$Parent_Node)  


# tidy up those child nodes
code_extract_2 <- code_extract %>%
  add_column(Child_Node = NA) %>%
  mutate(Child_Node = Parent_Node) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesBroad CategoryAgricultural", "Agricultural", Child_Node)) %>%   # broad categories
  mutate(Child_Node = ifelse(Parent_Node == "NodesBroad CategoryCleaning", "Cleaning", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesBroad CategoryFamily Incident", "Family Incident", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesBroad CategoryRoad Rage", "Road Rage", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesBroad CategoryBystander", "Bystander", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesBroad CategoryDrive-By", "Drive-By", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesBroad CategoryHolstering", "Holstering", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesBroad CategoryHunting", "Hunting", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesBroad CategoryTangential Crime", "Tangential Crime", Child_Node))%>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesFalse Positive", "False Positive", Child_Node)) %>%    # false positives
  mutate(Child_Node = ifelse(Parent_Node == "NodesFalse PositiveBB Gun-Related", "BB Gun-Related", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesFalse PositiveNot Initial Encounter", "Not Initial Encounter", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesFalse PositiveRubber bullet", "Rubber Bullet", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesFalse PositiveOther", "Other False Positive", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesFASTER CategoriesAccidental", "Unintentional", Child_Node)) %>%  # FASTER categories
  mutate(Child_Node = ifelse(Parent_Node == "NodesFASTER CategoriesAssault", "Assault", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesFASTER CategoriesAssaultAccidental Discharge Dx Code", "Assault-W34 Code", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesFASTER CategoriesIntentional", "Intentional", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesFASTER CategoriesIntentionalAccidental Discharge Dx Code", "Intentional-W34 Code", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesFASTER CategoriesUnclear Self-Directed", "Unclear Self-Directed", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesFASTER CategoriesUnclear Based on Text", "Unclear Based on Text", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesInjury", "Injury", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesLaw Enforcement", "Law Enforcement", Child_Node)) %>%     # law enforcement parent node
  mutate(Child_Node = ifelse(Parent_Node == "NodesLaw EnforcementOfficer-Involved", "Law Enforcement-Officer-Involved", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesLaw EnforcementOfficer-InvolvedAccidental Discharge Code", "Law Enforcement-Officer-Involved-W34 Code", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMisc", "Misc", Child_Node)) %>%       # misc parent node
  mutate(Child_Node = ifelse(Parent_Node == "NodesMiscAirlift", "Airlift", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMiscTransfer", "Transfer", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMiscHistory of Violence", "History of Violence", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPerson Causing Harm", "Person Causing Harm", Child_Node)) %>%     # person causing harm parent node
  mutate(Child_Node = ifelse(Parent_Node == "NodesPerson Causing HarmIntimate Partner", "Person Causing Harm-Intimate Partner", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPerson Causing HarmUnknown Person", "Person Causing Harm-Unknown Person", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPerson Causing HarmOther Known Person", "Person Causing Harm-Other Known Person", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPerson Causing HarmOther Family Member", "Person Causing Harm-Other Family Member", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPerson Causing HarmSelf-Inflicted", "Self-Inflicted", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSocial Determinants", "Social Determinants", Child_Node)) %>%     # social determinants parent node
  mutate(Child_Node = ifelse(Parent_Node == "NodesSocial DeterminantsDomestic Violence", "Social Determinants-Domestic Violence", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSocial DeterminantsHomelessness", "Social Determinants-Homelessness", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSubstance Use", "Substance Use", Child_Node)) %>%     # substance use parent node
  mutate(Child_Node = ifelse(Parent_Node == "NodesSubstance UseAlcohol", "Alcohol", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSubstance UseMeth", "Meth", Child_Node)) %>%    
  mutate(Child_Node = ifelse(Parent_Node == "NodesSubstance UseOpioid", "Opioid", Child_Node)) %>%    
  mutate(Child_Node = ifelse(Parent_Node == "NodesSubstance UseOther Drug", "Other Drug", Child_Node)) %>%    
  mutate(Child_Node = ifelse(Parent_Node == "NodesSubstance UseScreening", "Substance Use Screening", Child_Node)) %>% 
  mutate(Child_Node = ifelse(Parent_Node == "NodesStorage", "Storage", Child_Node)) %>%     # storage parent node
  mutate(Child_Node = ifelse(Parent_Node == "NodesTrauma Narrator", "Trauma Narrator", Child_Node)) %>%     # trauma narrator parent node
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-Place", "Time-Place", Child_Node)) %>%       # time and place parent node
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlacePlace", "Place", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceCity Name", "City Name", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceGas Station", "Gas Station", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceOther", "Place-Other", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlacePlacePark", "Park", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceParty", "Party", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlacePlacePrivate Residence", "Private Residence", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceSchool", "School", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceStreet or Motorway", "Street or Motorway", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceVehicle", "Vehicle", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlaceTimeRelative Time", "Relative Time", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime-PlaceTime", "Time", Child_Node)) %>%      
  mutate(Child_Node = ifelse(Parent_Node == "NodesType of Weapon", "Type of Weapon", Child_Node)) %>%     # type of weapon
  mutate(Child_Node = ifelse(Parent_Node == "NodesType of Weapon22", "Type of Weapon-22", Child_Node)) %>% 
  mutate(Child_Node = ifelse(Parent_Node == "NodesType of Weapon38", "Type of Weapon-38", Child_Node)) %>% 
  mutate(Child_Node = ifelse(Parent_Node == "NodesType of Weapon45", "Type of Weapon-45", Child_Node)) %>% 
  mutate(Child_Node = ifelse(Parent_Node == "NodesType of WeaponHandgun", "Type of Weapon-Handgun", Child_Node)) %>% 
  mutate(Child_Node = ifelse(Parent_Node == "NodesType of WeaponHandgun9mm", "Type of Weapon-Handgun-9mm", Child_Node)) %>% 
  mutate(Child_Node = ifelse(Parent_Node == "NodesType of WeaponShotgun or Rifle", "Type of Weapon-Shotgun or Rifle", Child_Node)) %>% 
  mutate(Child_Node = ifelse(Parent_Node == "NodesType of WeaponSubmachine Gun", "Type of Weapon-Submachine Gun", Child_Node)) %>% 
  mutate(Child_Node = ifelse(Parent_Node == "NodesType of WeaponUnknown", "Type of Weapon-Unknown", Child_Node)) %>% 
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)ChiefComplaintUpdates", "Nonsense", Child_Node)) %>%     # nonsense
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)clinical_impression_temp", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)ClinicalImpression", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)Diagnosis_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)Procedure_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)triage_notes_temp", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)TriageNotesOrig", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitDiagnosis_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitHospitalName", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitProcedure_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splittriage_notes_temp", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)CCDD", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitCCDD", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitChiefComplaintUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitclinical_impression_temp", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitClinicalImpression", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitTriageNotesOrig", "Nonsense", Child_Node))
  


# does it look fine?
table(code_extract_2$Child_Node) 


# now we can go back and tidy the parent nodes
table(code_extract_2$Parent_Node)

code_extract_2 <- code_extract_2 %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesBroad CategoryAgricultural", "Broad Category", Parent_Node)) %>%   # broad category
  mutate(Parent_Node = ifelse(Parent_Node == "NodesBroad CategoryBystander", "Broad Category", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesBroad CategoryCleaning", "Broad Category", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesBroad CategoryDrive-By", "Broad Category", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesBroad CategoryFamily Incident", "Broad Category", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesBroad CategoryHolstering", "Broad Category", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesBroad CategoryHunting", "Broad Category", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesBroad CategoryRoad Rage", "Broad Category", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesBroad CategoryTangential Crime", "Broad Category", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFalse Positive", "False Positive", Parent_Node)) %>%     # false positive
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFalse PositiveBB Gun-Related", "False Positive", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFalse PositiveNot Initial Encounter", "False Positive", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFalse PositiveOther", "False Positive", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFalse PositiveRubber bullet", "False Positive", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFASTER CategoriesAccidental", "FASTER Categories", Parent_Node)) %>%     # FASTER categories
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFASTER CategoriesAssault", "FASTER Categories", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFASTER CategoriesAssaultAccidental Discharge Dx Code", "FASTER Categories", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFASTER CategoriesIntentional", "FASTER Categories", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFASTER CategoriesIntentionalAccidental Discharge Dx Code", "FASTER Categories", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFASTER CategoriesUnclear Based on Text", "FASTER Categories", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFASTER CategoriesUnclear Self-Directed", "FASTER Categories", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)ChiefComplaintUpdates", "Nonsense", Parent_Node)) %>%     # nonsense
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)clinical_impression_temp", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)ClinicalImpression", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)Diagnosis_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)Procedure_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)triage_notes_temp", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)TriageNotesOrig", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitDiagnosis_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitHospitalName", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitProcedure_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splittriage_notes_temp", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_split (2)CCDD", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitCCDD", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitChiefComplaintUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitclinical_impression_temp", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitClinicalImpression", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "Nodesfirearm_visits_sample_splitTriageNotesOrig", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesHistory of Violence", "History of Violence", Parent_Node)) %>%      # history of violence
  mutate(Parent_Node = ifelse(Parent_Node == "NodesInjury", "Injury", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesLaw Enforcement", "Law Enforcement", Parent_Node)) %>%     # law enforcement
  mutate(Parent_Node = ifelse(Parent_Node == "NodesLaw EnforcementOfficer-Involved", "Law Enforcement", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesLaw EnforcementOfficer-InvolvedAccidental Discharge Code", "Law Enforcement", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSocial Determinants", "Social Determinants", Parent_Node)) %>%     # social determinants
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSocial DeterminantsHomelessness", "Social Determinants", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSocial DeterminantsDomestic Violence", "Social Determinants", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMisc", "Misc", Parent_Node)) %>%     # misc
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMiscTransfer", "Misc", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMiscAirlift", "Misc", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMiscHistory of Violence", "Misc", Parent_Node)) %>%  
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMiscNarrative in Triage Notes Incongruous with CCCC Accidental", "Misc", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMental Health", "Mental Health", Parent_Node)) %>%   # mental health
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPerson Causing Harm", "Person Causing Harm", Parent_Node)) %>%     # person causing harm parent node
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPerson Causing HarmIntimate Partner", "Person Causing Harm", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPerson Causing HarmUnknown Person", "Person Causing Harm", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPerson Causing HarmOther Known Person", "Person Causing Harm", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPerson Causing HarmOther Family Member", "Person Causing Harm", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPerson Causing HarmSelf-Inflicted", "Person Causing Harm", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesStorage", "Storage", Parent_Node)) %>%      # storage
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSubstance Use", "Substance Use", Parent_Node)) %>%    # substance use
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSubstance UseAlcohol", "Substance Use", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSubstance UseMeth", "Substance Use", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSubstance UseOpioid", "Substance Use", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSubstance UseOther Drug", "Substance Use", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSubstance UseScreening", "Substance Use", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-Place", "Time and Place", Parent_Node)) %>%      # time and place
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlacePlace", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlaceTime", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceCity Name", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceGas Station", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceOther", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlacePlacePark", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceParty", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlacePlacePrivate Residence", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceSchool", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceStreet or Motorway", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlacePlaceVehicle", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime-PlaceTimeRelative Time", "Time and Place", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTrauma Narrator", "Trauma Narrator", Parent_Node)) %>%      # trauma narrator
  mutate(Parent_Node = ifelse(Parent_Node == "NodesType of Weapon", "Type of Weapon", Parent_Node)) %>%     # type of weapon
  mutate(Parent_Node = ifelse(Parent_Node == "NodesType of Weapon22", "Type of Weapon", Parent_Node)) %>% 
  mutate(Parent_Node = ifelse(Parent_Node == "NodesType of Weapon38", "Type of Weapon", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesType of Weapon45", "Type of Weapon", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesType of WeaponHandgun", "Type of Weapon", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesType of WeaponHandgun9mm", "Type of Weapon", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesType of WeaponShotgun or Rifle", "Type of Weapon", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesType of WeaponSubmachine Gun", "Type of Weapon", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesType of WeaponUnknown", "Type of Weapon", Parent_Node))


# check that it looks fine
table(code_extract_2$Parent_Node)


# let's also just tidy the Parent_Org names while we're in here
table(code_extract_2$Parent_Org)

# it's full of backstrokes
code_extract_2$Parent_Org <- gsub(
  pattern = ('\\'),
  replacement = '',
  x = code_extract_2$Parent_Org,
  fixed = T
)

# i realised after writing this that I could have just dropped the leading characters
# oops
code_extract_2 <- code_extract_2 %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesCHI-FHS", "CHI-FHS", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesConfluence", "Confluence", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesEverett Clinic", "Everett Clinic", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesEvergreen", "Evergreen", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesHMC", "HMC", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesINHS", "INHS", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesJefferson", "Jefferson", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesKadlec", "Kadlec", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesKPWA", "KPWA", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesLegacy", "Legacy", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesLourdes", "Lourdes", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesMason", "Mason", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesMorton", "Morton", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesMultiCare", "MultiCare", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesOcean Beach", "Ocean Beach", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesPeaceHealth", "PeaceHealth", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesProsser", "Prosser", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesProvidence", "Providence", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesSkagit", "Skagit", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesSwedish", "Swedish", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesUnaffiliated", "Unaffiliated", Parent_Org)) %>%
  mutate(Parent_Org = ifelse(Parent_Org == "FilesUW Medicine", "UW Medicine", Parent_Org))
  
 
# check it out, check it out now
table(code_extract_2$Parent_Org)
  


########################
#    SAVE It OFF
########################

write.csv(code_extract_2, "FILE_PATH\\Amanda\\FASTER\\code_extract.csv") 




###########################################################
#             REPEAT FOR ASSAULT VISITS
###########################################################


########################
#   IMPORT THE KRAKEN 
########################

## API for data details output --  CSV
fieldlist <- list("Date", "TriageNotesOrig", "ClinicalImpression", "ChiefComplaintUpdates", "Diagnosis_Combo", "CCDD", "Procedure_Combo", 
                  "HospitalName", "C_Processed_BioSense_ID", "AgeGroup")

field <- paste(paste0("&field=",fieldlist), collapse="")

url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?endDate=31Dec20&percentParam=noPercent&datasource=va_hosp&startDate=01Apr20&medicalGroupingSystem=essencesyndromes&userId=611&site=934&hospFacilityType=emergency%20care&aqtTarget=TimeSeries&ccddCategory=cdc%20assault%20firearm%20injury%20v1&geographySystem=hospital&detector=nodetectordetector&timeResolution=monthly&hasBeenE=1&refValues=true"

api_response <- GET(url, authenticate(key_list("essence")[1,2], 
                                      key_get("essence", 
                                              key_list("essence")[1,2]))) 
api_response_csv <- content(api_response, by= "csv/text")

firearm_assault <- read_csv(api_response_csv)

# preserve the dataset in the y drive 
write.csv(firearm_visits_assault, "FILE_PATH\\Amanda\\FASTER\\firearm_visits_short.csv")




########################
#   DATA CLEANING 
########################

# view the data
view(firearm_assault)


### subsetting to only the columns I actually want

names(firearm_assault)

firearm_assault = subset (firearm_assault, select = c(Date, TriageNotesOrig, ClinicalImpression, ChiefComplaintUpdates, Diagnosis_Combo, CCDD, 
                                                    Procedure_Combo, HospitalName, C_Processed_BioSense_ID, AgeGroup, CCDDCategory_flat))



### drop observations when both triage notes AND clinical impression are empty ###
# this is to maximise the number of observations possible from which to take a random sample
firearm_visits_complete = subset(firearm_visits, (firearm_visits$TriageNotesOrig != "none") | 
                                   (firearm_visits$ClinicalImpression != "none"))


# check it worked
firearm_visits_complete$TriageNotesOrig %>% 
  sort %>% head  

firearm_visits_complete$TriageNotesOrig %>% 
  sort %>% tail

firearm_visits_complete$ClinicalImpression %>% 
  sort %>% head  

firearm_visits_complete$ClinicalImpression %>% 
  sort %>% tail


## keep those dataframes tidy
firearm_assault <- firearm_visits_complete

rm(firearm_visits_complete)

  
