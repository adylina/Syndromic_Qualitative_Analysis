# IMPORTANT ###############################################################################################################
#BEFORE RUNNING, SAVE YOUR ESSENCE CREDENTIALS TO THE WINDOW CREDENTIAL MANAGER - this prevents the need from typing them into the code below
# 1. Search for Credential Manager on your computer
# 2. Click "Windows Credentials"
# 3. Click "Add a generic credential"
# 4. For Internet or network address, type: essence
# 5. For user name, type: yourESSENCEusername
# 6. For Password, type: yourESSENCEpw
# 7. If you update your ESSENCE password, you will need to come into the credential manager and update it here as well
#######################################################################################################################

########################
#   PREP THE WORKSPACE
########################

##Load libraries 
library(lubridate)
library(tidyverse)
library(knitr)
library(httr)
library(jsonlite)
library(keyring)
library(ggplot2)
library(readxl)
library(dplyr)

##Set your wd filepath - where do you want the created doc to go?
setwd("FILE_PATH\\Amanda\\Qualitative Project\\MVC")


########################
#   IMPORT THE KRAKEN 
########################


#import dataset 
read_excel("FILE_PATH\\Amanda\\Qualitative Project\\MVC\\MVC_ED_Visits.xlsx") 

### remove visits at VM Yakima Valley due to duplication issue during activation
# I realised I can just do this in ESSENCE before downloading the data.


########################
#   DATA CLEANING 
########################


### dropping some columns ###

## useless and duplicative variables because things are WILD over here
MVC_Less = subset(MVC_ED_Visits, select = -c(FirstDateTimeAdded,DispositionDateTime,Insurance_Company_ID,Insurance_Coverage,
                                             Hospital_HRSA_County_Designation,HospitalDHHSRegion,OnsetDateTimeText,Diagnosis_Date_Time,
                                             Unique_Physician_Identifier,ChiefComplaintMDTUpdates,Medication_Combo,CCOrig_Length,
                                             DischargeDisposition,DiagnosisDatetime,DiagnosisText,DDProcessed,Procedure_Date_Time,
                                             Medical_Record_Number,Diagnosis_Type,Provider_Type_Code,) )

## remove more because I was worried about adding so many initialy 
names(MVC_Less)

MVC_Less2 = subset(MVC_Less, select = -c(InitEDAcuityAssessment,CCAvailable,AdmissionTypeCategory,Procedure_Code,Initial_Acuity_Combo,Medication_List,
                                         CCInformative,CCParsed_Length,ProviderDiagnosis,DDConverted,Medication_Code,Orig_Sex,DD_Length,
                                         Previous_Hospital_Unit,DischargeDispositionUpdates,Hospital_Unit_Code,DDInformative,Hospital_Service,
                                         Initial_Acuity_Code,PatientClassList,Recorded_Date_Time,Discharge_Instructions,Hospital_Unit_Description,
                                         C_Patient_Class_Updates,Pregnancy_Status_Code,Provider_Type_Combo,ProcedureCode,Problem_List_Code) )


## drop some direct identifiers
MVC_Less_Identified = subset(MVC_Less2, select = -c(Birth_Date_Time,Str_Birth_Date_Time) )

names(MVC_Less_Identified)

## drop those extra dataframes -- keep it tidy! 
remove(MVC_Less)
remove(MVC_Less2)


### drop observations when both triage notes AND clinical impression are empty ###
# this is to maximise the number of observations possible from which to take a random sample
MVC_TNandCI = subset(MVC_Less_Identified, !is.na(MVC_Less_Identified$TriageNotesOrig) | !is.na(MVC_Less_Identified$ClinicalImpression))

## drop extra dataframes
remove(MVC_Less_Identified)


########################
# RANDOM SAMPLE TIME
########################

### random sample time!! ###
MVC_Sample <- sample.rows(MVC_TNandCI, 450, replace = FALSE, prob = NULL)


### check if Patient ZIP is a hot mess
Patient_Zip
# spoiler: it is. 

### clean up patient ZIP so it's only the first 5 digits and can be worked with
MVC_Sample$Patient_Zip
MVC_Sample$Patient_Zip2 <- substr(MVC_Sample$Patient_Zip,1,5)
# confirm that it looks fine now
MVC_Sample$Patient_Zip2


### Confiem that hospital ZIP is tidy
MVC_Sample$HospitalZip
#it should look fine


### create some factors from character variables
MVC_Sample$`Age Group` <-as.factor(MVC_Sample$`Age Group`)

MVC_Sample$C_Death <-as.factor(MVC_Sample$C_Death)

MVC_Sample$HasBeenI <-as.factor(MVC_Sample$HasBeenI)

MVC_Sample$HospitalName <-as.factor(MVC_Sample$`Facility Name`)

MVC_Sample$HospitalRegion <-as.factor(MVC_Sample$`Facility Region`)

MVC_Sample$Region <-as.factor(MVC_Sample$Region)

MVC_Sample$Sex <-as.factor(MVC_Sample$Sex)


### describe things###

###Tabulate some fields to describe the visits in meaningful ways
AgeGroup_FacRegion <- table(MVC_Sample$`Age Group`,MVC_Sample$`Facility Region`)
AgeGroup_FacRegion

HasBeenI_FacRegion <- table(MVC_Sample$`Facility Region`,MVC_Sample$HasBeenI)
HasBeenI_FacRegion

Age_Sex <- table(MVC_Sample$`Age Group`,MVC_Sample$Sex)
Age_Sex

HasBeenI_FacName <- table(MVC_Sample$`Facility Name`,MVC_Sample$HasBeenI)
HasBeenI_FacName



########################
#   SAVING DATASET
########################



### write the data back to excel to load into NVivo ###
write.xlsx(MVC_Sample, "FILE_PATH\\Amanda\\Qualitative Project\\MVC\\MVC_ED_Visits_NVivo.xlsx")             



########################
#  IMPORT THE KRAKEN 2
########################

### bring the raw coded data extract back into R from NVivo ###
code_extract <- read_excel ("FILE_PATH\\Amanda\\Qualitative_Project\\MVC\\NVivo_Extracts\\Coding Summary By File Extract_29.xlsx")


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


# let's just tidy the Parent_Org names while we're in here
table(code_extract$Parent_Org)

# it's full of backstrokes
code_extract$Parent_Org <- gsub(
  pattern = ('\\'),
  replacement = '',
  x = code_extract$Parent_Org,
  fixed = T
)

# i realised after writing this that I could have just dropped the leading characters
# oops
code_extract <- code_extract %>%
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
table(code_extract$Parent_Org)


# the formatting of the parent node content makes R angry --> thanks to Cody for the help here!
code_extract$Parent_Node <- gsub(
  pattern = ('\\'),
  replacement = '',
  x = code_extract$Parent_Node,
  fixed = T
)


#  check how these are structured to do some top class copy/paste
table(code_extract$Parent_Node)


# drop the "nodes" business at the front end because it's obnoxious

#code_extract2 <- code_extract

#code_extract2$Parent_Node <- substr(-code_extract2$Parent_Node,1,5)


# tidy up those child nodes
code_extract <- code_extract %>%
  add_column(Child_Node = NA) %>%
  mutate(Child_Node = Parent_Node) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesAirbag", "Airbag", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesArrival", "Arrival", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesArrivalEMS", "EMS", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesArrivalLaw Enforcement", "Law Enforcement", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesArrivalOther", "Other", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesArrivalSelf", "Self", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesArrivalTransfer", "Transfer", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesBicycle", "Bicycle", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesBicycleHelmet", "Helmet", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision Type", "Collision Type", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeDitch", "Ditch", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeGeneral context", "General Context", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeImpact - head", "Impact - Head", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeImpact - rear", "Impact - Rear", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeImpact - side", "Impact - Side", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeImpact - sideDriver vs Passenger", "Side - Driver or Passenger", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeReversing", "Reversing", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeRoll", "Roll", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeStrike - animal", "Strike - Animals", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeStrike - bike", "Strike - Bicycle", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeStrike - motorcycle", "Strike - Motorcycle", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeStrike - object", "Strike - Object", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeStrike - pedestrian", "Strike - Pedestrian", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesCollision TypeVehicle count", "Strike - Vehicle Count", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)CCDD", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)Chief_Complaint_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)ChiefComplaintOrig", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)ChiefComplaintUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)ClinicalImpression", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)Diagnosis_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)Discharge Diagnosis", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)DischargeDiagnosisUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)Procedure_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)TriageNotesOrig", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitCCDD", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitCCDDCategory_flat", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitChief_Complaint_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitChiefComplaintOrig", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitChiefComplaintUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitClinicalImpression", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitDiagnosis_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitDischarge Diagnosis", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitDischargeDiagnosisUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitPatientClassUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitProblem_List_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitProcedure_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitTriageNotesOrig", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_LourdesChief_Complaint_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_LourdesChiefComplaintUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_LourdesClinicalImpression", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_LourdesDiagnosis_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_LourdesDischargeDiagnosisUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_LourdesProblem_List_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_LourdesProcedure_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_LourdesTriageNotesOrig", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MortonCCDD", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MortonChief_Complaint_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MortonChiefComplaintUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MortonClinicalImpression", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MortonDiagnosis_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MortonDischarge Diagnosis", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MortonDischargeDiagnosisUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MortonProcedure_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MultiCareCCDD", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MortonTriageNotesOrig", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MultiCareCCDDCategory_flat", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MultiCareChief_Complaint_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MultiCareChiefComplaintOrig", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MultiCareChiefComplaintUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MultiCareClinicalImpression", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MultiCareDiagnosis_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MultiCareDischarge Diagnosis", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MultiCareDischargeDiagnosisUpdates", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MultiCareProcedure_Combo", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_MultiCareTriageNotesOrig", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVisits_LourdesCCDD", "Nonsense", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesExtraction", "Extraction", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesExtractionEjected", "Ejected", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesExtractionEMS", "EMS", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesExtractionOther", "Other", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesExtractionSelf", "Self", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesFalse Positive", "False Positive", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesGeographic Location", "Geographic Location", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesGeographic LocationHighway-Freeway", "Highway or Freeway", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesGeographic LocationIntersection", "Intersection", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesGeographic LocationOther", "Other", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesGeographic LocationParking Lot", "Parking Lot", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesGeographic LocationSpecific", "Specific Location", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesGeographic LocationSurface Street", "Surface Street", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesInjuries", "Injuries", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesIntrusion", "Intrusion", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesIntrusionGreater Than or Equal to 12in", "Greater Than or Equal to 12in", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesIntrusionLess Than 12 in", "Less Than 12in", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesIntrusionNone", "None", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesIntrusionSignificant", "Significant", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesIntrusionUnclear", "Unclear", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesOther Information", "Other Information", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPedestrian", "Pedestrian", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPosition", "Position", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPositionDriver", "Driver", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPositionPassenger", "Passenger", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPositionPassengerPassenger - Alt Vehicle", "Passenger - Alt Vehicle", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPositionPassengerPassenger - Front", "Passenger - Front", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPositionPassengerPassenger - Rear", "Passenger - Rear", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesPrescription", "Prescription", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesRestraint", "Restraint", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesRestraint-", "(-)", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesRestraint+", "(+)", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesRestraintChildseat", "Childseat", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesRestraintUnknown", "Unknown", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSpeed", "Speed", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSpeed35 or Less", "35mph or Less", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSpeed36 to 60", "36mph to 60mph", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSpeed61 or Greater", "61mph or Greater", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSpeedHigh", "High", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSpeedLow", "Low", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSpeedOther", "Other", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSpeedStopped", "Stopped", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSpeedUnknown", "Unknown", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesSubstance Use", "Substance Use", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime of Event", "Time of Event", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime of Event0-1 Day", "0-1 Day", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime of Event1-7 Days", "1-7 Days", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime of EventMore than 7 Days", "More Than 7 Days", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime of EventSpecific", "Specific Date", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime of EventZ - Day", "Day", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesTime of EventZ - Night", "Night", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVehicle Type", "Vehicle Type", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVehicle TypeAlt Vech - ATV", "ATV", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVehicle TypeAlt Vech - Dirt Bike", "Dirt Bike", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVehicle TypeAlt Vech - Motorcycle", "Motorcycle", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVehicle TypeAlt Vech - Other", "Other", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVehicle TypeAlternative Vehicle", "Alternative Vehicle", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVehicle TypeBroad Type", "Broad Type", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVehicle TypeBus", "Bus", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVehicle TypeHelmet (Alt Vech)", "Alternate Vehicle - Helmet", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVehicle TypeLorry", "Lorry", Child_Node)) %>%
  mutate(Child_Node = ifelse(Parent_Node == "NodesVehicle TypeMake-Model", "Make-Model", Child_Node))
  
table(code_extract$Child_Node)


# clean the parent_node column
code_extract <- code_extract %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesAirbag", "Airbag", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesArrival", "Arrival", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesArrivalEMS", "Arrival", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesArrivalLaw Enforcement", "Arrival", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesArrivalOther", "Arrival", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesArrivalSelf", "Arrival", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesArrivalTransfer", "Arrival", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesBicycle", "Bicycle", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesBicycleHelmet", "Bicycle", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision Type", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeDitch", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeGeneral context", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeImpact - head", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeImpact - rear", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeImpact - side", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeImpact - sideDriver vs Passenger", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeReversing", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeRoll", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeStrike - animal", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeStrike - bike", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeStrike - motorcycle", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeStrike - object", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeStrike - pedestrian", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesCollision TypeVehicle count", "Collision Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)CCDD", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)Chief_Complaint_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)ChiefComplaintOrig", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)ChiefComplaintUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)ClinicalImpression", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)Diagnosis_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)Discharge Diagnosis", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)DischargeDiagnosisUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)Procedure_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_Split (2)TriageNotesOrig", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitCCDD", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitCCDDCategory_flat", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitChief_Complaint_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitChiefComplaintOrig", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitChiefComplaintUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitClinicalImpression", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitDiagnosis_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitDischarge Diagnosis", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitDischargeDiagnosisUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitPatientClassUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitProblem_List_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitProcedure_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesMVC_ED_Visits_NVivo_SplitTriageNotesOrig", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_LourdesChief_Complaint_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_LourdesChiefComplaintUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_LourdesClinicalImpression", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_LourdesDiagnosis_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_LourdesDischargeDiagnosisUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_LourdesProblem_List_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_LourdesProcedure_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_LourdesTriageNotesOrig", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MortonCCDD", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MortonChief_Complaint_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MortonChiefComplaintUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MortonClinicalImpression", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MortonDiagnosis_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MortonDischarge Diagnosis", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MortonDischargeDiagnosisUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MortonProcedure_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MultiCareCCDD", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MortonTriageNotesOrig", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MultiCareCCDDCategory_flat", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MultiCareChief_Complaint_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MultiCareChiefComplaintOrig", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MultiCareChiefComplaintUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MultiCareClinicalImpression", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MultiCareDiagnosis_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MultiCareDischarge Diagnosis", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MultiCareDischargeDiagnosisUpdates", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MultiCareProcedure_Combo", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_MultiCareTriageNotesOrig", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVisits_LourdesCCDD", "Nonsense", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesExtraction", "Extraction", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesExtractionEjected", "Extraction", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesExtractionEMS", "Extraction", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesExtractionOther", "Extraction", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesExtractionSelf", "Extraction", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesFalse Positive", "False Positive", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesGeographic Location", "Geographic Location", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesGeographic LocationHighway-Freeway", "Geographic Location", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesGeographic LocationIntersection", "Geographic Location", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesGeographic LocationOther", "Geographic Location", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesGeographic LocationParking Lot", "Geographic Location", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesGeographic LocationSpecific", "Geographic Location", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesGeographic LocationSurface Street", "Geographic Location", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesInjuries", "Injuries", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesIntrusion", "Intrusion", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesIntrusionGreater Than or Equal to 12in", "Intrusion", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesIntrusionLess Than 12 in", "Intrusion", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesIntrusionNone", "Intrusion", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesIntrusionSignificant", "Intrusion", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesIntrusionUnclear", "Intrusion", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesOther Information", "Other Information", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPedestrian", "Pedestrian", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPosition", "Position", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPositionDriver", "Position", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPositionPassenger", "Position", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPositionPassengerPassenger - Alt Vehicle", "Position", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPositionPassengerPassenger - Front", "Position", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPositionPassengerPassenger - Rear", "Position", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesPrescription", "Prescription", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesRestraint", "Restraint", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesRestraint-", "Restraint", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesRestraint+", "Restraint", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesRestraintChildseat", "Restraint", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesRestraintUnknown", "Restraint", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSpeed", "Speed", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSpeed35 or Less", "Speed", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSpeed36 to 60", "Speed", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSpeed61 or Greater", "Speed", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSpeedHigh", "Speed", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSpeedLow", "Speed", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSpeedOther", "Speed", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSpeedStopped", "Speed", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSpeedUnknown", "Speed", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesSubstance Use", "Substance Use", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime of Event", "Time of Event", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime of Event0-1 Day", "Time of Event", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime of Event1-7 Days", "Time of Event", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime of EventMore than 7 Days", "Time of Event", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime of EventSpecific", "Time of Event", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime of EventZ - Day", "Time of Event", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesTime of EventZ - Night", "Time of Event", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVehicle Type", "Vehicle Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVehicle TypeAlt Vech - ATV", "Vehicle Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVehicle TypeAlt Vech - Dirt Bike", "Vehicle Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVehicle TypeAlt Vech - Motorcycle", "Vehicle Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVehicle TypeAlt Vech - Other", "Vehicle Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVehicle TypeAlternative Vehicle", "Vehicle Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVehicle TypeBroad Type", "Vehicle Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVehicle TypeBus", "Vehicle Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVehicle TypeHelmet (Alt Vech)", "Vehicle Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVehicle TypeLorry", "Vehicle Type", Parent_Node)) %>%
  mutate(Parent_Node = ifelse(Parent_Node == "NodesVehicle TypeMake-Model", "Vehicle Type", Parent_Node))


table(code_extract$Parent_Node)


# preserve the dataset in the y drive 
write.csv(code_extract, "FILE_PATH\\Amanda\\Qualitative_Project\\MVC\\cleaned_extract.csv")

