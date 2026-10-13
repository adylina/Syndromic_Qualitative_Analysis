# Syndromic_Qualitative
Scripts for the management of data related to qualitative analysis projects

## Overview
In 2019, the Washington State Department of Health (DOH) Rapid Health Information NetwOrk (RHINO) program began using qualitative methods to explore the contextual information available in the free-text fields of emergency department visits. The scripts contained here were used to

1. Pull data out of the National Syndromic Surveillance Program (NSSP) Electronic Surveillance System for the Early Notificiation of Community-based Epidemics  (ESSENCE) platform using APIs 

2. Prepare the data to be imported into NVivo 12 for qualitative analysis
    
    2a. Remove direct identifiers (patient date of birth, ZIP Code of residence, medical record number)
    
    2b. Strip a large number of clinical and administrative fields irrelevant to the analysis
    
    2c. Exclude visits for which content was unavailable in both the triage notes and clinical impression (as these were key to the analysis)
    
    2d. Take a random sample of visits for analysis 

3. Pull the coded text extracts from NVivo back into R for cleaning
    
    3a. Rename columns
    
    3b. Rename parent and child nodes
    

## High-level information

1. The APIs call to a secure data system (NSSP ESSENCE) for which users will need to have credentials to access line-level records. The queries used for the APIs do not contain geographic limiters, so they should reliably pull data for other jurisdictions.

2. The keyring() package is used to call to Windows Credential Manager for the ESSENCE credentials of the user. Guidance is available from NSSP on how to use APIs with ESSENCE.

3. The scripts assume the the user is pulling in coded data extracts from NVivo. It is, however, possible to use just the first sections of the scripts to bring in raw data from ESSENCE. 
