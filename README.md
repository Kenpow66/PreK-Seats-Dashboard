---
title: "KHOLE Github Repository"
author: "Lauren E Price"
output: 
  html_document(
    toc = FALSE,
    theme = "simplex"
    )
--- 

# Scripted projects for C-Suite 

## Background lists needed for Programs
Multiple static data sources will be used in different steps.  
Pull in the following as TIBBLES during setup:
- cilCrosswalk.csv for naming conventions
  - "pathway"
- schsWithPK for identifying which campuses have PK and have No PK
  - "pathway2"
- schsECSE for identifying which campuses have ECSE and have No ECSE (room cap 11)
  - "pathway3"
- schsHeadStart identifying which campuses have HS (cap 18) and have No HS (cap 20)
  - "pathway4"
- NES and NES-A for reporting purposes, not for n-count caps
  - "pathway5"
- ECSE teachers who are going to be capped at ELEVEN students of any kind
  - "pathway6" 
  - Script will need to be run and then scrubbed every iteration to clean up:
  - 

## Max possible seats per campus given configuration of each
- Pull day's master schedule for all campuses
  - SIS > Enterprise Reporting > Master Schedule with Counts UAT
  
- Use cilCrosswalk to glue on campus and short name

- Drop rows at all campuses that do not offer PK

- Find the homerooms and section IDs for EE PK3 PK4 KG per campus
  - 0P999GEN	Read Test PK
  - 0K999GEN	Read Test KG
  
- Determine whether any sections with students are staffed by TEMP
- Flag the specific teachers with the ECSE flags
- Determine which Ts are doing a split room (!), including
  - EE split with other grade levels
  - PK split with KG
- If ECSE = TRUE, find the number of kids enrolled in all sections with ID at or above 600
  - Drop two ided-as-ECSE Tchrs who have no enrollees in sections > 600 for grades > 1 
  - 
  

## Currently enrolled students 
- Pull CSV of enrolled students per campus per grade
- Determine number of enrolled EE students per campus
- Determine n-counts of EE, PK3, PK4, and KG per campus

## 


