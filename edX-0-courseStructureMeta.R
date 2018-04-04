## ====================================================================================================== ##
# Title:        Extracting edX course metadata, course structure, and content module sequencing
# Project:      edX user trajectory analysis
# 
#     Copyright 2017 Michael Ginda
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
#     
#     http://www.apache.org/licenses/LICENSE-2.0
#     
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.
#      
#
# Authors:      Michael Ginda
# Affiliation:  Indiana University
# 
# Description:  This script extracts metadata that describes EdX course administration, and builds 
#               data frame of course modules (structural and content) based on the module metadata,
#               which is used in subsequent processing of student logs to look up parent-child 
#               relationships between modules; to create a node list for the learner trajectory networks;
#               and to aggregate analysis of module access patterns by students in a course.
#
#               The script first reads in an edX course's course structure hierarchy, which comes as 
#               JSON formatted file. The file is processed to extract the course level administrative data. 
#               Next, a data frame is created that list all modules found in the course hiearachy. The 
#               script identifies and orders course modules based on identified organizational structure
#               and sequence of content module.
#  
# File input stack: 
#            1) An edX course data package directory contining one "*.json" course structure file:
#               {org}-{course Identifier}-{term}-course_structure-{server}-analytics.json
#               (source: edX research documentation)
# 
# Package dependencies: jsonlite, reshape2, plyr, and tcltk
#
# Changelog:
#   2017.11.13. Initial Code
#   2018.02.06. Course structure extracted and formatting 
#   2018.02.07. Working version of code created
#   2018.02.08. Fixed sorting for vertical modules, removed numeric sort columns, 
#               added courseID field to align with the student event log formatter script.
#   2018.04.04  Clean-up of project description, title, alignment across edX course data processing
#              pipeline.
## ====================================================================================================== ##

######### Setup ########## 
## _Clean the environment ####
rm(list=ls()) 

## _start timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute elapsed time of script)

## _Load required packages #####
require("jsonlite")   #for working with JSON files (esp. read and write)
require("reshape2")   #for melting data
require("plyr")       #for Join
require("tcltk2")     #for OS independant GUI file and folder selection

####Functions 
#courseMeta
# @param filelist is the file location of course structure data
# @param path indicates the path used to save outputs files
##The courseMeta function is reads in edX Course Structure JSON files and extracts course metadata
##Specifically it captures the module type of the root node (verifying course data is captured);
##course enrollment and run dates.
courseStrMeta <- function(fileList,path){    
  #Extracts initial course structure
  course <- fromJSON(fileList)
  
  #Locates root note in JSON file and creates a df
  coursemeta <- as.data.frame(unlist(course[grepl("type\\@course",names(course))==T][[1]]))
  cols <- row.names(coursemeta)
  coursemeta <- data.frame(t(coursemeta))
  names(coursemeta) <- cols
  rownames(coursemeta) <- 1
  #creates course ID from course structure child node 1
  id <- strsplit(strsplit(as.character(coursemeta$children1),":")[[1]][2],"\\+")
  id <- paste0(id[[1]][1],"+",id[[1]][2],"+",id[[1]][3])
  #reduces to relevant categories
  coursemeta <- coursemeta[,c("metadata.display_name","category","metadata.start","metadata.end","metadata.enrollment_start","metadata.enrollment_end")]
  coursemeta <- cbind(id,coursemeta)
  #converts dates to POSIXCT
  coursemeta[,4] <- as.POSIXct(coursemeta[,4], tz="EST",format='%Y-%m-%dT%H:%M:%SZ')
  coursemeta[,5] <- as.POSIXct(coursemeta[,5], tz="EST",format='%Y-%m-%dT%H:%M:%SZ')
  coursemeta[,6] <- as.POSIXct(coursemeta[,6], tz="EST",format='%Y-%m-%dT%H:%M:%SZ')
  coursemeta[,7] <- as.POSIXct(coursemeta[,7], tz="EST",format='%Y-%m-%dT%H:%M:%SZ')
  #Saves metadata
  write.csv(coursemeta,file=paste0(path_output,"/",id,"-meta.csv"),row.names = F)
  rm(coursemeta,cols)
  
  #Creates Module list dataframe
  modList <- data.frame()
  moduleID <- as.data.frame(names(course))
  #Extracts Module List as a CSV dataframe
  for(i in 1:nrow(moduleID)){
    mod <- unlist(course[names(course)==moduleID[i,]])
    names(mod) <- sub('.*\\.','',names(mod))
    mod <- cbind(as.character(moduleID[i,]), as.data.frame(t(mod)))
    names(mod)[1] <- "id"
    if(i==1){
      modList <- rbind(modList,mod) 
      } else {
      mod[setdiff(names(modList),names(mod))] <- NA
      modList[setdiff(names(mod),names(modList))] <- NA
      modList <- rbind(modList,mod) 
      }
  }
  #Subset from module list full ID, module type, display name, markdown text, and children
  modList <- modList[,c("id","category","display_name","markdown","children",
                       paste0("children",seq(1,length(grep("children",names(modList)))-1,1)))]
  rm(mod,course)
  
  #Identifies Parent Module for Each Module based on reported children
  temp <- melt(modList[,-c(2:4)],id.vars=1,na.rm =T)
  temp <- temp[order(temp$id),]
  rownames(temp) <- 1:nrow(temp)
  temp$id <- factor(temp$id)
  temp$childOrder <- NA
  moduleID <- levels(temp$id)
  for(i in 1:length(moduleID)){
    temp[temp$id==moduleID[i],]$childOrder <- seq_along(temp[temp$id==moduleID[i],]$value)
  }
  temp <- temp[,c(3,1,4)]
  temp$id <- factor(temp$id, levels=levels(modList$id))
  names(temp) <- c("id","parent","childOrder")
  modList <- join(modList,temp)
  modList <- modList[,c(1:4,21,22)]
  rm(temp)
  
  #Shortens the module ID and the parent ID strings to 32hex
  modList$mod_hex_id <- NA
  modList$parent <- as.character(modList$parent)
  for(i in 1:nrow(modList)){
    modList[i,]$mod_hex_id <- strsplit(as.character(modList$id),"@")[[i]][3]
    modList[i,]$parent <- strsplit(as.character(modList$parent),"@")[[i]][3]
  }
  modList$mod_hex_id<- factor(modList$mod_hex_id)
  modList$parent <- factor(modList$parent,levels=levels(modList$mod_hex_id))
  
  #Sets level of hierarchy for each module in course
  modList$level <- -1
  #Sets Course Root node to child order to 0
  modList[is.na(modList$childOrder),c(6,8)] <- 0 
  
  #Identifies Chapter Modules
  modList[modList$parent==modList[modList$level==0,]$mod_hex_id & !is.na(modList$parent),]$level <- 1
  modList[is.na(modList$parent),]$parent <- modList[is.na(modList$parent),]$mod_hex_id
  moduleID <- modList[modList$level==1,]$mod_hex_id
  #Identifies Sequential Modules
  for(i in 1:length(moduleID)){
    modList[modList$parent==moduleID[i],]$level <- 2
  }
  moduleID <- modList[modList$level==2,]$mod_hex_id
  #Identifies Vertical Modules
  for(i in 1:length(moduleID)){
    modList[modList$parent==moduleID[i],]$level <- 3
  }
  moduleID <- modList[modList$level==3,]$mod_hex_id
  #Identifies Content Modules
  for(i in 1:length(moduleID)){
    modList[modList$parent==moduleID[i],]$level <- 4
  }
  
  #Set up Parent Module Identifiers
  modList$vrtModPar <- modList$seqModPar <- modList$chpModPar <- NA
  
  #Parent Verticle Module Look-up for Sorting
  modList[modList$level==4,]$vrtModPar <- as.character(modList[modList$level==4,]$parent)
  modList[modList$level==3,]$vrtModPar <- as.character(modList[modList$level==3,]$mod_hex_id)
  modList$vrtModPar <- factor(modList$vrtModPar,levels=levels(modList$mod_hex_id))
  
  #Parent Sequential Module Look-up for Sorting
  moduleID <- unique(modList[modList$level==3,]$mod_hex_id)
  for(i in 1:length(moduleID)){
    modList[!is.na(modList$vrtModPar) & modList$vrtModPar==moduleID[i],]$seqModPar <- as.character(modList[modList$mod_hex_id==moduleID[i],]$parent)
  }
  modList[modList$level==2,]$seqModPar <- as.character(modList[modList$level==2,]$mod_hex_id)
  modList$seqModPar <- factor(modList$seqModPar,levels=levels(modList$mod_hex_id))
  
  #Parent Chapter Module Look-up for Sorting
  moduleID <- modList[modList$level==2,]$mod_hex_id
  for(i in 1:length(moduleID)){
    modList[!is.na(modList$seqModPar) & modList$seqModPar==moduleID[i],]$chpModPar <- as.character(modList[modList$mod_hex_id==moduleID[i],]$parent)
  }
  modList[modList$level==1,]$chpModPar <- as.character(modList[modList$level==1,]$mod_hex_id)
  modList$chpModPar <- factor(modList$chpModPar,levels=levels(modList$mod_hex_id))
  
  #Parent Module - Child Order Look-Up Fields for Ordering
  modList$contentOrdSet <- modList$vrtOrdSet <- modList$seqOrdSet <- modList$chpOrdSet <- 0
  
  #Assigning Parent Chapters Module Child Order to Child Branches and Leaves
  moduleID <- modList[modList$level==1,]$mod_hex_id
  for(i in 1:length(moduleID)){
    modList[!is.na(modList$chpModPar) & modList$chpModPar==moduleID[i],]$chpOrdSet <- modList[modList$mod_hex_id==moduleID[i],]$childOrd
  }
  #Assigning Parent Sequential Module Child Order to Child Branches and Leaves
  moduleID <- modList[modList$level==2,]$mod_hex_id
  for(i in 1:length(moduleID)){
    modList[!is.na(modList$seqModPar) & modList$seqModPar==moduleID[i],]$seqOrdSet <- modList[modList$mod_hex_id==moduleID[i],]$childOrd
  }
  #Assigning Parent Vertical Module Child Order to Child Branches and Leaves
  moduleID <- modList[modList$level==3,]$mod_hex_id
  for(i in 1:length(moduleID)){
    modList[!is.na(modList$vrtModPar) & modList$vrtModPar==moduleID[i],]$vrtOrdSet <- modList[modList$mod_hex_id==moduleID[i],]$childOrd
  }
  #Fourth Level sorting (to distinguish vertical and content mods)
  modList[modList$level==4,]$contentOrdSet <- 1
  
  #Order Rows in data set to sequence order
  modList <- modList[order(modList$chpOrdSet,modList$seqOrdSet,modList$vrtOrdSet,modList$contentOrdSet,modList$childOrder,decreasing = F),]
  rownames(modList) <- 1:nrow(modList)
  modList$order <- seq(modList$id)
  #Removes Ordering Fields (redundant)
  modList <- modList[,-c(12:15)]
  #Adds a modparrent child lookup, used in processing student event logs
  modList$modparent_childlevel <- paste0(modList$parent,"/",modList$childOrder)
  modList$course <- id
  #Reorder columns
  modList <- modList[,c(1,7,14,2:4,12,6,8,9:11,5,13)]
  names(modList) <- c("id","mod_hex_id","courseID","mod_type","name","markdown","order",
                      "childOrder","treelevel","chpModPar","seqModPar","vrtModPar",
                      "parent","modparent_childlevel")
  write.csv(modList,file=paste0(path_output,"/",id,"-module-lookup.csv"),row.names = F)
}

######### Main ########## 
#Assigns path to read in data sets 
path_data = tclvalue(tkchooseDirectory())
#Assigns path to save processing outputs
path_output = tclvalue(tkchooseDirectory())

## _Build list of all event files for course####
#Store all the filenames of JSON formatted edX event logs within a user selected directory 
# (files ending with ".log.gz").
fileList <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = path_data,
                       pattern = ".json$")
#courseMeta Test
courseStrMeta(fileList,path=path_output)

######### Finishing Details ########## 
#Indicate completion
message("\n**** Complete! ****\n")

## _Script processing time feedback #####
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print(proc.time() - start)

## _Clear environment variables
rm(list=ls())   
