## ===================================================== ##
# Title:        Extracting all edX events for multiple users ####
# Project:      edX user trajectory analysis
# 
# Copyright 2017 Michael Ginda & Krishna Madhavan
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
# Authors:      Michael Ginda, Krishna Madhavan, Kerrie Douglas, Taylor Williams
# Affiliation:  Indiana University, Purdue University
#
# 
# Description:  This script processes a user's activity from the edX event log CSV file.
#               The script reformats time for use in R, and then sorts and reorders log files
#               The script then removes events without associate learning modules or session ID.
#               Script applies session data to all remaining events and creates/extracts module IDs
#                 
# File input stack: 
#               1) Set of known UserIDs
#               2) A folder contining one or more "*.csv" event log file(s)    (source: processed edX logs)
# 
# 
# Package dependencies: zoo, magrittr, stringr, tcltk
#
#
# Changelog:
#   2017.10.23. Initial Code
#   2017.10.26. Initial version of function
#   2017.10.30. Updated function to fix session backfill error; remove events that are not useful for analysis
#   2017.10.31. Final version of function with new session backfill fix, and test subject data processed
#   2017.11.02  Clean-up version for sharing on repository
#   2017.11.03  Correct error for adding module IDs for course information, student progrees, and wiki page events
## ===================================================== ##

######### Setup ########## 
## _Clean the environment ####
rm(list=ls()) 

## _start timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute elapsed time of script)

## _Load required packages #####
require("magrittr")   #Pipe tool
require("zoo")        #dataPrep
require("stringr")    #string parsing
require("tcltk2")     #for OS independant GUI file and folder selection

####Functions
#logFormatter 
##The logFormatter function is a modification of code provided by Purdue University team
##to allow mass extracting individual set of student logs based on known set of student IDs for an 
##edX course. The function creates a unique log file for each student ID in the list, 
##saved as either a JSON or CSV formatted file. The function currently set up to save as CSV,
##alternatively can be set for user defined action such as format=T csv if format=F, JSON set up possible.

logFormatter <- function(fileList,path){
  #Creates sequence of student logs files to parse
  numLogs <- length(fileList) 
  for(i in 1:numLogs){
    message("Processing log file ", i, " of ", numLogs)
    print(proc.time() - start)
    #Load data set
    data <- read.csv(fileList[i])
    
    ##Data format updates and column creation/organization
    #Updates Time field to R compliant format
    sapply(strsplit(as.character(data$time),split='+',fixed=T),function(x)(x[1])) %>% 
      as.POSIXlt(tz="EST",format='%Y-%m-%dT%H:%M:%S') -> data$time 
    #Reorders logs by the time field, and reset rownames
    data <- data[order(data$time),]
    rownames(data) <- 1:nrow(data) 
    #Creates a ourse ID to remove "course-v1:" text, which is not used in the mod ID.
    courseID <- strsplit(as.character(data$context.course_id[1]),'\\:')[[1]][2]
    #Creates column to place learning object moduleID idenfitied for each event
    data[c("module.key")] <- NA
    #Creates column to identify the appropriate current child of high-level modules
    data[c("mod.child.ref")] <- NA
    
    #Backfill fix for server events without a session ID; events to be removed.
    data <- rbind(data,NA)
    levels <- levels(data$session)
    levels[length(levels)+1] <- "lastsession"
    data$session <- factor(data$session, level=levels)
    data[nrow(data),]$session <- c("lastsession")
    
    #Identifies student session for server actions by backfilling session ID
    data$session <- na.locf(data$session,fromLast=T)
    
    #Removes dummy row from session fix
    data <- data[-nrow(data),]
    
    #Creates a dummy column to ID events without course module ID events, events are kept if value is 1
    data["kp"] <- 1
    
    ##Remove events without associated learning object modules
    ##These come before all actions actions, because they are known missing data cases;
    ##and server events lack Session IDs, which cause problem if they appear at the 
    ##end of asorted data file when you back fill session IDs to events.
    #Identifies events that do not relate to a learning object (course info, progress, wiki pages) 
    #data[grepl('\\{\\}', data$event)==T & grepl('courseware',data$event_type)==F,]$kp <- 0
    
    #Resets Page_close events to 1 (even though no associated module ID)
    #Supports backfill of Session IDS and calculation of time spent on given module
    #data[grepl('page\\_close', data$event_type)==T,]$kp <- 1 
    
    #Identifies Edx Enrollment Events that are not course specific 
    #Applied to only some students, as some student have taken prior edx course
    if(length(data[grepl('edx\\.',data$event_type)==T,])>0){
      data[grepl('edx\\.',data$event_type)==T,]$kp <- 0
    }
    
    #Applied if student has uploaded a file to the open assessment events
    if(length(data[grepl('openassessment\\.upload',data$event_type)==T,]) > 0){
      data[grepl('openassessment\\.upload',data$event_type)==T,]$kp <- 0
    }
    
    #removes data with a value of 0
    data <- data[data$kp!=0,]
    #Removes the KP field from the data set
    data <- data[-c(length(data))]
    
    ##Remmoved code: Removes Page_close events, 
    #if(length(data[data$event_type == c("page_close"),]$event_type) > 0){
    #  data <- data[data$event_type != c("page_close"),]
    #    }
    
    ##Extract or recreate module ID from the log data as appropriate to known cases
    #Problem events (problem_check, save_problem_success, showanswer) and Openassessmentblock events (all but .upload_file)
    data$module.key <- paste0(data$context.module.usage_key)
    
    #Non-learning object page events
    #Identifies events that do not relate to course information
    if(length(data[grepl('\\{\\}', data$event)==T & grepl('info',data$event_type)==T,]$event_type) > 0 ){
      data[grepl('\\{\\}', data$event)==T & grepl('info',data$event_type)==T,]$module.key <- 
        paste0("block-v1:",courseID,"+type@info")
    }
    #Identifies events that do not relate to a students progress
    if(length(data[grepl('\\{\\}', data$event)==T & grepl('progress',data$event_type)==T,]$event_type) > 0 ){
      data[grepl('\\{\\}', data$event)==T & grepl('progress',data$event_type)==T,]$module.key <- 
        paste0("block-v1:",courseID,"+type@progress")
    }
    #Identifies events that do not relate to a course wiki pages
    if(length(data[grepl('\\{\\}', data$event)==T & grepl('wiki',data$event_type)==T,]$event_type) > 0 ){
      data[grepl('\\{\\}', data$event)==T & grepl('wiki',data$event_type)==T,]$module.key <- 
        paste0("block-v1:",courseID,"+type@wiki")
    }
    #Problem_show events
    if(length(data[grepl('problem_show',data$event_type)==T,])>0){
      data[grepl('problem\\_show',data$event_type)==T,]$module.key <- 
        paste0(vapply(strsplit(as.character(data[grepl('problem\\_show',data$event_type)==T,]$event), '\\"'),'[',4,FUN.VALUE=character(1)))
    }
    #Videos (seek, load, play, stop, speed)
    if(length(data[grepl('\\w\\_video',data$event_type)==T,]$event) > 0){
      #Extracts module ID number and creates module ids for video watching and speed change events
      data[grepl('\\w\\_video',data$event_type)==T,]$module.key <- 
        paste0("block-v1:",courseID,"+type@video+block@",str_extract(data[grepl('\\w\\_video',data$event_type)==T,]$event,"[:alnum:]{32}"))
    }
    #video Transcripts
    if(length(data[grepl('\\w\\_transcript',data$event_type)==T,]$event) > 0){
      #Extracts module ID number and creates module ids for video transcript events
      data[grepl('\\w\\_transcript',data$event_type)==T,]$module.key <- 
        paste0("block-v1:",courseID,"+type@video+block@",str_extract(data[grepl('\\w\\_transcript',data$event_type)==T,]$event,"[:alnum:]{32}"))
    }
    #HTML Blocks (Level 2 and 3)
    if(length(data[grepl('/courseware',data$event_type)==T,])>0){
      #Extracts module ID number and creates ids for courseware events (highlevel modules)
      data[grepl('/courseware',data$event_type)==T,]$module.key <- 
        paste0("block-v1:",courseID,"+type@sequential+block@",vapply(strsplit(as.character(data[grepl('/courseware',data$event_type)==T,]$event_type), '\\/'),'[',6,FUN.VALUE=character(1)))
      #generates child reference for module (all of these modules should be level 2 or 3 of course hierarchy)
      #event to go with first child of lowest left decendants
      data[grepl('/courseware',data$event_type)==T,]$mod.child.ref <- 1
    }
    #Seq navigation - Goto
    if(length(data[grepl('\\,\\s\\"widget_placement\\"',data$event)==T,]$event) > 0 ){
      #Extracts parent module ids for sequential Goto events (which have a different parsed log format than other two sequence events)
      data[grepl('\\,\\s\\"widget_placement\\"',data$event)==T,]$module.key <- 
        paste0(vapply(strsplit(as.character(data[grepl('\\,\\s\\"widget_placement\\"',data$event)==T,]$event),'\\"'),'[',18,FUN.VALUE=character(1)))
      #Extracts child leaf for sequential Goto events at lower level of hiearchy (current state)
      data[grepl('\\,\\s\\"widget_placement\\"',data$event)==T,]$mod.child.ref <-
        paste0(vapply(strsplit(as.character(data[grepl('\\,\\s\\"widget_placement\\"',data$event)==T,]$event),'\\"'),'[',9,FUN.VALUE=character(1)))
    }
    #Seq navigation - Prev Next
    if(length(data[grepl('\\{\\"widget_placement',data$event)==T,]$event) > 0 ){
      #Extracts parent module ids for sequential Prev and Next events (which have a different parsed log format than other two sequence events)
      data[grepl('\\{\\"widget_placement',data$event)==T,]$module.key <- 
        paste0(vapply(strsplit(as.character(data[grepl('\\{\\"widget_placement',data$event)==T,]$event),'\\"'),'[',16,FUN.VALUE=character(1)))
      #Extracts child leaf for sequential Prev & Next events at lower level of hiearchy (current state)
      data[grepl('\\{\\"widget_placement',data$event)==T,]$mod.child.ref <-
        paste0(vapply(strsplit(as.character(data[grepl('\\{\\"widget_placement',data$event)==T,]$event),'\\"'),'[',13,FUN.VALUE=character(1)))
    }
    #Cleans up module child references, removing puntuation and spaces from the field
    data$mod.child.ref <- str_replace_all(data$mod.child.ref,"[^[:alnum:]]","") 
    
    #Creates user ID for file saving
    id<- data$context.user_id[1]
    write.csv(x = data, file = paste0(path,"/",id,".csv"),
              row.names = F)
    data <- NULL
  } 
}

######### Main ########## 

#Data can consist of a single list of student user IDs collected from the EdX course database
#data <- read.csv("dir/[USER_ID_LIST].csv",header=T)

#Creates paths used to locate directory for research data sets and save processing outputs
path_data = tclvalue(tkchooseDirectory())
path_output = tclvalue(tkchooseDirectory())

logFilePaths <- paste0(path_data,d$ids,".csv")
rm(d)

#Log Capture function for list of users
logFormatter(fileList=logFilePaths, path=path_output)

######### Finishing Details ########## 
#Indicate completion
message("\n**** Complete! ****\n")

## _Script processing time feedback #####
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print(proc.time() - start)

## _Clear environment variables
rm(list=ls())




