## ====================================================================================== ##
# Title:        Processing and formatting student's edX events logs for analysis          
# Project:      edX learner trajectory analysis
# 
#     Copyright 2017-2019 Michael Ginda
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
# Authors:      Michael Ginda
# Affiliation:  Indiana University
# 
# Description:  This script processes the raw student event logs event extracted 
#               from an edX course's daily event logs, provided in an edX Data Package.
#               The script cleans and reformat student event logs for learner 
#               trajectory analysis and modeling.
#
#               The scripts uses a custom function to identify the different event log 
#               use cases (e.g. students with no events; students who do not access 
#               content modules; and active students); calculates time between events,
#               identifies user sessions based on threshold for temporal gaps; the 
#               script then creates a white list of event records to keep, records are 
#               removed for two type reasons:
#                      1) interaction is with non-course content modules or 
#                         enrollment activity;
#                      2) event provided redundant information (e.g. problem module 
#                         browser events and server responses) and partial data;
#                         OR did not show meaningful interactions (e.g load_video events);
#                         OR were infrequently used by students in courses;
#               see in line comments for purpose of event removal. See logFormatter 
#               function parameters for details for selecting events that are appropriate
#               for removal.
#
#               After non-relevant event records are removed, navigation event logs 
#               that reference the second level of the course hierarchy (vertical pages)
#               are updated to identify fourth level child module being referenced in the
#               navigation event. The change places all logged events at the same level 
#               of the course hierarchy.
#
#               Events without a module order are removed, as they are outside of course
#               structure. Events without a given type are set to "mod_access". Events
#               with session times greater than or equal to 60 minutes are updated to 
#               to the mean time for events actions of the same type made by a learner.
#               Student event logs are saved as "{userID}.csv" files in the project
#               directory "./studentevents_processed/".
#
#               Finally, the script exports lists of students that indicate their activity 
#               status: inactive, active but has unusable activity, or is active and usable.
#               These files are saved in the project directory "./userlists/"
#
# File input stack: 
#            1) A processed edX Course structure and content module list:
#               - {org}+{course}+{term}-module-lookup.csv;
#               - extracted by script, edX-0-courseStructureMeta.R;
#            2) A list of student userIDs from an edX course:
#               - {org}-{course}-{term}-auth_user-students.csv;
#               - extracted by script, edX-1-studentUserList.R;
#            3) A "studentevents" directory containing one or more student
#               CSV event log file(s):
#               - {userID}.csv;
#               - extracted by script, edX-2-eventLogExtractor.R
#
# Output files:                        
#            1) A set of processed data tables of event action logs for each student:
#               - {userID}.csv;
#               - Used in scripts:
#                 * edX-4-learnerTrajectoryNet.R
#            2) A list capturing student userIDs who are active in the course,
#				        and have usable actions:
#               - {org}-{course}-{term}-auth_user-students-active.csv;
#               - Used in scripts:
#                 * edX-4-learnerTrajectoryNet.R;
#            3) A list capturing student userIDs who are active in the course,
#				        but have no usable actions:
#               - {org}-{course}-{term}-auth_user-students-unusableActivity.csv;
#            4) A list capturing student userIDs who are inactive in the course,
#				        and have no actions in their event logs:
#               - {org}-{course}-{term}-auth_user-students-inactive.csv; 
#
# Package dependencies: magrittr, stringr, plyr, tcltk, zoo
#
# Change log:
#   2019.05.22 Updated script to update processed log exports based on log import and
#              processing results; improve temporal duration calculations; added process
#              for drag-and-drop modules; updated processing for progress and account
#              setting events; improved processing of content navigation events (jumpTo
#              and link_clicked events).
#
## ====================================================================================== ##
#### Environmental Setup ####
rm(list=ls()) 
#rm(courseStr,courseID,curUserIDS,eventLog,fileList,start,studentLogs,subDir,logExtractor,i)

#Load required packages 
require("tcltk2")     #for OS independent GUI file and folder selection
require("zoo")        #temporal and na.locf function
require("magrittr")   #Pipe tool
require("stringr")    #string parsing
require("plyr")       #DPLYR

#### Paths #### 
#Generic Set up
#Assigns a path to open prior script outputs and to save new processing output files.
path_output = tclvalue(tkchooseDirectory())

## Create data processing output sub-directories for student without events
#students with zero events on load and those with zero events after post-data processing 
subDir = c("noEventsProc")
for(i in 1:length(subDir)){
  if(!file_test("-d", file.path(paste0(path_output,"/studentevents_processed/"), subDir[i]))){
    if(file_test("-f", file.path(paste0(path_output,"/studentevents_processed/"), subDir[i]))){
      stop("Path can't be created because a file with that name already exists.")
    } else {
      dir.create(file.path(paste0(path_output,"/studentevents_processed/"), subDir[i]))
    }
  }
}

#Creates list of files for student event log processing
#Load in CSV of students IDs
fileList <- list.files(path=paste0(path_output,"/studentevents/"),pattern=".csv")

#Load course structure data module-lookup
#CSV of course modules data extracted from the Course Structure, found in the
#edX student user database file: {org}-{course}-{run}-module-lookup.csv
courseStr <- list.files(full.names = TRUE, recursive = FALSE, 
                        path = paste0(path_output,"/course/"),
                        pattern = "module-lookup.csv")
#Extracts course identifier
courseStr <- read.csv(file=courseStr, header=T)
courseID <- as.character(courseStr$courseID[1])

#### Log Processing Variables ####
## Sets Variables and parameters used in log processing loop
#Start timer to track how long the script takes to execute
start <- proc.time() #save the time (to compute elapsed time of script)

#path is the sets the path for where output files will be saved.
path=path_output

#timeB_val is a numeric value setting the number of minutes that must pass for a  
# temporal session (tsess) to have ended; automatically set at 60 minute threshold.
timeB_val=60

#wl_min is a numeric value indicates a threshold for the number usable event logs
#required for additional processing
wl_min = 5

#timeBEst is a logical parameter that indicates if processing script
# should provide updated time period estimates for events where the period is equal to or greater 
# than the timeB (temporal session break) parameter; events with large periods are updated with
# the median period value that is calculcated based on events with the same type found in a
# given student's log. 
timeBEst=TRUE

#NC indicates whether non-content module pages in an EdX course (e.g Course Information, 
# wikis, student progress) are maintained in the analysis.
nc=FALSE

#PSE indicates whether server events associated with problem modules, specifically,
# showanswer and save_problem_success events, are maintained in analysis.
pse=FALSE

#VID indicates video events associated with infrequent use (transcript and closed 
# caption evnts), and video load events, which do not show meaningful use of content module.
vid=FALSE

#TRANS indicates video transcript events that are removed from the data set
#activity in the course.
trans=FALSE

#DRAG indicates drag and drop events that are redundant or provide inaccurate data for student
#activity in the course.
drag=FALSE

#OAS indicates that how openassessment event_types are handle to remove redundancy in logs
oas=FALSE

#Indicates number of log files to be processed by the loop.
numLogs <- length(fileList)
for(i in 1:numLogs){ 
  message("Processing log file ", i, " of ", numLogs)
  print(proc.time() - start)
  #Load data set
  data <- read.csv(paste0(path_output,"/studentevents/",fileList[i]))
  #Students without Data
  if(nrow(data)==0){
    data <- as.data.frame(matrix(data=NA,nrow=1,ncol=24))
    names(data) <- c("org_id","course_id","user_id","module_id","mod_hex_id","order","parent_id",
                     "module_type","time","period","session","tsess","context.path","event","event_type","event_source",
                     "problem_id","attempts","grade","max_grade","state.done","student_answer","submission","success")
    fileName <- as.data.frame(strsplit(fileList[i], split="\\/"))
    write.csv(x=data, file=paste0(path,"/studentevents_processed/",subDir[1],"/",fileName[nrow(fileName),]), row.names = F)
    data <- NULL
    #Students with too few rows
  } else if(nrow(data)<=wl_min){
    data <- cbind(data, as.data.frame(matrix(data=NA,nrow=nrow(data),ncol=6)))
    data <- data[,c(1,2,3,10,19:22,5,23,6,24,4,7,9,8,11:18)]
    names(data) <- c("org_id","course_id","user_id","module_id","mod_hex_id","order","parent_id",
                     "module_type","time","period","session","tsess","context.path","event","event_type","event_source",
                     "problem_id","attempts","grade","max_grade","state.done","student_answer","submission","success")
    fileName <- as.data.frame(strsplit(fileList[i], split="\\/"))
    write.csv(x=data, file=paste0(path,"/studentevents_processed/",subDir[1],"/",fileName[nrow(fileName),]), row.names = F)
    data <- NULL
  } else {
    #Creates a course ID by removing "course-v1:" text, which is not used in the module ID.
    uid <- data$context.user_id[1]
    
    ##Data format updates and column creation/organization
    #Updates Time field to R compliant format
    sapply(strsplit(as.character(data$time),split='+',fixed=T),function(x)(x[1])) %>% 
      as.POSIXlt(tz="EST",format='%Y-%m-%dT%H:%M:%S') -> data$time 
    #Reorders logs by the time field, and reset rownames
    data <- data[order(data$time),]
    rownames(data) <- 1:nrow(data)
    
    #First period calculation measures period of time between all events
    #This calculation is used to make a temporal session calculation 
    #Calc made in minutes
    rbind(as.data.frame(as.numeric(difftime(data[2:nrow(data),]$time,data[1:nrow(data)-1,]$time,units="mins"))),NA) -> period
    c("period") -> names(period)
    #Updates all records where period is greater than or equal to the time break set by user (or automatically of 60 minutes) 
    #This needs to be updated to change the value from a rounded 60 to another more meaningful value
    #For example, the value equals the mean time period measured for the module or event type.
    period[period >= timeB_val & !is.na(period), ] <- as.numeric(timeB_val)
    data <- cbind(data,period)
    
    #Creates temporal session based on the outliers in the period field.
    data$tsess <- NA
    #Searches for outliers periods to demarcate the end of a user session, 
    #id set by seq from 1 to max periods identified; if no max values are found,
    #it is assumed there was only one session.
    if(nrow(data[data$period >= timeB_val,])>1){
      data[data$period >= timeB_val & !is.na(data$period),]$tsess <- seq(from=1,to=nrow(data[data$period >= timeB_val & !is.na(data$period),]))
    } else(data$tsess <- 1)
    #For logs with multiple sessions ids, there is a chance that the last event has a blank
    #tsess value, and needs to have a final session ID provided.
    if(is.na(data[nrow(data),]$tsess)){
      fs <- max(data$tsess,na.rm=T)+1
      data$tsess <- as.factor(data$tsess)
      levels <- levels(data$tsess)
      levels[length(levels)+1] <- fs
      data$tsess <- factor(data$tsess, level=levels)
      data[nrow(data),]$tsess <- fs
    }
    #Backfills tsess session ids to all events after determining all sessions were 
    #identified
    data$tsess <- na.locf(data$tsess,fromLast=T)
    
    #Backfill fix for server events without a session ID; events to be removed.
    data<-rbind(data,NA)
    levels <- levels(data$session)
    levels[length(levels)+1] <- "lastsession"
    data$session <- factor(data$session, level=levels)
    data[nrow(data),]$session <- c("lastsession")
    
    #Identifies student session for server actions by backfilling session ID
    data$session <- na.locf(data$session,fromLast=T)
    
    #Removes dummy row from session fix
    data <- data[-nrow(data),]
    
    #Creates a dummy column to ID events without course module identifiers, events are kept if value is 1
    data["kp"] <- 1
    
    ##Remove events that are not associated learning object modules
    #NC parameter checks if a user wants to keep or remove the non-content modules in event logs:
    #which includes, course information, wiki, student progress, and account settings pages
    if(nc==TRUE){
      #Identifies events that do relate to course information and updates module.key field
      if(length(data[grepl('\\{\\}', data$event)==T & grepl('info',data$event_type)==T,]$event_type) > 0 ){
        data[grepl('\\{\\}', data$event)==T & grepl('info',data$event_type)==T,]$module.key <- 
          paste0("block-v1:",courseID,"+type@info")
      }
      #Identifies events that do relate to a students progress and updates module.key field
      if(length(data[grepl('\\{\\}', data$event)==T & grepl('progress',data$event_type)==T,]$event_type) > 0 ){
        data[grepl('\\{\\}', data$event)==T & grepl('progress',data$event_type)==T,]$module.key <- 
          paste0("block-v1:",courseID,"+type@progress")
      }
      if(nrow(data[grepl('\\/progress', data$event)==T,]) > 0 ){
        data[grepl('\\/progress', data$event)==T,]$module.key <- paste0("block-v1:",courseID,"+type@progress")
      }
      #Idenfities events related to setting adjustments
      if(nrow(data[grepl('\\/settings', data$event)==T,]) > 0 ){
        data[grepl('\\/settings', data$event)==T,]$module.key <- paste0("block-v1:",courseID,"+type@settings")
      }
      #Identifies events that do relate to a course wiki pages and updates module.key field
      if(length(data[grepl('\\{\\}', data$event)==T & grepl('wiki',data$event_type)==T,]$event_type) > 0 ){
        data[grepl('\\{\\}', data$event)==T & grepl('wiki',data$event_type)==T,]$module.key <- 
          paste0("block-v1:",courseID,"+type@wiki")
      }
    } else if (nc==FALSE){ 
      if(nrow(data[grepl('\\{\\}', data$event)==T & grepl('info',data$event_type)==T,]) > 0 ){
        data[grepl('\\{\\}', data$event)==T & grepl('info',data$event_type)==T,]$kp <- 0
      }
      #Identifies events that do not relate to a students progress
      if(nrow(data[grepl('\\{\\}', data$event)==T & grepl('progress',data$event_type)==T,]) > 0 ){
        data[grepl('\\{\\}', data$event)==T & grepl('progress',data$event_type)==T,]$kp <- 0
      }
      if(nrow(data[grepl('\\/progress', data$event)==T,]) > 0 ){
        data[grepl('\\/progress', data$event)==T,]$kp <- 0
      }
      #Idenfities events related to setting adjustments
      if(nrow(data[grepl('\\/settings', data$event)==T,]) > 0 ){
        data[grepl('\\/settings', data$event)==T,]$kp <- 0
      }
      #Identifies events that do not relate to a course wiki pages
      if(nrow(data[grepl('\\{\\}', data$event)==T & grepl('wiki',data$event_type)==T,]) > 0 ){
        data[grepl('\\{\\}', data$event)==T & grepl('wiki',data$event_type)==T,]$kp <- 0
      }
      
    } else stop("invalid 'nc' specification to maintain non-content modules in analysis")
    
    #Identifies Edx Enrollment Events that are not course specific 
    #Applied to only some students, as some student have taken prior edx course
    if(nrow(data[grepl('edx\\.course\\.enrollment\\.',data$event_type),])>0){
      data[grepl('edx\\.course\\.enrollment\\.',data$event_type)==T,]$kp <- 0
    }
    
    #Applied if student has uploaded a file to the open assessment events
    if(nrow(data[grepl('openassessment\\.upload',data$event_type)==T,]) > 0){
      data[grepl('openassessment\\.upload',data$event_type)==T,]$kp <- 0
    }
    
    #Applied if student has 'edx.grades.problem.submitted'
    if(nrow(data[grepl('edx\\.grades\\.problem\\.submitted',data$event_type)==T,]) > 0){
      data[grepl('edx\\.grades\\.problem\\.submitted',data$event_type)==T,]$kp <- 0
    }
    if(nrow(data[grepl('edx\\.grades\\.subsection\\.grade\\_calculated',data$event_type)==T,]) > 0){
      data[grepl('edx\\.grades\\.subsection\\.grade\\_calculated',data$event_type)==T,]$kp <- 0
    }
    
    #Removes select drag and drop events from log
    if(drag==F){
      if(nrow(data[grepl('type\\@drag\\-and\\-drop\\-v2',data$event_type)==T,]) > 0){
        data[grepl('type\\@drag\\-and\\-drop\\-v2',data$event_type)==T,]$kp <- 0
      }
      #Feedback open is logged every second, and does not accurately indicat
      #period of time or number of times a student used feedback on drag and drop
      if(nrow(data[grepl('edx\\.drag\\_and\\_drop\\_v2\\.feedback\\.opened',data$event_type)==T,]) > 0){
        data[grepl('edx\\.drag\\_and\\_drop\\_v2\\.feedback\\.opened',data$event_type)==T,]$kp <- 0
      }
    }
    
    #Removes specified problem server events from event logs
    if(pse==FALSE){
      if(nrow(data[grepl('save\\_problem\\_success', data$event_type)==T,])>0){
        data[grepl('save\\_problem\\_success', data$event_type)==T,]$kp <- 0 
      }
      if(nrow(data[grepl('showanswer', data$event_type)==T,])>0){
        data[grepl('showanswer', data$event_type)==T,]$kp <- 0 
      }
      if(nrow(data[grepl('problem_check', data$event_type)==T & data$event_source=="browser",])>0){
        data[grepl('problem_check', data$event_type)==T & data$event_source=="browser",]$kp <- 0 
      }
    }
    
    #Removes video events with low insight value or use in logs
    if(vid==FALSE){
      #removes load video events
      if(nrow(data[grepl('load\\_video', data$event_type)==T,])>0){
        data[grepl('load\\_video', data$event_type)==T,]$kp <- 0  
      }
      if(nrow(data[grepl('speed\\_change\\_video', data$event_type)==T,])>0){
        data[grepl('speed\\_change\\_video', data$event_type)==T,]$kp <- 0  
      }
      #closed captioning events
      if(nrow(data[grepl('\\w\\cc\\_menu', data$event_type)==T,])>0){
        data[grepl('\\w\\cc\\_menu', data$event_type)==T,]$kp <- 0
      }
    }
    #Removes transcript related events
    if(trans==FALSE){
      #User transcript actions
      if(nrow(data[grepl('\\w\\_transcript',data$event_type)==T,])>0){
        data[grepl('\\w\\_transcript',data$event_type)==T,]$kp <- 0
      }
      #Server generated transcript actions
      if(nrow(data[grepl('publish\\_completion',data$event_type)==T,])>0){
        data[grepl('publish\\_completion',data$event_type)==T,]$kp <- 0
      }
    }
    
    #Removes server navigation events indicating a user moved to a new page
    if(nrow(data[grepl('xmodule_handler',data$event_type) | grepl('jump_to',data$event_type),])>0){
      data[grepl('xmodule_handler',data$event_type) | grepl('jump_to',data$event_type),]$kp <- 0
    }
    
    #Checks to see if there are any rows kept after removing specified events. If no event is to be kept after
    #the field checks, the file is saved to a special directory for these cases.
    if(sum(data[data$kp==1,]$kp) == 0){
      data <- cbind(data, as.data.frame(matrix(data=NA,nrow=nrow(data),ncol=4)))
      data <- data[,c(1:3,10,22:25,5,19,6,20,4,7,9,8,11:18)]
      names(data) <- c("org_id","course_id","user_id","module_id","mod_hex_id","order","parent_id",
                       "module_type","time","period","session","tsess","context.path","event","event_type","event_source",
                       "problem_id","attempts","grade","max_grade","state.done","student_answer","submission","success")
      fileName <- as.data.frame(strsplit(fileList[i], split="\\/"))
      write.csv(x = data, file = paste0(path,"/studentevents_processed/",subDir[1],"/",uid,".csv"),
                row.names = F)
      data <- NULL
    } else {
      ##Extracts or recreate module ID from the log fields for various known modules cases (if not removed by user)
      #Creates column to place learning object moduleID idenfitied for each event
      #removes data with a value of 0
      data <- data[data$kp!=0,]
      #Removes the KP field from the data set
      data <- data[-c(length(data))]
      
      #Creates the module.key field used
      data[c("module.key")] <- NA
      #Creates column to identify the appropriate current child of higher-level modules (i.e. chapters and page sequences)
      data[c("mod.child.ref")] <- NA
      
      #All events that have a module_id are converted to the module.key used in the Course Structure (courseStr)
      if(nrow(data[!is.na(data$module_id),]) > 0 ){
        data[!is.na(data$module_id),]$module.key <- paste0("block-v1:",courseID,"+type@",
                                                           vapply(strsplit(as.character(data[!is.na(data$module_id),]$module_id),'\\/'),'[',3,FUN.VALUE=character(1)),
                                                           "+block@",
                                                           substring(vapply(strsplit(as.character(data[!is.na(data$module_id),]$module_id),'\\/'),'[',4,FUN.VALUE=character(1)),1,32))
      }
      
      ##Extracts child reference for sequential pages
      #Updates course sequential blocks
      if(nrow(data[grepl('/courseware',data$event_type)==T,])>0){
        #Extracts module ID number and creates ids for courseware events (high level modules)
        data[grepl('/courseware',data$event_type)==T,]$module.key <- 
          paste0("block-v1:",courseID,"+type@sequential+block@",vapply(strsplit(as.character(data[grepl('/courseware',data$event_type)==T,]$event_type), '\\/'),'[',6,FUN.VALUE=character(1)))
        #generates child reference for module (all of these modules should be level 2 or 3 of course hierarchy)
        #event to go with first child of lowest left decendants
        data[grepl('/courseware',data$event_type)==T,]$mod.child.ref <- 1
      }
      #Seq navigation - Goto
      if(nrow(data[grepl('\\,\\s\\"widget_placement\\"',data$event)==T,]) > 0 ){
        #Extracts child leaf for sequential Goto events at lower level of hiearchy (current state)
        data[grepl('\\,\\s\\"widget_placement\\"',data$event)==T,]$mod.child.ref <-
          paste0(vapply(strsplit(as.character(data[grepl('\\,\\s\\"widget_placement\\"',data$event)==T,]$event),'\\"'),'[',9,FUN.VALUE=character(1)))
      }
      #Seq navigation - Prev Next
      if(nrow(data[grepl('\\{\\"widget_placement',data$event)==T,]) > 0 ){
        #Extracts child leaf for sequential Prev & Next events at lower level of hiearchy (current state)
        data[grepl('\\{\\"widget_placement',data$event)==T,]$mod.child.ref <-
          paste0(vapply(strsplit(as.character(data[grepl('\\{\\"widget_placement',data$event)==T,]$event),'\\"'),'[',13,FUN.VALUE=character(1)))
      }
      
      ##Process to identify child modules via a lookup table
      #Cleans up module child references, removing puntuation and spaces from the field
      data$mod.child.ref <- str_replace_all(data$mod.child.ref,"[^[:alnum:]]","")
      
      #Final Module Look-up table from course structure
      #Module key is set to character to allow matching without the difficulties of factor variables
      data$module.key <- as.character(data$module.key)
      
      ##Course branch module identifier clean up
      #All module ID for events that reference activity at the 2nd level of the course hiearchy are 
      #processed to identify the appropriate 3rd level branch module and then 4th level content module 
      #that a user navigated too in the course. The process uses a look-up table to identify the 
      #appropriate children of verticle and sequential modules. To the child referenced in the navigation 
      #event or inferred to the first child, when a child reference was unavailable.
      #Create bridge lookup for events from sequential blocks (level 2 of course hierarchy)
      look <- data[grepl("chapter",data$module.key)==T |grepl("sequential",data$module.key)==T | grepl("vertical",data$module.key)==T,c("module.key","mod.child.ref")]
      if(nrow(look)>0){
        #Gives those look-up references child references if they are missing
        look[is.na(look$mod.child.ref) | grepl("[[:digit:]]",as.numeric(look$mod.child.ref))==F,]$mod.child.ref <- 1
        look$parentid <- str_extract(look$module.key,"[:alnum:]{32}")
        look$childref <- paste(look$parentid,look$mod.child.ref,sep="/")
        look[,"replace"]<-NA
        look[,"level"]<-NA
        #Sequential modules Replacements for chapter modules
        if(nrow(look[grepl("chapter",look$module.key)==T,])>0){
          #Looks up each block module ID and finds module ID of first child or known child leaf
          #Known child leaf numbers are taken from the seq_[goto,prev,next] events (1:N), other courseware events given child leaf 1
          temp <- look[grepl("chapter",look$module.key)==T,]
          for(i in 1:nrow(temp)){
            if(length(courseStr[courseStr$modparent_childlevel==temp[i,]$childref,]$id)>0){
              temp[i,]$replace <- as.character(courseStr[courseStr$modparent_childlevel==temp[i,]$childref,]$id)
              temp[i,]$level <- courseStr[courseStr$modparent_childlevel==temp[i,]$childref,]$treelevel
            } 
            else {
              temp[i,]$replace <- NA
              temp[i,]$level <- NA
            }
          }
          #All module ID for events that reference activity at the 3rd level of the course hiearchy are 
          #processed to identify the appropriate 4th level content module that a user navigated too in the course.
          temp$parentid <- str_extract(temp$replace,"[:alnum:]{32}")
          temp$childref <- paste(temp$parentid,1,sep="/")
          levels(temp$childref) <- levels(courseStr$modparent_childlevel) 
          look[grepl("chapter",look$module.key)==T,] <- temp
          rm(temp)
        }
        #Vertical modules replacements for sequential modules
        if(nrow(look[grepl("sequential",look$module.key)==T | grepl("sequential",look$replace)==T,])>0){
          #Looks up each block module ID and finds module ID of first child or known child leaf
          #Known child leaf numbers are taken from the seq_[goto,prev,next] events (1:N), other courseware events given child leaf 1
          temp <- look[grepl("sequential",look$module.key)==T | grepl("sequential",look$replace)==T,]
          for(i in 1:nrow(temp)){
            if(length(courseStr[courseStr$modparent_childlevel==temp[i,]$childref,]$id)>0){
              temp[i,]$replace <- as.character(courseStr[courseStr$modparent_childlevel==temp[i,]$childref,]$id)
              temp[i,]$level <- courseStr[courseStr$modparent_childlevel==temp[i,]$childref,]$treelevel
            } 
            else {
              temp[i,]$replace <- NA
              temp[i,]$level <- NA
            }
          }
          #All module ID for events that reference activity at the 3rd level of the course hiearchy are 
          #processed to identify the appropriate 4th level content module that a user navigated too in the course.
          temp$parentid <- str_extract(temp$replace,"[:alnum:]{32}")
          temp$childref <- paste(temp$parentid,1,sep="/")
          levels(temp$childref) <- levels(courseStr$modparent_childlevel) 
          look[grepl("sequential",look$module.key)==T | grepl("sequential",look$replace)==T,] <- temp
          rm(temp)
        }
        #Content and assessment modules replacements for vertical modules
        if(nrow(look[grepl("vertical",look$module.key)==T | grepl("vertical",look$replace)==T,])>0){
          #Looks up each block module ID and finds module ID of first child or known child leaf
          #Known child leaf numbers are taken from the seq_[goto,prev,next] events (1:N), other courseware events given child leaf 1
          temp <- look[grepl("vertical",look$module.key)==T | grepl("vertical",look$replace)==T,]
          for(i in 1:nrow(temp)){
            if(length(courseStr[courseStr$modparent_childlevel==temp[i,]$childref,]$id)>0){
              temp[i,]$replace <- as.character(courseStr[courseStr$modparent_childlevel==temp[i,]$childref,]$id)
              temp[i,]$level <- courseStr[courseStr$modparent_childlevel==temp[i,]$childref,]$treelevel
            } 
            else {
              temp[i,]$replace <- NA
              temp[i,]$level <- NA
            }
          }
          #All module ID for events that reference activity at the 3rd level of the course hiearchy are 
          #processed to identify the appropriate 4th level content module that a user navigated too in the course.
          temp$parentid <- str_extract(temp$replace,"[:alnum:]{32}")
          temp$childref <- paste(temp$parentid,1,sep="/")
          levels(temp$childref) <- levels(courseStr$modparent_childlevel) 
          look[grepl("vertical",look$module.key)==T | grepl("vertical",look$replace)==T,] <- temp
          rm(temp)
        }
        #Copies over replacement children module IDs for the original sequential block modules ID
        data[grepl("chapter",data$module.key)==T |grepl("sequential",data$module.key)==T | grepl("vertical",data$module.key)==T, ]$module.key  <- look$replace
      }
      rm(look)
      #Resets module.key as a factor and resets it to course structure levels
      #needed for matching edge and node statistics witht the node list
      data$module.key <- as.factor(data$module.key)
      #Removes module.child.ref field
      data <- data[,-ncol(data)]
      
      #Extracts the module type and module unique identifiers from the verbose moduleID
      if(length(data[!is.na(data$module.key),]$module.key)>0){
        data$mod_hex_id <- do.call(rbind,strsplit(as.character(data$module.key),'\\@'))[,3]
        data$module_type <- do.call(rbind,strsplit(as.character(data$module.key ),'\\@'))[,2]
      } else {
        data$mod_hex_id <- courseID
        data$module_type  <- "course"
      }
      
      #Adds module order in course sequence logs and sequence page 
      data <- join(data,courseStr[,c(2,7,13)],by="mod_hex_id")
      
      #Second period calculation measures period between only the maintained events
      #This calculation is used to make a temporal session calculation 
      #Calc made in minutes
      rbind(as.data.frame(as.numeric(difftime(data[2:nrow(data),]$time,data[1:nrow(data)-1,]$time,units="mins"))),NA) -> period
      c("period") -> names(period)
      
      #Updates all records where period is greater than or equal to the time break set by user (or automatically of 60 minutes) 
      #This needs to be updated to change the value from a rounded 60 to another more meaningful value
      #For example, the value equals the mean time period measured for the module or event type.
      period[period >= timeB_val & !is.na(period), ] <- as.numeric(timeB_val)
      data$period <- period$period
      data[nrow(data),]$period <- mean(data$period,na.rm = T)
      
      #Removes events where the order cannot be found in the course structure.
      #The modules may have been removed for a variety of reasons, although students 
      # may have had access to the content during the course run. 
      if(nrow(data[!is.na(data$order),])==0){
        data <- data[,c(1:3,10,22,24,25,23,5,19,6,20,4,7,9,8,11:18)]
        names(data)[c(1:3,7)] <- c("org_id","course_id","user_id","parent_id")
        fileName <- as.data.frame(strsplit(fileList[i], split="\\/"))
        #Writes processed logfile user ID for file saving 
        write.csv(x = data, file = paste0(path,"/studentevents_processed/",subDir[1],"/",uid,".csv"),
                  row.names = F)
        data <- NULL
      } else {
        if(nrow(data[is.na(data$order),])>0){
          data <- data[!is.na(data$order),]
        }
        ##Updates event_type field for non-typed event module visits.
        #converts records where events_types have a module URL to a generic access event "mod_access"
        levels <- levels(data$event_type)
        levels[length(levels)+1] <- "mod_access"
        data$event_type <- factor(data$event_type, level=levels)
        if(nrow(data[grepl("course-v1",as.character(data$event_type))==T, ])>0){
          data[grepl("course-v1",as.character(data$event_type))==T, ]$event_type <- c("mod_access")
        }
        
        ##### Revised period time estimate based on values of events with similar event_type ####
        #Update time estimate for events where gap is greater than or less than 60 minutes
        if(timeBEst==TRUE){
          if(nrow(data[data$period==timeB_val,])>0){
            if(is.null(length(!is.nan(data$period)==T))==F){
              #Sets up event_type variables found in a students log
              event_type_tmp <- unique(as.character(data$event_type))
              #Loops through each event_type present to check if there is an event equal to temporal session (tsess) cut off time (timeB_val).
              #If there are rows that are equal to the tsess cut off, it replaces the period with a more accurate estimate of 
              #the amount of time a student took on the action, to provide a better estimate.
              for(i in 1:length(event_type_tmp)){
                if(nrow(data[data$period==timeB_val & data$event_type==event_type_tmp[i],])>0){
                  median(data[data$period<timeB_val & data$event_type==event_type_tmp[i],]$period) -> median_temp 
                  if(!is.na(median_temp)==T){
                    median_temp -> data[data$period==timeB_val & data$event_type==event_type_tmp[i],]$period
                  } else {
                    median(data[data$period<timeB_val,]$period) -> data[data$period==timeB_val & data$event_type==event_type_tmp[i],]$period
                  }
                }
              }
            }
          }
        }
        
        #Updates module_id field
        data$module_id <- data$module.key
        
        #Reorders columns
        data <- data[,c(1:3,10,22,24,25,23,5,19,6,20,4,7,9,8,11:18)]
        names(data)[c(1:3,7)] <- c("org_id","course_id","user_id","parent_id")
        
        #Writes processed logfile user ID for file saving 
        write.csv(x = data, file = paste0(path,"/studentevents_processed/",uid,".csv"),
                  row.names = F)
      }
    }
  }
  data <- NULL
} 

## Saves out list of user IDs for students with usable logs and unusable logs
courseID <- gsub("\\+","-",courseID)

users <- list.files(path=paste0(path_output,"/studentevents_processed/"),pattern=".csv")
users <- data.frame(do.call('rbind',strsplit(users,"\\.")))
names(users) <- c("userID","v")
write.csv(x=users[,1], file=paste0(path_output,"/userlists/",courseID,"-auth_user-students-active.csv"),row.names = F)

users <- list.files(path=paste0(path_output,"/studentevents_processed/",subDir[1]),pattern=".csv")
users <- data.frame(do.call('rbind',strsplit(users,"\\.")))
names(users) <- c("userID","v")
write.csv(x=users[,1], file=paste0(path_output,"/userlists/",courseID,"-auth_user-students-inactive.csv"),row.names = F)

#### Finishing details ####
#Indicate completion
message("\n**** Complete! ****\n")

## Script processing time feedback
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in minutes):\n")
print((proc.time()[3] - start[3])/60)

## Clear environment variables
#rm(list=ls())
rm(data,courseStr,start,path,subDir,courseID,fileList,users,timeB_val,timeBEst,wl_min,oas,
   pse,vid,trans,nc,drag,numLogs,uid,median_temp,k,i,fs,event_type_tmp,levels,fileName,period)
