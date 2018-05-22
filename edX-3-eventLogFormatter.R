## ====================================================================================== ##
# Title:        Processing and formatting student's edX events logs for analysis          
# Project:      edX learner trajectory analysis
# 
#     Copyright 2017-2018 Michael Ginda
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
#               - extracted by script, "edX-0-courseStructureMeta.R";
#            2) A list of student userIDs from an edX course:
#               - {org}-{course}-{term}-auth_user-students.csv;
#               - extracted by script, "edX-1-studentUserList.R";
#            3) A "studentevents" directory containing one or more student
#               CSV event log file(s):
#               - {userID}.csv;
#               - extracted by script, "edX-2-eventLogExtractor.R"
#
# Output files:                        
#            1) A set of processed data tables of event action logs for each student:
#               - {userID}.csv;
#               - Used in scripts:
#                 * "edX-4-studentTrajectoryNet.R"
#                 *	"edX-6-studentFeatureExtraction.R"			
#            2) A list capturing student userIDs who are active in the course,
#				        and have usable actions:
#               - {org}-{course}-{term}-auth_user-students-active.csv;
#               - Used in scripts:
#                 * "edX-4-studentTrajectoryNet.R";
#                 *	"edX-6-studentFeatureExtraction.R;
#            3) A list capturing student userIDs who are active in the course,
#				        but have no usable actions:
#               - {org}-{course}-{term}-auth_user-students-unusableActivity.csv;
#            4) A list capturing student userIDs who are inactive in the course,
#				        and have no actions in their event logs:
#               - {org}-{course}-{term}-auth_user-students-inactive.csv; 
#
# Package dependencies: magrittr, stringr, plyr, tcltk
#
# Change log:
#   2017.10.23. Initial Code
#   2017.10.26. Initial version of function
#   2017.10.30. Updated function to fix session backfill error; remove events that 
#               are not useful for analysis
#   2017.10.31. Final version of function with new session backfill fix, and test 
#                subject data processed
#   2017.11.02  Clean-up version for sharing on repository
#   2017.11.03  Correct error for adding module IDs for course information,  
#               student progress, and wiki page events
#   2017.11.14  Clean up script introduction text
#   2017.11.22  Updated function to better identify edx enrollment and open assessment 
#               uploads events
#   2017.12.15  Updated function with nc parameter that determines if non-content 
#               modules are kept or removed from the network analysis and visualizations
#   2018.01.05  Updated script to calculated module identifiers in processing script;
#               added parameters VID to remove specified video (non-user engage) and 
#               PSE to remove redundant problem events (redundant); 
#               fixed if statements that caused errors; add module type to events. 
#   2018.01.12  Updated script with parameter courseStr to add use course structure 
#               to convert events from higher levels of course structure tree to their
#               relevant children content modules. Adds "module_type" & "module_order" 
#               Added tsess session identifiers based on the calculate periods of events.
#               Added 'modAccess' event_type for access in log that lack specific events 
#               type categorization. Begin addition of control features to save out
#               student logs in cases where they logged zero events, or logged events
#               that are not apart of model.
#   2018.01.16  Updated script to add new directory for saved results of student 
#               event log use cases. Added parameters subZ and subN to function
#               to save special cases (zero events, and no usable events). Creates
#               list of user identifiers associated with the three special cases.
#   2018.01.18  Updated logFormatter function to remove events where no "order" is 
#               found for an module referenced in an event, which indicates that 
#               the module was not found in the final course module structure. 
#   2018.02.02  Cleaned-up script for sharing and added function to manually select user
#               ID list.
#   2018.02.08  Aligned logFormatter function with outputs of the 
#               "edX-courseStructureMeta.R" script; added parameter to allow user
#               to set manual time length for tsess field.
#   2018.04.05  Update paths used to collect data, removed the getCSVdata function,
#               updated to read and save files in correct output directories; 
#               updated description text.
#   2018.04.09  Update filenames for saved lists of student 
#               activity use cases (e.g from noEvents to inactive, from unusableEvents to 
#               unusableActivity, events and active).
#   2018.04.30  Update time calculations for session events with times of 60 minutes 
#               or greater.
#   2018.05.21  Script format alignment.
#
## ====================================================================================== ##
######### Setup ########## 
## _Clean the environment ####
rm(list=ls()) 
#rm(courseStr,courseID,curUserIDS,eventLog,fileList,start,studentLogs,subDir,logExtractor,i)

## _Load required packages #####
require("magrittr")   #Pipe tool
require("stringr")    #string parsing
require("plyr")       #DPLYR
require("tcltk2")     #for OS independent GUI file and folder selection

####Functions
#logFormatter 
##The logFormatter function is a modification of code provided
##to allow mass extracting individual set of student logs based on known set of student IDs for an 
##edX course. The function creates a unique log file for each student ID in the list, 
##saved as either a JSON or CSV formatted file. The function currently set up to save as CSV,
##alternatively can be set for user defined action such as format=T csv if format=F, JSON set up possible.
#current event types handled by script:
#Course wiki, progress, and information pages
#Course Navigation events (seq_prev, seq_next, seq_goto)
#Problem events (problem_check, problem_show, save_problem_success, showanswer) 
#Openassessmentblock events (openassessmentblock.self_assess, openassessmentblock.save_submission,
#                            openassessmentblock.create_submission, openassessmentblock.get_peer_submission
#                            openassessmentblock.peer_assess, openassessmentblock.submit_feedback_on_assessments
#                            )
#Video events (load_video, play_video, pause_video, stop_video, seek_video, hide_transcript, show_transcript, speed_change_video)
#logFormatter v1.0 Updated for this project
# Allow to both export and convert an igraph object to JSON.
#
# @param filelist Filelist contains a list of filenames of individual user's EdX logs.
# @param courseStr is a processed version of a course's module structure (tree)
# @param path is the sets the path for where output files will be saved.
# Qparam timeB is a numeric value setting the number of minutes that must pass for a  
#        temporal session (tsess) to have ended; automatically set at 60 minute threshold.
# @param subZ is the directory name set by a user for where logs with zero events are saved.
# @param subN is the directory name set by a user for where logs with no usable events are saved.
# @param NC indicates whether non-content module pages in an EdX course (e.g Course Information, 
#        wikis, student progress) are maintained in the analysis.
# @param PSE pse indicates whether server events associated with problem modules, specifically,
#        showanswer and save_problem_success events, are maintained in analysis.
# @param VID vid indicates video events associated with infrequent use (transcript and closed 
#        caption evnts), and video load events, which do not show meaningful use of content module.
# @param timeBEst timeBEst is a logical parameter that indicates if processing script
#        should provide updated time period estimates for events where the period is equal to or greater 
#        than the timeB (temporal session break) parameter; events with large periods are updated with
#        the median period value that is calculcated based on events with the same type found in a
#        given student's log. 

logFormatter <- logFormatter <- function(fileList,courseStr,path,timeB=60,subZ,subN,nc,pse,vid,timeBEst=TRUE){
  numLogs <- length(fileList)
  for(i in 1:numLogs){
    message("Processing log file ", i, " of ", numLogs)
    print(proc.time() - start)
    #Load data set
    data <- read.csv(fileList[i])
    
    if(nrow(data)==0){
      data <- as.data.frame(matrix(data=NA,nrow=1,ncol=14))
      names(data) <- c("user_id","mod_hex_id","order","mod_parent_id","module_type","event_type",
                       "time","period","session","tsess","event.attempts","event.grade",
                       "event.max_grade","event.success")
      fileName <- as.data.frame(strsplit(fileList[i], split="\\/"))
      write.csv(x=data, file=paste0(path,"/studentevents_processed/",subZ,"/",fileName[nrow(fileName),]), row.names = F)
      data <- NULL
      paste0(path_output,"/studentEvents_processed",subZ)
    } else {
      
      #Creates a course ID by removing "course-v1:" text, which is not used in the module ID.
      courseID <- strsplit(as.character(data$context.course_id[1]),'\\:')[[1]][2]
      uid <- data$context.user_id[1]
      
      #removes fields unused in processing, modeling, analysis, or visualization
      data <- data[,-c(1:3,11,14,20,22:32)]
      
      ##Data format updates and column creation/organization
      #Updates Time field to R compliant format
      sapply(strsplit(as.character(data$time),split='+',fixed=T),function(x)(x[1])) %>% 
        as.POSIXlt(tz="EST",format='%Y-%m-%dT%H:%M:%S') -> data$time 
      #Reorders logs by the time field, and reset rownames
      data <- data[order(data$time),]
      rownames(data) <- 1:nrow(data)
      
      #Calculate measures period of time between events (time taken for action and module)
      #Calc made in minutes
      rbind(as.data.frame(as.numeric(difftime(data[2:nrow(data),]$time,data[1:nrow(data)-1,]$time,units="mins"))),NA) -> period
      c("period") -> names(period)
      #Updates all records where period is greater than or equal to the time break set by user (or automatically of 60 minutes) 
      #This needs to be updated to change the value from a rounded 60 to another more meaningful value
      #For example, the value equals the mean time period measured for the module or event type.
      period[period >= timeB & !is.na(period), ] <- as.numeric(timeB)
      period[nrow(period), ]<- mean(period$period,na.rm=T)
      data <- cbind(data,period)
      rm(period)

      #Creates temporal session based on the outliers in the period field.
      data$tsess <- NA
      #Searches for outliers periods to demarcate the end of a user session, 
      #id set by seq from 1 to max periods identified; if no max values are found,
      #it is assumed there was only one session.
      if(nrow(data[data$period >= timeB,])>1){
        data[data$period >= timeB,]$tsess <- seq(from=1,to=nrow(data[data$period >= timeB,]))
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
      
      ##Remove events without associated learning object modules
      ##These come before all actions actions, because they are known missing data cases;
      ##and server events lack Session IDs, which cause problem if they appear at the 
      ##end of asorted data file when you back fill session IDs to events.
      #Identifies events that do not relate to a learning object these include: course information
      #student progress, wiki pages, course about, and visits to a discussion forum
      if(nrow(data[grepl('\\{\\}', data$event)==T & grepl('courseware',data$event_type)==F,])>0){
        data[grepl('\\{\\}', data$event)==T & grepl('courseware',data$event_type)==F,]$kp <- 0
      }
      
      #nc parameter if statement checks to determines if a user wants to keep the non-content modules: course information, 
      #wiki, and student progress module pages in the analysis or if they need to be removed
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
        #Identifies events that do relate to a course wiki pages and updates module.key field
        if(length(data[grepl('\\{\\}', data$event)==T & grepl('wiki',data$event_type)==T,]$event_type) > 0 ){
          data[grepl('\\{\\}', data$event)==T & grepl('wiki',data$event_type)==T,]$module.key <- 
            paste0("block-v1:",courseID,"+type@wiki")
        }
      } else if (nc==FALSE){ 
        if(length(data[grepl('\\{\\}', data$event)==T & grepl('info',data$event_type)==T,]$event_type) > 0 ){
          data[grepl('\\{\\}', data$event)==T & grepl('info',data$event_type)==T,]$kp <- 0
        }
        #Identifies events that do not relate to a students progress
        if(length(data[grepl('\\{\\}', data$event)==T & grepl('progress',data$event_type)==T,]$event_type) > 0 ){
          data[grepl('\\{\\}', data$event)==T & grepl('progress',data$event_type)==T,]$kp <- 0
        }
        #Identifies events that do not relate to a course wiki pages
        if(length(data[grepl('\\{\\}', data$event)==T & grepl('wiki',data$event_type)==T,]$event_type) > 0 ){
          data[grepl('\\{\\}', data$event)==T & grepl('wiki',data$event_type)==T,]$kp <- 0
        }
      } else stop("invalid 'nc' specification to maintain non-content modules in analysis")
      
      #Identifies Edx Enrollment Events that are not course specific 
      #Applied to only some students, as some student have taken prior edx course
      if(nrow(data[grepl('edx\\.course\\.enrollment\\.',data$event_type),])>0){
        data[grepl('edx\\.course\\.enrollment\\.',data$event_type)==T,]$kp <- 0
      }
      
      #Removes Page_close events
      if(nrow(data[grepl('page\\_close', data$event_type),])>0){
        data[grepl('page\\_close', data$event_type)==T,]$kp <- 0 
      }
      
      #Applied if student has uploaded a file to the open assessment events
      if(nrow(data[grepl('openassessment\\.upload',data$event_type)==T,]) > 0){
        data[grepl('openassessment\\.upload',data$event_type)==T,]$kp <- 0
      }
      
      #Removes specified problem server events from event logs
      if(pse==FALSE){
        if(nrow(data[grepl('save\\_problem\\_success', data$event_type)==T,])>0){
          data[grepl('save\\_problem\\_success', data$event_type)==T,]$kp <- 0 
        }
        if(nrow(data[grepl('showanswer', data$event_type)==T,])>0){
          data[grepl('showanswer', data$event_type)==T,]$kp <- 0 
        }
      }
      
      #Removes video events with low insight value or use in logs
      if(vid==FALSE){
        #removes transcript events
        if(nrow(data[grepl('\\w\\_transcript',data$event_type)==T,])>0){
          data[grepl('\\w\\_transcript',data$event_type)==T,]$kp <- 0
        }
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
      k <- sum(data[data$kp==1,]$kp)
      
      #Checks to see if there are any rows kept after removing specified events. If no event is to be kept after
      #the field checks, the file is saved to a special directory for these cases. Else processing will continue 
      #to 
      if(k == 0){
        write.csv(x = data, file = paste0(path,"/studentevents_processed/",subN,"/",uid,".csv"),
                  row.names = F)
      } else {
        ##Extracts or recreate module ID from the log fields for various known modules cases (if not removed by user)
        #Creates column to place learning object moduleID idenfitied for each event
        #removes data with a value of 0
        data <- data[data$kp!=0,]
        #Removes the KP field from the data set
        data <- data[-c(length(data))]
        data[c("module.key")] <- NA
        #Creates column to identify the appropriate current child of higher-level modules (i.e. chapters and page sequences)
        data[c("mod.child.ref")] <- NA
        
        #Problem events (e.g. problem_check, save_problem_success, showanswer) and Openassessmentblock events (all but .upload_file)
        data$module.key <- paste0(data$context.module.usage_key)
        
        #Problem_show events
        if(nrow(data[grepl('problem_show',data$event_type)==T,])>0){
          data[grepl('problem\\_show',data$event_type)==T,]$module.key <- 
            paste0(vapply(strsplit(as.character(data[grepl('problem\\_show',data$event_type)==T,]$event), '\\"'),'[',4,FUN.VALUE=character(1)))
        }
        #Videos Events (e.g. seek, load, play, stop, speed)
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
        #Course Branch Blocks (Level 3 of course structure)
        if(length(data[grepl('/courseware',data$event_type)==T,])>0){
          #Extracts module ID number and creates ids for courseware events (high level modules)
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
        look <- data[grepl("sequential",data$module.key)==T,18:19]
        look$parentid <- str_extract(look$module.key,"[:alnum:]{32}")
        look$childref <- paste(look$parentid,look$mod.child.ref,sep="/")
        look[,"replace"]<-NA
        look[,"level"]<-NA
        
        #Looks up each sequential block module ID and finds module ID of first child or known child leaf
        #Known child leaf numbers are taken from the seq_[goto,prev,next] events (1:N), other courseware events given child leaf 1
        for(i in 1:nrow(look)){
          if(length(courseStr[courseStr$modparent_childlevel==look[i,]$childref,]$id)>0){
            look[i,]$replace <- as.character(courseStr[courseStr$modparent_childlevel==look[i,]$childref,]$id)
            look[i,]$level <- courseStr[courseStr$modparent_childlevel==look[i,]$childref,]$treelevel
          } 
          else {
            look[i,]$replace <- NA
            look[i,]$level <- NA
          }
        }
        
        #All module ID for events that reference activity at the 3rd level of the course hiearchy are 
        #processed to identify the appropriate 4th level content module that a user navigated too in the course.
        look$parentid <- str_extract(look$replace,"[:alnum:]{32}")
        look$childref <- paste(look$parentid,1,sep="/")
        levels(look$childref) <- levels(courseStr$modparent_childlevel) 
        
        #Performs the final module ID look-up
        for(i in 1:nrow(look)){
          if(length(courseStr[courseStr$modparent_childlevel==look[i,]$childref,]$id)>0){
            look[i,]$replace <- as.character(courseStr[courseStr$modparent_childlevel==look[i,]$childref,]$id) 
            look[i,]$level <- courseStr[courseStr$modparent_childlevel==look[i,]$childref,]$treelevel
          } else {
            look[i,]$replace <- NA
            look[i,]$level <- NA
          }
        }
        #Copies over replacement children module IDs for the original sequential block modules ID
        data[grepl("sequential",data$module.key)==T, ]$module.key  <- look$replace
        rm(look)
        #Resets module.key as a factor and resets it to course structure levels
        #needed for matching edge and node statistics witht the node list
        data$module.key <- as.factor(data$module.key)
        #removes module.child.ref field
        data <- data[,-ncol(data)]
        #Extracts the module type and module unique identifiers from the verbose moduleID
        data$mod_hex_id <- do.call(rbind,strsplit(as.character(data$module.key ),'\\@'))[,3]
        data$module_type <- do.call(rbind,strsplit(as.character(data$module.key ),'\\@'))[,2]    
        #Adds module order in course sequence logs and sequence page 
        data <- join(data,courseStr[,c(2,7,13)],by="mod_hex_id")
        
        #Removes events where the order cannot be found in the course structure.
        #The modules may have been removed for a variety of reasons, although students 
        # may have had access to the content during the course run. 
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
        
        #Update time estimate for events where gap is greater than or less than 60 minutes
        if(timeBEst==TRUE){
          if(nrow(data[data$period==timeB,])>0){
            if(nrow(data[data$period==timeB & data$event_type==c('page_close'),])>0){
              median(data[data$period<timeB & data$event_type==c('page_close'),]$period) -> data[data$period==timeB & data$event_type==c('page_close'),]$period
            }
            if(nrow(data[data$period==timeB & data$event_type==c('mod_access'),])>0){
              median(data[data$period<timeB & data$event_type==c('mod_access'),]$period) -> data[data$period==timeB & data$event_type==c('mod_access'),]$period
            }
              #Video event outliar time estimates
              if(nrow(data[data$period==timeB & data$event_type==c('pause_video'),])>0){
                median(data[data$period<timeB & data$event_type==c('pause_video'),]$period) -> data[data$period==timeB & data$event_type==c('pause_video'),]$period
              }
            if(nrow(data[data$period==timeB & data$event_type==c('play_video'),])>0){
              median(data[data$period<timeB & data$event_type==c('play_video'),]$period) -> data[data$period==timeB & data$event_type==c('play_video'),]$period
            }
            if(nrow(data[data$period==timeB & data$event_type==c('seek_video'),])>0){
              median(data[data$period<timeB & data$event_type==c('seek_video'),]$period) -> data[data$period==timeB & data$event_type==c('seek_video'),]$period
            }
            if(nrow(data[data$period==timeB & data$event_type==c('stop_video'),])>0){
              median(data[data$period<timeB & data$event_type==c('stop_video'),]$period) -> data[data$period==timeB & data$event_type==c('stop_video'),]$period
            }
            #Problem events outliar time estimates
            if(nrow(data[data$period==timeB & data$event_type==c('problem_check'),])>0){
              median(data[data$period<timeB & data$event_type==c('problem_check'),]$period) -> data[data$period==timeB & data$event_type==c('problem_check'),]$period
            }
            if(nrow(data[data$period==timeB & data$event_type==c('problem_show'),])>0){
              median(data[data$period<timeB & data$event_type==c('problem_show'),]$period) -> data[data$period==timeB & data$event_type==c('problem_show'),]$period
            }
            #Open Assessment event outliar time estimages
            if(nrow(data[data$period==timeB & data$event_type==c('openassessmentblock.create_submission'),])>0){
              median(data[data$period<timeB & data$event_type==c('openassessmentblock.create_submission'),]$period) -> data[data$period==timeB & data$event_type==c('openassessmentblock.create_submission'),]$period
            }
            if(nrow(data[data$period==timeB & data$event_type==c('openassessmentblock.get_peer_submission'),])>0){
              median(data[data$period<timeB & data$event_type==c('openassessmentblock.get_peer_submission'),]$period) -> data[data$period==timeB & data$event_type==c('openassessmentblock.get_peer_submission'),]$period
            }
            if(nrow(data[data$period==timeB & data$event_type==c('openassessmentblock.save_submission'),])>0){
              median(data[data$period<timeB & data$event_type==c('openassessmentblock.save_submission'),]$period) -> data[data$period==timeB & data$event_type==c('openassessmentblock.save_submission'),]$period
            }
            if(nrow(data[data$period==timeB & data$event_type==c('openassessmentblock.peer_assess'),])>0){
              median(data[data$period<timeB & data$event_type==c('openassessmentblock.peer_assess'),]$period) -> data[data$period==timeB & data$event_type==c('openassessmentblock.peer_assess'),]$period
            }
            if(nrow(data[data$period==timeB & data$event_type==c('openassessmentblock.self_assess'),])>0){
              median(data[data$period<timeB & data$event_type==c('openassessmentblock.self_assess'),]$period) -> data[data$period==timeB & data$event_type==c('openassessmentblock.self_assess'),]$period
            }
            if(nrow(data[data$period==timeB & data$event_type==c('openassessmentblock.submit_feedback_on_assessments'),])>0){
              median(data[data$period<timeB & data$event_type==c('openassessmentblock.submit_feedback_on_assessments'),]$period) -> data[data$period==timeB & data$event_type==c('openassessmentblock.submit_feedback_on_assessments'),]$period
            }
            #Navigation Events
            if(nrow(data[data$period==timeB & data$event_type==c('seq_next'),])>0){
              median(data[data$period<timeB & data$event_type==c('seq_next'),]$period) -> data[data$period==timeB & data$event_type==c('seq_next'),]$period
            }
            if(nrow(data[data$period==timeB & data$event_type==c('seq_goto'),])>0){
              median(data[data$period<timeB & data$event_type==c('seq_goto'),]$period) -> data[data$period==timeB & data$event_type==c('seq_goto'),]$period
            }
            if(nrow(data[data$period==timeB & data$event_type==c('seq_prev'),])>0){
              median(data[data$period<timeB & data$event_type==c('seq_prev'),]$period) -> data[data$period==timeB & data$event_type==c('seq_prev'),]$period
            }
          }
        }
        
        #Maintain only fields that are needed for analysis
        data <- data[,c(3,19,21,22,20,6,7,16,9,17,12:15)]
        names(data) <- c("user_id","mod_hex_id","order","mod_parent_id","module_type","event_type",
                         "time","period","session","tsess","event.attempts","event.grade",
                         "event.max_grade","event.success")
        
        #Writes processed logfile user ID for file saving 
        write.csv(x = data, file = paste0(path,"/studentevents_processed/",uid,".csv"),
                  row.names = F)
      }
    }
    k = NULL
    data <- NULL
  } 
}

######### Main ########## 
## _start timer to track how long the script takes to execute
start <- proc.time() #save the time (to compute elapsed time of script)

#Generic Set up
#Assigns a path to open prior script outputs and to save new processing output files.
path_output = tclvalue(tkchooseDirectory())

##Create data processing output sub-directories for student without events
#students with zero events on load and those with zero events after post-data processing 
subDir = c("zeroEvents", "noEventsProc")
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
fileList <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = paste0(path_output,"/userlists/"),
                       pattern = "auth_user-students.csv$")
fileList <- read.csv(file=fileList,header=T)[1]
fileList <- paste0(path_output,"/studentevents/",fileList$id,".csv")

#Load course structure data module-lookup
#CSV of course modules data extracted from the Course Structure, found in the
#edX student user database file: {org}-{course}-{run}-module-lookup.csv
courseStr <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = paste0(path_output,"/course/"),
                       pattern = "module-lookup.csv")
courseStr <- read.csv(file=courseStr, header=T)
#Extracts course identifier
courseID <- as.character(courseStr $courseID[1])
courseID <- gsub("\\+","-",courseID)

##Log Capture function for list of users, set up to 
logFormatter(fileList=fileList, time=60, courseStr=courseStr, 
             path=path_output, subZ=subDir[1], subN=subDir[2], 
             nc=FALSE, pse=FALSE, vid=FALSE, timeBEst = TRUE)

##Saves out list of user IDs for students with usable logs and unusable logs
users <- list.files(path=paste0(path_output,"/studentevents_processed/"),pattern=".csv")
users <- data.frame(do.call('rbind',strsplit(users,"\\.")))
names(users) <- c("userID","v")
write.csv(x=users[,1], file=paste0(path_output,"/userlists/",courseID,"-auth_user-students-active.csv"),row.names = F)

users <- list.files(path=paste0(path_output,"/studentevents_processed/",subDir[1]),pattern=".csv")
users <- data.frame(do.call('rbind',strsplit(users,"\\.")))
names(users) <- c("userID","v")
write.csv(x=users[,1], file=paste0(path_output,"/userlists/",courseID,"-auth_user-students-inactive.csv"),row.names = F)

users<- list.files(path=paste0(path_output,"/studentevents_processed/",subDir[2]),pattern=".csv")
users <- data.frame(do.call('rbind',strsplit(users,"\\.")))
names(users) <- c("userID","v")
write.csv(x=users[,1], file=paste0(path_output,"/userlists/",courseID,"-auth_user-students-unusableActivity.csv"),row.names = F)

######### Finishing Details ########## 
#Indicate completion
message("\n**** Complete! ****\n")

## _Script processing time feedback #####
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print(proc.time() - start)

## _Clear environment variables
rm(list=ls())
