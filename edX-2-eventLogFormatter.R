## ====================================================================================================== ##
# Title:        Extracts edX events logs for idenfitied multiple, individual student users 
# Project:      edX learner trajectory analysis
# 
#     Copyright 2017 Michael Ginda & Krishna Madhavan
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
# Authors:      Michael Ginda^, Krishna Madhavan*, & Taylor Williams*
# Affiliation:  ^Indiana University, *Purdue University
# 
# Description:  A daily event log for edX course participants (students) are 
#               created by this script using an edX course's daily combined event logs and a 
#               list of users enrolled in the course, that are provided in a edX Data Package.
#
#               The script takes a list of course participants and identifies students within a 
#               course and saves this list of users.  Using the student's edX identifier, the script
#               loops through the daily event logs to identify all logged activity (e.g server, 
#               browser, and mobile device event) made by the user. The resulting individual 
#               participants logs are saved as a CSV file.
#               (*NOTE: the edX provided logs are in NDJSON format, not the typical JSON format.)
#  
# File input stack: 
#            1) A folder containing one or more "*.log.gz" event log file(s)    
#            2) A data frame edX course authenticated list of students:
#               {org}-{course Identifier}-{term}-auth_user-students.csv
# 
# Package dependencies: jsonlite, ndjson, plry, tcltk
#
# Changelog:
#   2017.08.11. Initial Code
#   2017.08.13. Added user output and user save as for output file
#   2017.08.15. Changed to extracting single user's activity
#   2017.08.28. Fixed require statement bug (moved required packages in individual require statements)
#   2017.08.29. Update to comments, file saving updates, added save to RData file (added to JSON and CSV) 
#   2017.09.29. Updating file for sharing
#   2017.10.19. Updated file create function to produce event trajectory logs from a list of student IDs
#   2017.10.31. Updated function to maintain only relevant fields
#   2018.02.05. Cleaned-Up script to list all fields perserved in final logs CSV files
#   2018.04.04. Description, title, and file input stack corrections; 
#               updated function parameter definitions; creates lists of users for extraction for course
#   2018.04.05. Forked the user list creation to a new script, simplifying package dependencies, file stack
#               and data input sources. Path_output now are used for loading results of prior scripts.
#
## ====================================================================================================== ##

######### Setup ########## 
## _Clean the environment ####
rm(list=ls()) 

## _start timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute elapsed time of script)

## _Load required packages #####
require("jsonlite")   #for working with JSON files (esp. read and write)
require("ndjson")     #needed to read the non-standard JSON log files (NDJSON format)
require("tcltk2")     #for OS independent GUI file and folder selection
require("plyr")       #for joining user tables together
require("stringr")    #for string manipulation
####Functions 
#logCapture 
# @param curUserIDS is the file location of course structure data
# @param eventLog==NULL sets a dummy eventLog dataframe
# @param filelist a list of daily event logs from a course's edX Data Package
# @param path indicates the path used to save outputs files
##The logCapture function is a modification of code provided by Purdue University team
##to allow mass extracting individual student's event logs from course event logs, based on known set 
##of student IDs for an edX course. The function creates a unique log file for each student ID in the list, 
##saved as either a JSON or CSV formatted file. The function currently set up to save as CSV,
##alternatively can be set for user defined action such as format=T csv if format=F, JSON set up possible.

logCapture <- function(curUserIDS,eventLog,fileList,path){      
  numStudents <- length(curUserIDS)
  numLogFiles <- length(fileList) 
  for(j in 1:numStudents){
    curUser <- curUserIDS[j]
    message("Processing student ", j, " of ", numStudents)
    for(i in 1:numLogFiles){
      curFileName <- fileList[i] 
      #print update message to console
      message("Processing log file ", i, " of ", numLogFiles)
      print(proc.time() - start)
      #read log data (NOTE: logs are in NDJSON format, not typical JSON format)
      ndData <- ndjson::stream_in(curFileName)
      #extract events for a single user, add to the complete eventLog for that user
      eventLog <- rbind.data.frame(eventLog, subset(ndData,ndData$context.user_id==curUser), fill=TRUE)
      id <- curUser
    }
    #Identifies all columns that are maintained for the rest of the workflow
    eventLog<-subset(eventLog,select=c("accept_language","agent","augmented.country_code",
                                       "context.course_id","context.org_id","context.user_id","event",
                                       "event_source","event_type","time","username","name","session",
                                       "context.module.display_name","context.module.usage_key",
                                       "event.problem_id","event.attempts","event.grade","event.max_grade",
                                       "event.state.seed","event.success","event.answer.file_key",
                                       "event.attempt_number","event.created_at","event.submission_uuid",
                                       "event.submitted_at","event.feedback","event.feedback_text",
                                       "event.rubric.content_hash","event.score_type","event.scored_at",
                                       "event.scorer_id"))
    ###Unused columns that could be useful for other courses
    #event.correctness	event.hint_label	event.hints.0.text	
    #event.module_id	event.problem_part_id	event.question_type	
    #event.trigger_type	event.enrollment_mode	event.social_network	
    #event.answers	event.submission
    
    #Write student log file
    write.csv(x = eventLog, file = paste0(path,"/",id,".csv"),
              row.names = F)
    #Clears the event log df for next student
    eventLog <- NULL
  }
}

######### Main ########## 
#Assigns path where R may read in events logs from the edX data package 
path_data = tclvalue(tkchooseDirectory())

#Assigns path where R saves processing outputs for user logs
path_output = paste0(tclvalue(tkchooseDirectory()),"/")

#Identifies the output of the student user list for an edX course. 
#The pattern parameter identifies the outputs of the edX-1-studentUserList.R script
fileList <- list.files(full.names = F, recursive = FALSE, 
                       path = paste0(path_output,"userlists"),
                       pattern = "auth_user-students.csv")

#Alternative userlists
users <- read.csv(paste0(path_output,"/userlists/students-missingLogs-20180404.csv"),header=T)

#Sets ID list for processing
curUserIDS <- users$id #this converts dataframe of user ids to integer list needed for the function
#Removes user list related objects
rm(userList,users,userProf,userFileName)

#dummy eventLog object used in logCapture function
eventLog <- NULL

## _Build list of all event files for course####
#Store all the filenames of JSON formatted edX event logs within a user selected directory 
# (files ending with ".log.gz").
fileList <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = path_data,
                       pattern = ".log.gz$")

#Log Capture function for list of users
logCapture(curUserIDS,eventLog,fileList,path=paste0(path_output,"studentevents/"))

######### Finishing Details ########## 
#Indicate completion
message("\n**** Complete! ****\n")

## _Script processing time feedback #####
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print(proc.time() - start)

## _Clear environment variables
rm(list=ls())   
