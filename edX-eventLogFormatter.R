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
# Description:  This script extracts all of a user's activity from the edX event log files in a user 
#               selected folder.  It outputs the resulting data (all events tied to a single user's ID) 
#               as a standard a CSV file. 
#               (*NOTE: the edX provided logs are in NDJSON format, not the typical JSON format.)
#  
# File input stack: 
#            1) A folder contining one or more "*.log.gz" event log file(s)    (source: edX)
# 
# Package dependencies: jsonlite, ndjson, tcltk
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
#
## ===================================================== ##

######### Setup ########## 
## _Clean the environment ####
rm(list=ls()) 

## _start timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute elapsed time of script)

## _Load required packages #####
require("ndjson")     #needed to read the non-standard JSON log files (NDJSON format)
require("jsonlite")   #for working with JSON files (esp. read and write)
require("tcltk2")     #for OS independant GUI file and folder selection

####Functions 
#logCapture 
##The logCapture function is a modification of code provided by Purdue University team
##to allow mass extracting individual set of student logs based on known set of student IDs for an 
##edX course. The function creates a unique log file for each student ID in the list, 
##saved as either a JSON or CSV formatted file. The function currently set up to save as CSV,
##alternatively can be set for user defined action such as format=T csv if format=F, JSON set up possible.

logCapture <- function(curUserIDS,eventLog,fileList,path){      
  numStudents <- length(curUserIDS)
  numLogFiles <- length(fileList) 
  for(j in 1:numStudents){
    curUser <- curUserIDS[j]
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
    ###Not used columns that could be useful for other courses
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
#Test set of individual userIDs for students in an edX Course whose event data should be extracted
#Test code for function
d <- data.frame(matrix(ncol = 1, nrow = 6))
names(d) <- c("ids")
d$ids <-  c("229670106","798100575","230119567","211115035","77747279","799212761")
curUserIDS <- d$ids
rm(d)

eventLog <- NULL

#Creates paths used to locate directory for research data sets and save processing outputs
path_data <- c("Z:/research/17-Boeing/data/edx/MITProfessionalX_SysEngxB1_3T2016/events")
#path_data = tclvalue(tkchooseDirectory())
path_output <- c("Z:/research/17-Boeing/data/edx/MITProfessionalX_SysEngxB1_3T2016/studentevents/")
#path_output = tclvalue(tkchooseDirectory())

## _Build list of all event files for course####
#Store all the filenames of JSON formatted edX event logs within a user selected directory 
# (files ending with ".log.gz").
fileList <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = path_data,
                       pattern = ".log.gz$")

#Log Capture function for list of users
logCapture(curUserIDS,eventLog,fileList,path=path_output)

######### Finishing Details ########## 
#Indicate completion
message("\n**** Complete! ****\n")

## _Script processing time feedback #####
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print(proc.time() - start)

## _Clear environment variables
rm(list=ls())   