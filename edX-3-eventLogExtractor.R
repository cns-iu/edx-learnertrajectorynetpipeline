## ====================================================================================== ##
# Title:        Extracts edX events logs for identified multiple, individual 
#               student users 
# Project:      edX learner trajectory analysis
# 
#     Copyright 2017-2019 Michael Ginda & Krishna Madhavan
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
# Description:  A daily event log for edX course participants (students) are created 
#               by this script using an edX course's daily combined event logs 
#               (provided in a edX Data Package) and a list of users enrolled in 
#               the course extracted from the edX course database.
#
#               The script takes a list of course participants and identifies students 
#               within a course and saves this list of users.  Using the student's 
#               edX identifier, the script loops through the daily event logs to 
#               identify all logged activity (e.g server, browser, and mobile device 
#               event) made by the user. (*NOTE: the edX provided logs are in NDJSON 
#               format, not the typical JSON format.) 
#  
# File input stack: 
#            1) An edX course "events" directory for an edX course containing one or 
#               more "*.log.gz" event log file(s) for an edX course;
#            2) A data table of student userIDs from an edX course:
#               - {org}-{course Identifier}-{term}-auth_user-students.csv;
#               - extracted by script, edX-1-studentUserList.R
#
# Output files:                        
#            1) An set of extracted data tables of event action logs for each student 
#               in an edX course:
#               - {userID}.csv;
#               - Used in scripts:
#                 * edX-3-eventLogFormatter.R
#
# Package dependencies: jsonlite, ndjson, dplyr, tcltk
#
# Change log:
#   2019.05.16  Bug fixed for log extractor function updates from 2019.05.15.
#   2019.05.30  Updated script to keep paths to data if user set 
#               them with a previous pipeline script pipeline.
#
## ====================================================================================== ##
#### Environment Setup #####
# Load required packages 
require("jsonlite")   #for working with JSON files (esp. read and write)
require("ndjson")     #needed to read the non-standard JSON log files (NDJSON format)
require("tcltk2")     #for OS independent GUI file and folder selection
require("dplyr")      #for selecting columns and joining user tables together

#### Functions ####
#logExtractor 
# @param curUserIDS is the file location of course structure data
# @param eventLog==NULL sets a dummy eventLog dataframe
# @param filelist a list of daily event logs from a course's edX Data Package
# @param varNames a list of variables that will be extracted from logs for a user
# @param path_output indicates the path used to save outputs files
## The logExtractor function is a modification of code provided by Purdue University team
## to allow mass extracting individual student's event logs from course event logs, based on known set 
## of student IDs for an edX course. The function creates a unique log file for each student ID in the list, 
## saved as either a JSON or CSV formatted file. The function currently set up to save as CSV,
## alternatively can be set for user defined action such as format=T csv if format=F, JSON set up possible.
logExtractor <- function(users,fileList,varNames,path_output){
  #Clean variables for fun
  #Loop counts number of students and logs
  numStudents <- length(users)
  numLogFiles <- length(fileList)
  path_output <- paste0(path_output,"/studentEvents/")
  #Loops through students
  for(j in 1:numStudents){
    extractLog <- NULL
    tempLog <- NULL
    curUser <- users[j]
    message("Processing student ", j, " of ", numStudents)
    #Loops through event logs
    for(i in 1:numLogFiles){
      #print update message to console
      message("Processing log file ", i, " of ", numLogFiles)
      print(proc.time() - start)
      #read log data (NOTE: logs are in NDJSON format, not typical JSON format)
      ndData <- ndjson::stream_in(fileList[i])
      #extract events for a single user, add to the complete eventLog for that user
      extractLog <- rbind.data.frame(extractLog, subset(ndData,ndData$context.user_id==curUser), fill=TRUE)
    }
    ##Number of rows identified in student's extracted log file
    extractRows <- nrow(extractLog)
    ##Next check extracted logs for data variables of interest
    #Tests to see if the event log extracted has 
    if(extractRows>1){ 
      #Creates a column with first variable in varNames vector
      #Test if variable name is in fill list (as is); Then test as a match, else an empty set
      if(varNames[1] %in% colnames(extractLog)){
        tempLog <- select(extractLog,varNames[1])
      } else if(length(extractLog[,grep(varNames[1],names(extractLog))])>1){
        tempLog <- select(extractLog,matches(varNames[1]))
      } else {
        tempLog <- data.frame(as.character(matrix("", nrow=extractRows, ncol=1)))
      }
      #Loop through each remaining variable to check if it is in the extracted log for a student
      for(k in 2:length(varNames)){
        if(varNames[k] %in% colnames(extractLog)){
          tempLog <- cbind(tempLog,select(extractLog,varNames[k]))
        } else if(length(extractLog[,grep(varNames[k],names(extractLog))])>=1){
          tempLog <- cbind(tempLog,select(extractLog,matches(varNames[k])[[1]]))
        } else {
          tempLog <- cbind(tempLog,as.character(matrix("", nrow=extractRows, ncol=1)))  
        }
      }
      #Names final data frame with the variables
      names(tempLog) <- varNames
    } 
    #Tests if tempLog has data attached before saving a log record
    if(is.null(tempLog)==F){
      #Write student log file
      write.csv(x = tempLog, file = paste0(path_output,curUser,".csv"),
                row.names = F)
    }
  }
}

#### Paths ####
#Working path directory setting by user selection. Vs using a prior assignment.
#Checks if a user has previously assign a path with a prior script 
#If false, lets user assign path to directory to read in a course'
#data from the edX data package
if(exists("path_data")==FALSE){
  path_data = tclvalue(tkchooseDirectory())
}

#Checks if a user has previously assign a path with a prior script 
#If false, lets user assign path to previous processing output files of an
#edX course using the a previous processing scripts from this pipeline.
if(exists("path_output")==FALSE){
  path_output = tclvalue(tkchooseDirectory())
}

#### Build list of all event files for course ####
#Store all the filenames of JSON formatted edX event logs within a user selected directory 
# (files ending with ".log.gz").
fileList <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = paste0(path_data,"/events/"),
                       pattern = ".gz$")

#### Identifies the output of the student user list for an edX course ####
#The pattern parameter identifies the outputs of the edX-1-studentUserList.R script
users <- read.csv(list.files(full.names = T, recursive = FALSE, 
                    path = paste0(path_output,"/userlists/"),
                    pattern = "-students.csv$"), header=T)[1]
#Sets ID list for processing
users <- users$id 

##Variable List - identifies variables of interst for a research based on comparison
# of extacted MITxPro event logs
varNames <- c("context.org_id","context.course_id","context.user_id","context.path",
              "time","session","event","event_source","event_type","module_id",
              "problem_id","attempts","grade","max_grade","state.done","student_answer",
              "submission","success")

#### Main processing ####
## Start timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute elapsed time of script)

## Log Capture function for list of users
logExtractor(users=users, fileList = fileList,
             varNames = varNames, path_output = path_output)

#### Finishing details ####
#Indicate completion
message("\n**** Complete! ****\n")

## Script processing time feedback
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in min):\n")
print((proc.time()[3] - start[3])/60)

## Clear environment variables
rm(logExtractor,start,users,varNames,fileList)