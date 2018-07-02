## ====================================================================================== ##
# Title:        Extracts list of student users in an edX course
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
# Description:  This script creates a list of active students users in an edX course, 
#               based on authenticated user and user profile tables provided in a 
#               edX Data Package.
#         
#               The script the list of course participants (instructors, administrators, 
#               and students) and user demographic profiles, and joins these data frames 
#               using the users' edX identifiers (which are also used in the log files). 
#               Only relevant fields are maintained (id, is_staff, is_active, is_superuser, 
#               last_login, date_joined, gender, year_of_birth, level_of_education, 
#               goals, allow_certificate, country), a detailed description of these fields
#               may be found in the edX Research Guide.
#                     
# File input stack: 
#            1) An edX course "state" directory an edX course;
#            2) An data table of users for an edX course:
#               - {org}-{course Identifier}-{term}-auth_user-{server}-analytics.sql
#                 (source: edX research documentation);
#            3) A data table of user profiles from an edX course:
#               - {org}-{course Identifier}-{term}-auth_userprofile-{server}-analytics.sql
#                 (source: edX research documentation)
# 
# Output files:  
#            1) A data table of student user identifiers, grade, certificate status
#               and demographics:
#               - {org}-{course Identifier}-{term}-auth_user-students.csv;
#               - Used in scripts:
#                 * edX-2-eventLogExtractor.R
#                 * edX-3-learnerLogFormatter.R
#
# Package dependencies: tcltk, plyr, stringr
#
# Change log:
#   2017.04.04. Initial Code
#   2018.04.05. Description, title, and file input stack 
#               updated function parameter definitions; creates lists of users for 
#               extraction for course
#   2018.05.21  Script format alignment.
#   2018.07.02  File output stack updates.
## ====================================================================================== ##
######### Setup ########## 
## _Clean the environment ####
rm(list=ls()) 

## _start timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute elapsed time of script)

## _Load required packages #####
require("tcltk2")     #for OS independent GUI file and folder selection
require("stringr")    #for string manipulation
require("plyr")       #for table joins

######### Main ########## 
#Assigns path where R may read in state data from the edX data package 
path_state = tclvalue(tkchooseDirectory())
#Assigns path where R saves processing outputs for user logs
path_output = paste0(tclvalue(tkchooseDirectory()),"/")

##Identifying student users from an edX course
#List of authenticated users extracted from edX course data package
userList <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = path_state,
                       pattern = "auth_user")
users <- read.csv(userList[1],sep = '\t',header=T)[,c(1,7:11)]
userProf <- read.csv(userList[2],sep = '\t',header=T)[,c(2,8,10:14)]
names(userProf)[1] <- 'id'
users <- join(users,userProf,by="id")

#Subset users to remove non-students from the list
users <- users[users$is_staff==0,]

#Saves the list of students for the analysis
userFileName <- sapply(str_split(userList[1],pattern="/"),tail,1)
userFileName <- str_split(userFileName,pattern="-")
#Sets file name for edX course user list (uses format {org}-{course Identifier}-{term}-auth_user-students)
userFileName <- paste(userFileName[[1]][1],userFileName[[1]][2],userFileName[[1]][3],userFileName[[1]][4],"students",sep="-")
write.csv(users,file=paste0(path_output,"userlists/",userFileName,".csv"), row.names=F)

######### Finishing Details ########## 
#Indicate completion
message("\n**** Complete! ****\n")

## _Script processing time feedback #####
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print(proc.time() - start)

## _Clear environment variables
rm(list=ls())   
