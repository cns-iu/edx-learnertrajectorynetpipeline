## ====================================================================================== ##
# Title:        Data Anonymizer for EdX  User Data
# Project:      edX learner trajectory analysis
# 
#     Copyright 2019 Michael Ginda
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
# Description:  This script creates anonymizes student's user profile data
#               provided from an edX course. Names are removed and email addresses
#               processed to preserve anonymity of students from researchers.
#         
#               After this script is run, it is recommended that the edX course archives
#               are deleted locally so ensure no re-idenfication is possible.
#
#               Note that this script does not process the forum.csv files from an edX
#               course, which may contain information that can re-identify students.
#                     
# File input stack: 
#            1) An edX course "state" directory an edX course;
#            2) A data table of users for an edX course:
#               - {org}-{course Identifier}-{term}-auth_user-{server}-analytics.sql
#                 or .csv file (source: edX research documentation);
#            3) A data table of user id maps to an edX course forum:
#               - {org}-{course Identifier}-{term}-user_id_map-{server}-analytics.sql
#                 or .csv file (source: edX research documentation);
#            4) A data table of user profiles
#               - {org}-{course Identifier}-{term}-profiles-{server}-analytics.sql
#                 or .csv file (source: edX research documentation);
#            5) A data table of person course data table
#               - {org}-{course Identifier}-{term}-person_course-{server}-analytics.sql
#                 or .csv file (source: edX research documentation);
#            6) A data table of combined user information data table
#               - {org}-{course Identifier}-{term}-user_info_combo-{server}-analytics.sql
#                 or .csv file (source: edX research documentation);
#            7) A data table of course certificates awarded to students
#               - {org}-{course Identifier}-{term}-certificates-{server}-analytics.sql
#                 or .csv file (source: edX research documentation);
#
# Note: File names may vary based on how edX Data Czar exports data sets. Efforts have
# been made to ensure that potential variations for user data files may be loaded.
#
# Output files:  
#            1) Each input file loaded for processing is saved over the original copy
#               to ensure that only anonymized data is preserved for the research team.
#
# Package dependencies: tcltk, magrittr
#
# Change log:
#   20198.05.13  Initial script created 
#
## ====================================================================================== ##
#### Environment Set-up ####
## Load required packages 
require("tcltk2")     #for OS independent GUI file and folder selection
require("magrittr")   #for %>% pipe

#### Paths #### 
#Assigns path to directory where R may read in a course' state data from the edX data package 
path_data = tclvalue(tkchooseDirectory())
path_data = p1
## Start timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute elapsed time of script)

## Identifying student users from an edX course
#List of authenticated users extracted from edX course data package
userFilename <- list.files(full.names = TRUE, recursive = FALSE, 
                           path = paste0(path_data,"/state/"),
                           pattern = "users")
#Tests if user data is found with given pattern
if(length(userFilename)==0){
  userFilename <- list.files(full.names = TRUE, recursive = FALSE, 
                             path = paste0(path_data,"/state/"),
                             pattern = "user-")
}
#Delete Forum Mapping, which may not be desired by researcher
forumMap=T
#User ID Map to edX Forum
if(forumMap==T){
  useridMap <- list.files(full.names = TRUE, recursive = FALSE, 
                          path = paste0(path_data,"/state/"),
                          pattern = "map")
}
#User Profile data files
userProfileFile <- list.files(full.names = TRUE, recursive = FALSE, 
                              path = paste0(path_data,"/state/"),
                              pattern = "profile")

userInfoComboFile <- list.files(full.names = TRUE, recursive = FALSE, 
                                path = paste0(path_data,"/state/"),
                                pattern = "combo")

personCourseFile <- list.files(full.names = TRUE, recursive = FALSE, 
                                path = paste0(path_data,"/state/"),
                                pattern = "person_course")

#Course certificate award data
certificateFile <- list.files(full.names = TRUE, recursive = FALSE, 
                              path = paste0(path_data,"/state/"),
                              pattern = "cert")

#### Anonymization of Data ####
#Test for an SQL or CSV file extension and then processes data
if(grepl("\\.sql",userFilename)==T){
  #SQL Tab Sep formatted data
  if(length(userFilename)>0){
    users <- read.delim(userFilename[1],header=T, sep="\t")
    users[,c(2:4)] <- ""
    users$email <- str_split(users$email, pattern="\\@") %>% sapply("[",2)
    write.table(users,file=userFilename[1], row.names = F, sep = "\t")
    rm(users,userFilename)
  } else {rm(userFilename)}
  if(length(useridMap)>0){
    #Cleans mapping for forums 
    map <- read.delim(useridMap[1],header=T, sep="\t")
    map[3] <- ""
    write.table(map,file=useridMap[1], row.names = F, sep="\t")
    rm(map,useridMap)
  } else {rm(useridMap)}
  if(length(userInfoComboFile)>0){
    #Cleans mapping for forums 
    combo <- read.csv(userInfoComboFile[1],header=T)
    combo$username <- combo$profile_name <- combo$profile_meta <- 
      combo$profile_mailing_address <- combo$certificate_name <- ""
    combo$email <- str_split(combo$email, pattern="\\@") %>% sapply("[",2)
    write.table(combo,file=userInfoComboFile[1], row.names = F, sep="\t")
    rm(combo,userInfoComboFile)
  } else {rm(userInfoComboFile)}
  if(length(personCourseFile)>0){
    #Cleans mapping for forums 
    perCor <- read.csv(personCourseFile[1],header=T)
    perCor$username <- ""
    write.table(perCor,file=personCourseFile[1], row.names = F, sep="\t")
    rm(perCor,personCourseFile)
  } else {rm(personCourseFile)}
  if(length(certificateFile)>0){
    #Cleans out names in certificate data
    certs <- read.delim(certificateFile[1],header=T, sep="\t")
    certs$name <- ""
    write.table(certs,file=certificateFile[1], row.names = F,sep="\t")
    rm(certs,certificateFile)  
  } else {rm(certificateFile)}
  if(length(userProfileFile)>0){
    #Clears name, mailing address and bio entry provided by a user
    profile <- read.delim(userProfileFile[1],header=T, sep="\t")
    profile[,c(3,6,9,16)] <- ""
    write.table(profile,file=userProfileFile[1],row.names = F, sep="\t")
    rm(userProfileFile,profile)
  } else {rm(userProfileFile)}
 } else {
  #CSV Sep formatted data
  if(length(userFilename)>0){
    users <- read.csv(userFilename[1],header=T) 
    users$username <- users$first_name <- users$last_name <- ""
    users$email <- str_split(users$email, pattern="\\@") %>% sapply("[",2)
    write.csv(users,file=userFilename[1], row.names = F)
    rm(users,userFilename)
  } else {rm(userFilename)}
  if(length(useridMap)>0){
    #Cleans mapping for forums 
    map <- read.csv(useridMap[1],header=T)
    map[3] <- ""
    write.csv(map,file=useridMap[1], row.names = F)
    rm(map,useridMap)
  } else {rm(useridMap)}
  if(length(userInfoComboFile)>0){
    #Cleans mapping for forums 
    combo <- read.csv(userInfoComboFile[1],header=T)
    combo$username <- combo$profile_name <- combo$profile_meta <- 
      combo$profile_mailing_address <- combo$certificate_name <- ""
    combo$email <- str_split(combo$email, pattern="\\@") %>% sapply("[",2)
    write.csv(combo,file=userInfoComboFile[1], row.names = F)
    rm(combo,userInfoComboFile)
  } else {rm(userInfoComboFile)}
  if(length(personCourseFile)>0){
   #Cleans mapping for forums 
   perCor <- read.csv(personCourseFile[1],header=T)
   perCor$username <- ""
   #Note the person_course file also contains detailed geospatial data about users
   write.csv(perCor,file=personCourseFile[1], row.names = F)
   rm(perCor,personCourseFile)
  } else {rm(personCourseFile)} 
  if(length(certificateFile)>0){
    #Cleans out names in certificate data
    certs <- read.csv(certificateFile[1],header=T)
    certs$name <- ""
    write.csv(certs,file=certificateFile[1], row.names = F)
    rm(certs,certificateFile)  
  } else {rm(certificateFile)}
  if(length(userProfileFile)>0){
    #Clears name, mailing address and bio entry provided by a user
    profile <- read.csv(userProfileFile[1],header=T)
    profile[,c(3,6,9,16)] <- ""
    write.csv(profile,file=userProfileFile[1],row.names = F)
    rm(userProfileFile,profile)
  } else {rm(userProfileFile)}
 }

#### Finishing details ####
#Indicate completion
message("\n**** Complete! ****\n")
## Script processing time feedback
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in minutes):\n")
print((proc.time()[3] - start[3])/60)
rm(forumMap,start)