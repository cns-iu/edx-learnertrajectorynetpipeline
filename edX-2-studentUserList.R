## ====================================================================================== ##
# Title:        Extracts list of student users in an edX course
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
#            2) A data table of users for an edX course:
#               - {org}-{course Identifier}-{term}-auth_user-{server}-analytics.sql 
#                 or .csv file (source: edX research documentation);
#            3) A data table of user profiles from an edX course:
#               - {org}-{course Identifier}-{term}-auth_userprofile-{server}-analytics.sql
#                 or .csv file (source: edX research documentation);
#            4) A data table of course enrollments
#               - {org}-{course Identifier}-{term}-enrollments-{server}-analytics.sql
#                 or .csv file (source: edX research documentation);
#            5) A data table of grades for students in the course
#               - {org}-{course Identifier}-{term}-coursegrades-{server}-analytics.sql
#                 or .csv file (source: edX research documentation);
#            6) A data table of course certificates awarded to students
#               - {org}-{course Identifier}-{term}-certificates-{server}-analytics.sql
#                 or .csv file (source: edX research documentation);
#            7) A data table of roles for users in the course.
#               - {org}-{course Identifier}-{term}-rolecourse-{server}-analytics.sql
#                 or .csv file (source: edX research documentation).
#
# Note: File names may vary based on how edX Data Czar exports data sets. Efforts have
# been made to ensure that potential variations for user data files may be loaded.
# 
# Output files:  
#            1) A data table of student user identifiers, grade, certificate status
#               and demographics:
#               - {org}-{course Identifier}-{term}-auth_user-students.csv;
#               - Used in scripts:
#                 * edX-3-eventLogExtractor.R
#                 * edX-4-eventLogFormatter.R
#
# Package dependencies: tcltk, plyr, stringr, Hmisc
#
# Change log:
#   2017.04.04. Initial Code
#   2018.04.05. Description, title, and file input stack 
#               updated function parameter definitions; creates lists of users for 
#               extraction for course
#   2018.05.21  Script format alignment.
#   2018.07.02  File output stack updates.
#   2018.07.03  Update script for new course data naming conventions; user table now
#               includes grades, certificates, and enrollment data. User list is subset
#               for active student users.
#   2018.07.22  Updated script to remove users from list who are on the rolecourse.csv 
#               data set. These are instructors who are presented as non-staff to 
#               students in the course. Update to remove research staff in student pool.
#   2018.09.12  Updated to accommidate courses that may or may not have a data set of
#               user roles or grades provided directly in edX data package; limit fields
#               returned in final user data.
#   2018.09.20  Update to reorder script lines that update "letter_grade" field.
#   2019.05.13  Update data loading, preserved fields and their order 
#               in output file "...auth_user-students.csv"; updated file input stack.
#
## ====================================================================================== ##
#### Environment setup ####
## Clean the R environment
#rm(list=ls()) 
rm(course,coursemeta,mod,modList,temp,cols,fileList,i,id,moduleID,start)
## Load required packages 
require("tcltk2")     #for OS independent GUI file and folder selection
require("stringr")    #for string manipulation
require("plyr")       #for table joins
require("Hmisc")      # %nin% function

#### Paths #### 
#Assigns path to directory where R may read in a course' state data from the edX data package 
#path_data = tclvalue(tkchooseDirectory())
path_data <- p2
#Assigns path where R saves processing outputs for user logs
#path_output = tclvalue(tkchooseDirectory())
path_output <- p_o2

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
#User Profile data
userProf <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = paste0(path_data,"/state/"),
                       pattern = "profile")
#Course certificate award data
certs <- list.files(full.names = TRUE, recursive = FALSE, 
                    path = paste0(path_data,"/state/"),
                    pattern = "cert")

## The enrollment and grade data tables may not be included for every course
#Enrollment data
enroll <- list.files(full.names = TRUE, recursive = FALSE, 
                     path = paste0(path_data,"/state/"),
                     pattern = "enroll")
#Overall course grade data
grade <- list.files(full.names = TRUE, recursive = FALSE, 
                     path = paste0(path_data,"/state/"),
                     pattern = "coursegrade")

#Course Roles
role <- list.files(full.names = TRUE, recursive = FALSE, 
                    path = paste0(path_data,"/state/"),
                    pattern = "rolecourse.csv$")

#### Processing Parameters ####
## Parameter to remove known research users from course lists
#TRUE means these users are removed, FALSE means they are kept in the data.
researchers=T

## Sets course certificate (grading) threshold based on grading policy for the course overall
if(length(list.files(full.names = TRUE, recursive = FALSE, 
                       path = paste0(path_data,"/state/"),
                       pattern = "grading_policy.csv$"))>0){
  pass_grade = read.csv(list.files(full.names = TRUE, recursive = FALSE, 
                                   path = paste0(path_data,"/state/"),
                                   pattern = "grading_policy.csv$"),header = T)$overall_cutoff_for_pass[1]
} else {
  pass_grade = .65
}

## Student Age User settings
#Sets maximum age boundary for year of birth
maxAge=75
#Sets minimum age boundary for year of birth
minAge=20

#### Read in data sets and update names ####
#Checks to see if file name ends in SQL or csv
if(grepl("\\.sql",userFilename)==T){
  users <- read.delim(userFilename[1],header=T, sep="\t")[,c(1,7,8,11)]
  userProf <- read.delim(userProf[1],header=T, sep="\t")[,c(2,8,10:11,14)]
  certs <- read.delim(certs[1],header=T, sep="\t")[,c(2,12,4,8)]
  enroll <- read.delim(enroll[1],header=T, sep="\t")[,c(2,4)]
  names(certs)[2:4] <- c("cert_created_date","percent_grade","letter_grade")
} else {
  users <- read.csv(userFilename[1],header=T)[,c(1,7,8,11)]
  userProf <- read.csv(userProf[1],header=T)[,c(2,8,10:11,14)]
  certs <- read.csv(certs[1],header=T)[,c(2,12,4,8)]
  enroll <- read.csv(enroll[1],header=T)[,c(2,4)]
  names(certs)[2:4] <- c("cert_created_date","percent_grade","letter_grade")
}
names(userProf)[1] <- names(enroll)[1] <- names(certs)[1] <- 'id'
names(enroll)[2] <-c("enroll_created_date")

#These fields are tested because not all edX data packages incorporate these files
#Loads in grade data OR creates the grade data frame based on certificate data set
if(length(grade)>0){
  grade <- read.csv(grade[1],header=T)[,c(2,4:8)]
  names(grade)[c(1,5,6)] <- c("id","grade_created_date","grade_modified_date")
} else {
  if(ncol(certs)==4){
    grade <- certs[,c(1,3,4)]
    grade$letter_grade <- as.character(grade$letter_grade)
     if(length(grade[grade$letter_grade!="downloadable",]$letter_grade)>0){
       grade[grade$letter_grade!="downloadable",]$letter_grade <- "Not Passing" 
     }
       grade[grade$letter_grade=="downloadable",]$letter_grade <- "Pass"
    certs <- certs[,c(1,2)]
  }
}

if(length(role)>0){
  role <- read.csv(role[1],header=T)[,c(3:4)]  
} else {
  role <- data.frame(matrix(data=NA,nrow=1,ncol=2))
  names(role) <- c("user_id","role")
}

## Combine Data sets
users <- join(users,userProf,by="id")
users <- join(users,enroll,by="id")
users <- join(users,grade,by="id")
users <- join(users,certs,by="id")
rm(userProf,certs,enroll,grade)

#Rearrange columns
users <- users[c(1:3,5:8,10,11,4,9,12:15)]

#### Subset to remove staff and researchers #### 
#Subset users to remove non-students from the list
users <- users[users$is_staff==0 & users$is_active==1,]

#Checks against roles for administrators not listed correctly in user list
users <- users[which(users$id %nin% role$user_id),]

#Checks for know Boeing IU research team
if(researchers==T){
  role <- list.files(full.names = TRUE, recursive = FALSE, 
                     path = paste0(path_output,"/userlists/"),
                     pattern = "researchteam_ids.csv$")
  role <- read.csv(role[1],header=T)[1:2]
  users <- users[which(users$id %nin% role$user_id),]
}
rm(role)

#### Final clean-up of user data set####
## Grades & Certification fields
if(nrow(users[is.na(users$percent_grade),])>0){
  users[is.na(users$percent_grade),]$percent_grade <- 0.00  
}

if(nrow(users[grepl("Pass",users$letter_grade)==F,])>0) {
  users$letter_grade <- as.character(users$letter_grade)
  users[grepl("Pass",users$letter_grade)==F,]$letter_grade <- "Not Passing"
}

## Creates Percentiles and Certification Group Field used in GLMs and Visualization Groupings
users$percentile <- ecdf(users$percent_grade)(users$percent_grade)
users$certGrp <- NA
users$certGrp <- ifelse(users$percent_grade>pass_grade,paste0("Certified (< ",pass_grade*100,"% Grade)"),paste0("Not certified (> ",pass_grade*100,"% Grade)"))
users$certGrp <- as.factor(users$certGrp)
rm(pass_grade)

## Clean-up Gender field
users$gender <- as.character(users$gender)
clean <- c("NULL","","none","o","other")
for(i in 1:length(clean)){
  if(nrow(users[users$gender==clean[i] & !is.na(users$gender),])>0){
    users[users$gender==clean[i] & !is.na(users$gender),]$gender <- NA  
  }
}
users$gender <- factor(users$gender,levels=c("m","f"),labels=list("Male","Female"))

## Clean-up Year of Birth field
users$year_of_birth <- as.character(users$year_of_birth)
if(nrow(users[users$year_of_birth=="NULL",])>0){
  users[users$year_of_birth=="NULL",]$year_of_birth <- NA  
}
#Sets max reportable year to within a retirement window 75
if(nrow(users[users$year_of_birth <= (as.numeric(format(Sys.Date(), "%Y"))-maxAge) & !is.na(users$year_of_birth),])>0){
  users[users$year_of_birth <= (as.numeric(format(Sys.Date(), "%Y"))-maxAge) & !is.na(users$year_of_birth),]$year_of_birth <- NA  
}
#Sets min age to year of young employees (college grads) 20
if(nrow(users[users$year_of_birth >= (as.numeric(format(Sys.Date(), "%Y"))-minAge) & !is.na(users$year_of_birth),])>0){
  users[users$year_of_birth >= (as.numeric(format(Sys.Date(), "%Y"))-minAge) & !is.na(users$year_of_birth),]$year_of_birth <- NA  
}
users$year_of_birth <- as.numeric(users$year_of_birth)
rm(maxAge,minAge)

## Clean-up Level of Education field
users$level_of_education <- as.character(users$level_of_education)
for(i in 1:length(clean)){
  if(nrow(users[users$level_of_education==clean[i] & !is.na(users$level_of_education),])>0){
    users[users$level_of_education==clean[i] & !is.na(users$level_of_education),]$level_of_education <- NA  
  }
}
if(nrow(users[users$level_of_education=="p_oth" & !is.na(users$level_of_education),])>0){
  users[users$level_of_education=="p_oth" & !is.na(users$level_of_education),]$level_of_education <- "p"  
}
users$level_of_education <- factor(users$level_of_education,levels=c("hs","a","b","m","p"),labels=c("High School","Associates","Bachelors","Masters","Doctoral"))
rm(clean,i)

#Updates names of student demographi and performance data
names(users)[c(5,6,12)] <- c("yob","loe","cert_status")

#Clean up country field
if(nrow(users[users$country=="NULL",])>0){
  users[users$country=="NULL",]$country <- ""  
}

#Reorders columns
users <- users[,c(1,4:7,8:9,16,17,10,11,13,14,12,15)]

#### Exporting the user data set ####
## Load Course Metadata
# This is used for file naming
meta <- list.files(full.names = TRUE, recursive = FALSE, 
                   path = paste0(path_output,"/course/"),
                   pattern = "meta")
meta <- read.csv(meta[1],header=T)[1]
meta$id <-gsub("\\+","\\-",meta$id)

#Sets file name for user list
userFilename <- paste0(meta$id,"-auth_user-students")
#Saves user list as a csv
write.csv(users,file=paste0(path_output,"/userlists/",userFilename,".csv"), row.names=F)

#### Finishing details ####
#Indicate completion
message("\n**** Complete! ****\n")

## Script processing time feedback
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print((proc.time()[3] - start[3])/60)
## Clear environment variables
rm(researchers,meta,users,start,userFilename)