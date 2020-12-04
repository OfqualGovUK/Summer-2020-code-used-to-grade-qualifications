
################################################################################################
##### Processing data for DCP approach - A Level - updated 06/07/20 ######
###############################################################################################

.libPaths("/path/to/folder")

library(tidyverse)

#Import subject mapping (version 3)
setwd("/path/to/folder")
subjectmapping<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)
subjectmapping<-subjectmapping%>%select(SubjectGroup, QualificationLevel, Type, ReformPhase, EntryCode=D5..EntryCode, SpecificationCode=D7.D8...SpecificationCode, CertificationCode=D7.D8...CertificationCode)


#WORKING DIRECTORY NEEDS TO BE UPDATED TO FOLDER WHERE DATA FILES ARE STORED
#import historical data
setwd("/path/to/folder")
GCE2019<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)
GCE2018<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)
GCE2017<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)
HistoricAlevel<-rbind(GCE2019, GCE2018, GCE2017)

#remove initial 0's from centre numbers
HistoricAlevel$CentreNo<-as.character(gsub("(^|[^0-9])0+","", HistoricAlevel$CentreNo))

#update A* in historic data
HistoricAlevel$SubjectLevelGrade[HistoricAlevel$SubjectLevelGrade=="*"]<-"A*"

#import historic NCN
setwd("/path/to/folder")
historicNCN<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)
historicNCN$NCN_old<-as.character(historicNCN$NCN_old)
historicNCN$NCN_new<-as.character(historicNCN$NCN_new)

#replace NCN if new NCN
HistoricAlevel<-left_join(HistoricAlevel, historicNCN, by=c("CentreNo"="NCN_old"))
HistoricAlevel$CentreNo<-ifelse(is.na(HistoricAlevel$NCN_new), HistoricAlevel$CentreNo, HistoricAlevel$NCN_new)

#merge in subject mapping
HistoricAlevel<-HistoricAlevel%>%select(-c(SubjectGroup, QualificationLevel, Type, ReformPhase))
HistoricAlevelS<-inner_join(HistoricAlevel, subjectmapping, by=c("SpecificationCode"))
HistoricAlevelC<-inner_join(HistoricAlevel, subjectmapping, by=c("CertificationCode"))
HistoricAlevel<-plyr::rbind.fill(HistoricAlevelS, HistoricAlevelC)
HistoricAlevel<-HistoricAlevel%>%select(-c(CertificationCode.x, CertificationCode.y, SpecificationCode.x, SpecificationCode.y))


#import current years data
setwd("/path/to/folder")
GCE2020<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)

#Rename variables to match historic data
GCE2020<-GCE2020%>%rename(SubjectGroup=Subject.Grouping,
                          EntryCode=Entry.Code,
                                CentreNo=Centre.number,
                                ReformPhase=Reform.Phase,
                                PrivateCandidate=Private.Candidate.Flag,
                                CentreType=Centre.Type,
                                QualificationLevel=Qualification.Level)

GCE2020<-GCE2020%>%select(-c(ReformPhase, QualificationLevel, Type, SubjectGroup))

#remove initial 0's from centre numbers
GCE2020$CentreNo<-as.character(gsub("(^|[^0-9])0+","", GCE2020$CentreNo))


#read in CAG data - MAY BE IN DIFFERENT FORMAT, CHANGE AS APPROPRIATE
setwd("/path/to/folder")
L3CAGdata<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)

#set centre variable to character and remove iniital 0's
L3CAGdata$CentreNo<-as.character(gsub("(^|[^0-9])0+","", L3CAGdata$CentreNo))

#filter to A-level
L3CAGdata<-L3CAGdata%>%filter(QualificationLevel=="A level")

L3CAGdata<-L3CAGdata%>%select(UniqueCandidateIdentifier, CentreNo, CertificationCode, SpecificationCode, CentreAssessedGrade, CentreRankOrder)

#merge 2020 data with CAG data
#CHECK MERGES ALL CANDIDATES CORRECTLY AND NO DUPLICATES CREATED
match1<-inner_join(L3CAGdata, GCE2020, by = c("UniqueCandidateIdentifier", "CentreNo",  "CertificationCode"="EntryCode"))
anti1<-anti_join(L3CAGdata, GCE2020, by = c("UniqueCandidateIdentifier", "CentreNo",   "CertificationCode"="EntryCode"))
match2<-inner_join(anti1, GCE2020, by = c("UniqueCandidateIdentifier", "CentreNo",   "SpecificationCode"="EntryCode"))
anti2<-anti_join(anti1, GCE2020, by = c("UniqueCandidateIdentifier", "CentreNo",   "SpecificationCode"="EntryCode"))

matchall<-plyr::rbind.fill(match1, match2)
matchall<-plyr::rbind.fill(matchall, anti2)

#remove non-valid CAGs
Alevel2020<-subset(matchall, !CentreAssessedGrade%in%c("X", "Q", "-2", ""))  

#merge in subject mapping
Alevel2020S<-inner_join(Alevel2020, subjectmapping, by=c("SpecificationCode"))
Alevel2020C<-inner_join(Alevel2020, subjectmapping, by=c("CertificationCode"))
Alevel2020<-plyr::rbind.fill(Alevel2020S, Alevel2020C)
Alevel2020<-Alevel2020%>%select(-c(CertificationCode.x, CertificationCode.y, SpecificationCode.x, SpecificationCode.y))


############# Data Filters ################################

#remove non-regulated quals
HistoricAlevel<-subset(HistoricAlevel, ReformPhase%in%c("Ofqual-regulated Phase 1 reformed A-level", "Ofqual-regulated Phase 2 reformed A-level", "Ofqual-regulated Phase 3 reformed A-level", "Ofqual-regulated Phase 4 reformed A-level", "Pre-reform A-level"))
HistoricAlevel<-subset(HistoricAlevel, SubjectGroup!="")
                       
#Only data on A*-G grading scale
HistoricAlevel<-subset(HistoricAlevel, SubjectLevelGrade%in%c("A*", "A", "B", "C", "D", "E", "U"))

Alevel2020<-subset(Alevel2020, CentreAssessedGrade%in%c("A*", "A", "B", "C", "D", "E", "U"))

########################################################################################################

#identify years for modelling
yearminus1and2and3<-subset(HistoricAlevel, ExamSeries%in%c("June 2019", "June 2018", "June 2017"))
currentyear<-Alevel2020

#List of DLP centres 
DLPlist<-c("00000", "00000", "00000", "00000", "00000", "00000", "00000")

#remove private candidates from historical data if not in DLP centre
yearminus1and2and3<-subset(yearminus1and2and3, CentreNo%in%DLPlist|PrivateCandidate==0)

#remove partial absentees from historical data
yearminus1and2and3<-subset(yearminus1and2and3, PartialAbsence==0)

#Only include subjects with prior performance data
currentyear_phase4<-currentyear%>%filter(!SubjectGroup%in%yearminus1and2and3$SubjectGroup)
currentyear<-currentyear%>%filter(SubjectGroup%in%yearminus1and2and3$SubjectGroup)

#remove subjects with <100 candidates total from current data (very small subjects handled seperately)
currentyear_smallsubs<-currentyear%>%group_by(SubjectGroup)%>%mutate(count=n())%>%filter(count<100)%>%select(-count)%>%ungroup()
currentyear<-currentyear%>%group_by(SubjectGroup)%>%mutate(count=n())%>%filter(count>=100)%>%select(-count)%>%ungroup()

#split out private candidates for modelling
currentyear_nonprivate<-subset(currentyear, CentreNo%in%DLPlist|PrivateCandidate==0|is.na(PrivateCandidate))
currentyear_private<-subset(currentyear, !CentreNo%in%DLPlist&PrivateCandidate==1)

#remove unused objects from environment
rm(list=setdiff(ls(),c("currentyear", "currentyear_nonprivate","currentyear_private", "yearminus1and2and3")))

#create centre type and qualification type lookup for consistent output
centretype<-currentyear%>%select(CentreNo, CentreType)%>%distinct(CentreNo, .keep_all = TRUE)
qualtype<-currentyear%>%select(SubjectGroup, Type)%>%distinct(SubjectGroup, .keep_all = TRUE)






