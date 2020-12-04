
#######################################################################
##### Processing data for DCP approach - GCSE - updated 06/07/20 ######
#######################################################################

.libPaths("/path/to/folder")

library(tidyverse)

#Import subject mapping (version 3)
setwd("/path/to/folder")
subjectmapping<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)
subjectmapping<-subjectmapping%>%select(SubjectGroup, QualificationLevel, Type, ReformPhase, EntryCode=D5..EntryCode, SpecificationCode=D7.D8...SpecificationCode, CertificationCode=D7.D8...CertificationCode)

#WORKING DIRECTORY NEEDS TO BE UPDATED TO FOLDER WHERE DATA FILES ARE STORED
#import historical data
setwd("/path/to/folder")
GCSE2019<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)
GCSE2018<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)
HistoricGCSE<-rbind(GCSE2019, GCSE2018)

#remove initial 0's from centre numbers
HistoricGCSE$CentreNo<-as.character(gsub("(^|[^0-9])0+","", HistoricGCSE$CentreNo))


#import historic NCN
setwd("/path/to/folder")
historicNCN<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)
historicNCN$NCN_old<-as.character(historicNCN$NCN_old)
historicNCN$NCN_new<-as.character(historicNCN$NCN_new)

#replace NCN if new NCN
HistoricGCSE<-left_join(HistoricGCSE, historicNCN, by=c("CentreNo"="NCN_old"))
HistoricGCSE$CentreNo<-ifelse(is.na(HistoricGCSE$NCN_new), HistoricGCSE$CentreNo, HistoricGCSE$NCN_new)

#merge in subject mapping
HistoricGCSE<-HistoricGCSE%>%select(-c(SubjectGroup, QualificationLevel, Type, ReformPhase))
HistoricGCSES<-inner_join(HistoricGCSE, subjectmapping, by=c("SpecificationCode"))
HistoricGCSEC<-inner_join(HistoricGCSE, subjectmapping, by=c("CertificationCode"))
HistoricGCSE<-plyr::rbind.fill(HistoricGCSES, HistoricGCSEC)
HistoricGCSE<-HistoricGCSE%>%select(-c(CertificationCode.x, CertificationCode.y, SpecificationCode.x, SpecificationCode.y))


#import current years data
setwd("/path/to/folder")
GCSE2020<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)

#Rename variables to match historic data
GCSE2020<-GCSE2020%>%rename(SubjectGroup=Subject.Grouping,
                          EntryCode=Entry.Code,
                          CentreNo=Centre.number,
                          ReformPhase=Reform.Phase,
                          PrivateCandidate=Private.Candidate.Flag,
                          CentreType=Centre.Type,
                          QualificationLevel=Qualification.Level)

GCSE2020<-GCSE2020%>%select(-c(ReformPhase, QualificationLevel, Type, SubjectGroup))

#set centre variable to character
GCSE2020$CentreNo<-as.character(gsub("(^|[^0-9])0+","", GCSE2020$CentreNo))

#read in CAG data - MAY BE IN DIFFERENT FORMAT, CHANGE AS APPROPRIATE
setwd("/path/to/folder")
L2CAGdata<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)

#set centre variable to character
L2CAGdata$CentreNo<-as.character(gsub("(^|[^0-9])0+","", L2CAGdata$CentreNo))

L2CAGdata<-L2CAGdata%>%select(UniqueCandidateIdentifier, CentreNo, CertificationCode, SpecificationCode, CentreAssessedGrade, CentreRankOrder)

#merge 2020 data with CAG data
#CHECK MERGES ALL CANDIDATES CORRECTLY AND NO DUPLICATES CREATED
match1<-inner_join(L2CAGdata, GCSE2020, by = c("UniqueCandidateIdentifier", "CentreNo",  "CertificationCode"="EntryCode"))
anti1<-anti_join(L2CAGdata, GCSE2020, by = c("UniqueCandidateIdentifier", "CentreNo",   "CertificationCode"="EntryCode"))
match2<-inner_join(anti1, GCSE2020, by = c("UniqueCandidateIdentifier", "CentreNo",   "SpecificationCode"="EntryCode"))
anti2<-anti_join(anti1, GCSE2020, by = c("UniqueCandidateIdentifier", "CentreNo",   "SpecificationCode"="EntryCode"))

matchall<-plyr::rbind.fill(match1, match2)
matchall<-plyr::rbind.fill(matchall, anti2)

#remove non-valid CAGs
GCSE2020<-subset(matchall, !CentreAssessedGrade%in%c("X", "Q", "-2", ""))  

#merge in subject mapping
GCSE2020S<-inner_join(GCSE2020, subjectmapping, by=c("SpecificationCode"))
GCSE2020C<-inner_join(GCSE2020, subjectmapping, by=c("CertificationCode"))
GCSE2020<-plyr::rbind.fill(GCSE2020S, GCSE2020C)
GCSE2020<-GCSE2020%>%select(-c(CertificationCode.x, CertificationCode.y, SpecificationCode.x, SpecificationCode.y))



############# Data Filters (may change with the final version of the data) ################################

#Restrict to Ofqual regulated GCSEs
HistoricGCSE<-subset(HistoricGCSE, ReformPhase%in%c("Ofqual-regulated Phase 1 reformed GCSE FC", "Ofqual-regulated Phase 2 reformed GCSE FC", "Ofqual-regulated Phase 2 reformed GCSE SC", "Ofqual-regulated Phase 3 reformed GCSE FC", "Ofqual-regulated Phase 4 reformed GCSE FC"))
HistoricGCSE<-subset(HistoricGCSE, SubjectGroup!="")

#Only subjects on 9-1 grading scale
HistoricGCSE<-subset(HistoricGCSE, !SubjectLevelGrade%in%c("X","-","Q","","-2"))


GCSE2020<-subset(GCSE2020, CentreAssessedGrade%in%c("9","8","7","6","5","4","3","2","1","U")&SubjectGroup!="Combined Science")



########################################################################################################

#identify years for modelling
yearminus1and2<-subset(HistoricGCSE, ExamSeries%in%c("June 2019", "June 2018"))
currentyear<-GCSE2020

yearminus1and2$NormalisedKS2Score[yearminus1and2$NormalisedKS2Score=="999.999"]<-NA
currentyear$NormalisedKS2Score[currentyear$NormalisedKS2Score=="999.999"]<-NA

#List of DLP centres - UPDATE TO IMPORT DOC WHEN AVALIABLE
DLPlist<-c("00000", "00000", "00000", "00000", "00000", "00000", "00000")

#remove private candidates from historical data if not in DLP centre
yearminus1and2<-subset(yearminus1and2, CentreNo%in%DLPlist|PrivateCandidate==0)

#remove partial absentees from historical data
yearminus1and2<-subset(yearminus1and2, PartialAbsence==0)

#Only include subjects with prior performance data (Phase 4 reform subject handled seperately)
currentyear_phase4<-currentyear%>%filter(!SubjectGroup%in%yearminus1and2$SubjectGroup)
currentyear<-currentyear%>%filter(SubjectGroup%in%yearminus1and2$SubjectGroup)

#remove subjects with <100 candidates total from current data (very small subjects handled seperately)
currentyear_smallsubs<-currentyear%>%group_by(SubjectGroup)%>%mutate(count=n())%>%filter(count<100)%>%select(-count)%>%ungroup()
currentyear<-currentyear%>%group_by(SubjectGroup)%>%mutate(count=n())%>%filter(count>=100)%>%select(-count)%>%ungroup()

#split out private candidates for modelling
currentyear_nonprivate<-subset(currentyear, CentreNo%in%DLPlist|PrivateCandidate==0|is.na(PrivateCandidate))
currentyear_private<-subset(currentyear, !CentreNo%in%DLPlist&PrivateCandidate==1)

#remove unused objects from environment
rm(list=setdiff(ls(),c("currentyear", "currentyear_nonprivate","currentyear_private", "yearminus1and2")))

#create centre type and qualification lookup for  output consistency
centretype<-currentyear%>%select(CentreNo, CentreType)%>%distinct(CentreNo, .keep_all = TRUE)
qualtype<-currentyear%>%select(SubjectGroup, Type)%>%distinct(SubjectGroup, .keep_all = TRUE)

