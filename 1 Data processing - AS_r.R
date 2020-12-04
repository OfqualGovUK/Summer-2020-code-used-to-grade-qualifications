
################################################################################################
##### Processing data for DCP approach - AS - updated 06/07/20 ######
###############################################################################################

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
HistoricAS<-rbind(GCE2019, GCE2018, GCE2017)

#remove initial 0's from centre numbers
HistoricAS$CentreNo<-as.character(gsub("(^|[^0-9])0+","", HistoricAS$CentreNo))

#import historic NCN
setwd("/path/to/folder")
historicNCN<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)
historicNCN$NCN_old<-as.character(historicNCN$NCN_old)
historicNCN$NCN_new<-as.character(historicNCN$NCN_new)

#replace NCN if new NCN
HistoricAS<-left_join(HistoricAS, historicNCN, by=c("CentreNo"="NCN_old"))
HistoricAS$CentreNo<-ifelse(is.na(HistoricAS$NCN_new), HistoricAS$CentreNo, HistoricAS$NCN_new)

#merge in subject mapping
HistoricAS<-HistoricAS%>%select(-c(SubjectGroup, QualificationLevel, Type, ReformPhase))
HistoricASS<-inner_join(HistoricAS, subjectmapping, by=c("SpecificationCode"))
HistoricASC<-inner_join(HistoricAS, subjectmapping, by=c("CertificationCode"))
HistoricAS<-plyr::rbind.fill(HistoricASS, HistoricASC)
HistoricAS<-HistoricAS%>%select(-c(CertificationCode.x, CertificationCode.y, SpecificationCode.x, SpecificationCode.y))


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

#set centre variable to character and remove initial 0's
GCE2020$CentreNo<-as.character(gsub("(^|[^0-9])0+","", GCE2020$CentreNo))


#read in CAG data - MAY BE IN DIFFERENT FORMAT, CHANGE AS APPROPRIATE
setwd("/path/to/folder")
L3CAGdata<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)

#set centre variable to character
L3CAGdata$CentreNo<-as.character(gsub("(^|[^0-9])0+","", L3CAGdata$CentreNo))

#filter to AS only
L3CAGdata<-L3CAGdata%>%filter(QualificationLevel=="AS")

#remove unused variables
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
AS2020<-subset(matchall, !CentreAssessedGrade%in%c("X", "Q", "-2", ""))  

#merge in subject mapping
AS2020S<-inner_join(AS2020, subjectmapping, by=c("SpecificationCode"))
AS2020C<-inner_join(AS2020, subjectmapping, by=c("CertificationCode"))
AS2020<-plyr::rbind.fill(AS2020S, AS2020C)
AS2020<-AS2020%>%select(-c(CertificationCode.x, CertificationCode.y, SpecificationCode.x, SpecificationCode.y))


############ Data Filters ################################

#remove non-regulated quals
HistoricAS<-subset(HistoricAS, ReformPhase%in%c("Ofqual-regulated Phase 1 reformed AS", "Ofqual-regulated Phase 2 reformed AS", "Ofqual-regulated Phase 3 reformed AS", "Pre-reform AS"))
HistoricAS<-subset(HistoricAS, SubjectGroup!="")

#Only data on A-G grading scale
HistoricAS<-subset(HistoricAS, SubjectLevelGrade%in%c("A", "B", "C", "D", "E", "U"))

AS2020<-subset(AS2020, CentreAssessedGrade%in%c("A", "B", "C", "D", "E", "U"))

########################################################################################################

#identify years for modelling
yearminus1and2and3<-subset(HistoricAS, ExamSeries%in%c("June 2019", "June 2018", "June 2017"))
currentyear<-AS2020

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

#create centre type and qualification type lookup for conistent output
centretype<-currentyear%>%select(CentreNo, CentreType)%>%distinct(CentreNo, .keep_all = TRUE)
qualtype<-currentyear%>%select(SubjectGroup, Type)%>%distinct(SubjectGroup, .keep_all = TRUE)




