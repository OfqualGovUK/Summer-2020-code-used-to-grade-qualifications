#######################################################################################
##### Functions for allocating grades to candidates A level - - updated 06/07/20 ######
#######################################################################################

#Read in predictions
TargetDistributions<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)

#filter to A-level
TargetDistributions<-subset(TargetDistributions, Qualification.Level=="A-level")

#include list of subjects which should not be adjusted to meet predictions
AlevelSmallSubList<-TargetDistributions$Subject.Grouping[TargetDistributions$Total.Matched<500]


#set directory to save output
setwd("/path/to/folder")

# 1. run model to impute marks
currentyear_nonprivate_imputedmarks<-Imputemarks(currentyear_private, currentyear_nonprivate, Alevelmodeloutput, outputs=TRUE)

# 2. calculate grade boundaries based on marks and required cumulative frequencies except subjects in small subject list 
Alevelgradeboundaries<-calculategradeboundaries(currentyear_nonprivate_imputedmarks, TargetDistributions, outputs=TRUE)

# 3. impute grades based on marks and grade boundaries except subjects in small subject list 
currentyear_nonprivate_imputedgrades<-Imputegradebasedonmark(currentyear_private, currentyear_nonprivate_imputedmarks,Alevelgradeboundaries, outputs=FALSE)

  
  
######################################### FUNCTIONS - RUN ALL BELOW FIRST ##################################################  
############### Function for imputing marks ######################################
Imputemarks<-function(currentdata_private, currentdata, modeloutput, outputs=FALSE){

  #join model output to candidate level data
candidatemarks<-left_join(currentdata, modeloutput, by = c("SubjectGroup", "CentreNo"))

#create single rank order across grades excluding private candidates for allocating marks
candidatemarks<-candidatemarks%>%group_by(SubjectGroup, CentreNo)%>%
  arrange(factor(CentreAssessedGrade, levels=c("A*", "A", "B", "C", "D", "E","U")), CentreRankOrder)%>%
  mutate(rank_noprivate=1:n(),
         totalcands=length(CentreAssessedGrade))%>%
           ungroup()
 
#assign marks to candidates based on rank order and centre predicted outcomes         
candidatemarks<-candidatemarks%>%mutate(rankperc=(rank_noprivate-0.5)/totalcands,
                                        maxgrade = case_when(
                                          rankperc <= PredAStar ~ 6, 
                                          rankperc <= PredA ~ 5,
                                          rankperc <= PredB ~ 4,
                                          rankperc <= PredC ~ 3,
                                          rankperc <= PredD ~ 2,
                                          rankperc <= PredE ~ 1,
                                          rankperc <= PredU ~ 0,
                                          TRUE ~ NA_real_), 
                                        cumperc = case_when(
                                          rankperc <= PredAStar ~ PredAStar, 
                                          rankperc <= PredA ~ PredA,
                                          rankperc <= PredB ~ PredB,
                                          rankperc <= PredC ~ PredC,
                                          rankperc <= PredD ~ PredD,
                                          rankperc <= PredE ~ PredE,
                                          rankperc <= PredU ~ PredU,
                                          TRUE ~ NA_real_), 
                                        cumpercplus1 = case_when(
                                          rankperc <= PredAStar ~ 0,
                                          rankperc <= PredA ~ PredAStar,
                                          rankperc <= PredB ~ PredA,
                                          rankperc <= PredC ~ PredB,
                                          rankperc <= PredD ~ PredC,
                                          rankperc <= PredE ~ PredD,
                                          rankperc <= PredU ~ PredE,
                                          TRUE ~ NA_real_),
                                        CandidateMark=round2(100*(maxgrade+((cumperc-rankperc)/(cumperc-cumpercplus1))),10),
                                        UnadjustedGrade=dplyr::recode(maxgrade, "6"="A*","5"="A","4"="B","3"="C", "2"="D", "1"="E", "0"="U")
#remove unneeded variables
      )%>%select(-c(totalcands, rankperc, maxgrade, cumperc, cumpercplus1,totalcands_centprevious,
                    totalcands_centcurrent, matchedcands_centcurrent, PredAStar, 
                    PredA, PredB, PredC, PredD, PredE, PredU))

#write output D11a
if (outputs==TRUE){
  candidatemarks2<-plyr::rbind.fill(candidatemarks, currentdata_private)
   write_csv(candidatemarks2, paste(AwardingOrganisation, QualificationLevel, "outputD11a.csv", sep="_"))   
}

return(candidatemarks)
}


############ Function to calculate grades based on imputed marks and target outcomes for matched candidates ############################

calculategradeboundaries<-function(candidatemarks, target, outputs=FALSE){
  
  #for each subject calculate cut scores (grade boundaries) to meet national distribution
  #create empty output data frame
  boundaries<-data.frame(SubjectGroup=character(),NMatchedCands=numeric(), MarkCutE=numeric(), MarkCutD=numeric(),MarkCutC=numeric(),MarkCutB=numeric(),MarkCutA=numeric(),MarkCutAStar=numeric(), stringsAsFactors = FALSE)
  
  #remove small subjects
  candidatemarks<-subset(candidatemarks, !SubjectGroup%in%AlevelSmallSubList)
  
  #create list of further maths candidates
  Fmathscands<-candidatemarks$UniqueCandidateIdentifier[candidatemarks$SubjectGroup=="Further Mathematics"]
  
  #filter to matched candidates and create percentile rank
  candidatemarks<-candidatemarks%>%
    filter(YearEndAge==18&!is.na(NormalisedMeanGCSE_England)&NGCSEsTaken>2&!is.na(CandidateMark)&Country=="England")%>%
    group_by(SubjectGroup)%>%
    arrange(desc(CandidateMark))%>%
    mutate(rank_noprivate=1:n(),
           totalcands=length(CentreAssessedGrade),
           rankprop=rank_noprivate/totalcands)%>%
    ungroup()
  
  #create list of subjects
  subnames<-unique(candidatemarks$SubjectGroup)
  
  #loop over each subject in list
  for(i in 1:length(subnames)){
    
    #subset data to subject
    markssub<-subset(candidatemarks,SubjectGroup== subnames[i])
    
    #if maths, remove further maths candidates and recalculate percentile rank order
    if(subnames[i]=="Mathematics"){
      markssub<-subset(markssub,!UniqueCandidateIdentifier%in%Fmathscands)
      markssub<-markssub%>%
        arrange(desc(CandidateMark))%>%
        mutate(rank_noprivate=1:n(),
               totalcands=length(CentreAssessedGrade),
               rankprop=rank_noprivate/totalcands)%>%
        ungroup()
      
         }
    
    #count matched candidates for output
    NMatched<-nrow(markssub)
    
    #find target distribution from predictions
    TargetDist<-target[target$Subject.Grouping==subnames[i],15:ncol(target)]/target$Total.Matched[target$Subject.Grouping==subnames[i]]
    
    #calculate boundaries as nearest candidate mark to target percentiles
    AStarbound<-round2(ifelse(TargetDist[1]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[1])))]),10)
    Abound<-round2(ifelse(TargetDist[2]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[2])))]),10)
    Bbound<-round2(ifelse(TargetDist[3]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[3])))]),10)
    Cbound<-round2(ifelse(TargetDist[4]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[4])))]),10)
    Dbound<-round2(ifelse(TargetDist[5]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[5])))]),10)
    Ebound<-round2(ifelse(TargetDist[6]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[6])))]),10)
    
    boundaries[nrow(boundaries)+1,]<-t(c(subnames[i],NMatched,Ebound,Dbound,Cbound,Bbound,Abound,AStarbound))
   
  }

  #Write output D12a
  if (outputs==TRUE){
    write_csv(boundaries, paste(AwardingOrganisation, QualificationLevel, "outputD12a.csv", sep="_"))  
    
  }
  
  return(boundaries)
}


############### Function to calculate grades based on marks #########################
#Calculate grades
Imputegradebasedonmark<-function(currentdata_private, candidatemarks, gradeboundaries, outputs=FALSE){
 
  #seperate out subjects not adjusted to meet predictions
  candidatemarks_smallsubs<-subset(candidatemarks, SubjectGroup%in%AlevelSmallSubList)
  candidatemarks_othersubs<-subset(candidatemarks, !SubjectGroup%in%AlevelSmallSubList)
  
#for each subject calculate cut scores (grade boundaries) to meet national distribution
#create empty output data frame
candidatemarks2<-NULL

#create list of subjects
subnames<-unique(candidatemarks_othersubs$SubjectGroup)

#loop over each subject in list
for(i in 1:length(subnames)){
  
  #subset data to subject
  markssub<-subset(candidatemarks_othersubs,SubjectGroup== subnames[i])
  cutpoints<-c(0, as.numeric(subset(gradeboundaries,SubjectGroup== subnames[i])[1,3:ncol(gradeboundaries)]),1001)
  
  #place candidates into groups based on cutpoints
  markssub$ImputedGrade<-.bincode(markssub$CandidateMark, breaks=as.numeric(cutpoints), include.lowest = TRUE, right=FALSE)
  
  #select grade labels from lookup table
  labels<-c("U", "E", "D", "C", "B", "A", "A*")
  
  #replace bin numbers with grade labels
  markssub$ImputedGrade<-dplyr::recode(markssub$ImputedGrade, "7"=labels[7], "6"=labels[6], "5"=labels[5], "4"=labels[4], "3"=labels[3], "2"=labels[2], "1"=labels[1])
  
  #add to output dataframe
  candidatemarks2<-rbind(candidatemarks2, markssub)

}

#give small subjects unadjusted marks and bind back to other data
candidatemarks_smallsubs$ImputedGrade<-candidatemarks_smallsubs$UnadjustedGrade
candidatemarks_all<-plyr::rbind.fill(candidatemarks2, candidatemarks_smallsubs)

#Write output D17
if (outputs==TRUE){
  candidatemarks3<-plyr::rbind.fill(candidatemarks_all, currentdata_private)
  write_csv(candidatemarks3, paste(AwardingOrganisation, QualificationLevel, "outputD17_initial.csv", sep="_")) 
}

return(candidatemarks_all)
}

################################################################################################



