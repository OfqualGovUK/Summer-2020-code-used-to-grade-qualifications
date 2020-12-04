####################################################################################
##### Functions for allocating grades to candidates GCSE - - updated 22/06/20 ######
####################################################################################


#Read in outcome matrix
TargetDistributions<-read.csv("/path/to/file.csv", stringsAsFactors = FALSE)
TargetDistributions<-subset(TargetDistributions, Qualification.Level=="GCSE")

#include list of subjects which should not be adjusted to meet predictions
GCSESmallSubList<-TargetDistributions$Subject.Grouping[TargetDistributions$Total.Matched<500]


#set directory to save output
setwd("/path/to/folder")

# 1. run model to impute marks
currentyear_nonprivate_imputedmarks<-Imputemarks(currentyear_private, currentyear_nonprivate, GCSEmodeloutput, outputs=TRUE)

# 2. calculate grade boundaries based on marks and required cumulative frequencies except subjects in small subject list 
GCSEgradeboundaries<-calculategradeboundaries(currentyear_nonprivate_imputedmarks, TargetDistributions, outputs=TRUE)

# 3. impute grades based on marks and grade boundaries except subjects in small subject list 
currentyear_nonprivate_imputedgrades<-Imputegradebasedonmark(currentyear_private, currentyear_nonprivate_imputedmarks,GCSEgradeboundaries, outputs=FALSE)



######################################### FUNCTIONS - RUN ALL BELOW FIRST ##################################################  

############### Function for imputing marks ######################################
Imputemarks<-function(currentdata_private, currentdata, modeloutput, outputs=FALSE){
  
  #join model output to candidate level data
  candidatemarks<-left_join(currentdata, modeloutput, by = c("SubjectGroup", "CentreNo"))
  
  #create single rank order across grades excluding private candidates for allocating marks
  candidatemarks<-candidatemarks%>%group_by(SubjectGroup, CentreNo)%>%
    arrange(factor(CentreAssessedGrade, levels=c("9", "8", "7", "6", "5", "4","3","2","1", "U")), CentreRankOrder)%>%
    mutate(rank_noprivate=1:n(),
           totalcands=length(CentreAssessedGrade))%>%
    ungroup()
  
  #assign marks to candidates based on rank order and centre predicted outcomes         
  candidatemarks<-candidatemarks%>%mutate(rankperc=(rank_noprivate-0.5)/totalcands,
                                          maxgrade = case_when(
                                            rankperc <= Pred9 ~ 9, 
                                            rankperc <= Pred8 ~ 8,
                                            rankperc <= Pred7 ~ 7,
                                            rankperc <= Pred6 ~ 6,
                                            rankperc <= Pred5 ~ 5,
                                            rankperc <= Pred4 ~ 4,
                                            rankperc <= Pred3 ~ 3,
                                            rankperc <= Pred2 ~ 2,
                                            rankperc <= Pred1 ~ 1,
                                            rankperc <= PredU ~ 0,
                                            TRUE ~ NA_real_), 
                                          cumperc = case_when(
                                            rankperc <= Pred9 ~ Pred9, 
                                            rankperc <= Pred8 ~ Pred8,
                                            rankperc <= Pred7 ~ Pred7,
                                            rankperc <= Pred6 ~ Pred6,
                                            rankperc <= Pred5 ~ Pred5,
                                            rankperc <= Pred4 ~ Pred4,
                                            rankperc <= Pred3 ~ Pred3,
                                            rankperc <= Pred2 ~ Pred2,
                                            rankperc <= Pred1 ~ Pred1,
                                            rankperc <= PredU ~ PredU,
                                            TRUE ~ NA_real_), 
                                          cumpercplus1 = case_when(
                                            rankperc <= Pred9 ~ 0,
                                            rankperc <= Pred8 ~ Pred9,
                                            rankperc <= Pred7 ~ Pred8,
                                            rankperc <= Pred6 ~ Pred7,
                                            rankperc <= Pred5 ~ Pred6,
                                            rankperc <= Pred4 ~ Pred5,
                                            rankperc <= Pred3 ~ Pred4,
                                            rankperc <= Pred2 ~ Pred3,
                                            rankperc <= Pred1 ~ Pred2,
                                            rankperc <= PredU ~ Pred1,
                                            TRUE ~ NA_real_),
                                          CandidateMark=100*(maxgrade+((cumperc-rankperc)/(cumperc-cumpercplus1))),
                                          UnadjustedGrade=dplyr::recode(maxgrade, "9"="9","8"="8","7"="7","6"="6", "5"="5", "4"="4","3"="3","2"="2","1"="1", "0"="U")
                                          #remove unneeded variables
  )%>%select(-c(totalcands, rankperc, maxgrade, cumperc, cumpercplus1,totalcands_centprevious,
                totalcands_centcurrent, matchedcands_centcurrent, Pred9, 
                Pred8, Pred7, Pred6, Pred5, Pred4,Pred3,Pred2,Pred1, PredU))
  
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
  boundaries<-data.frame(SubjectGroup=character(),NMatchedCands=numeric(), MarkCut1=numeric(), MarkCut2=numeric(),MarkCut3=numeric(),MarkCut4=numeric(),MarkCut5=numeric(),MarkCut6=numeric(),MarkCut7=numeric(),MarkCut8=numeric(),MarkCut9=numeric(), stringsAsFactors = FALSE)
  
  candidatemarks<-subset(candidatemarks, !SubjectGroup%in%GCSESmallSubList)
  
  candidatemarks<-candidatemarks%>%
    filter(YearEndAge==16&!is.na(NormalisedKS2Score)&!is.na(CandidateMark)&!CentreType%in%c(2,5,7)&Country=="England")%>%
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
    
    NMatched<-nrow(markssub)
    
    TargetDist<-target[target$Subject.Grouping==subnames[i],15:ncol(target)]/target$Total.Matched[target$Subject.Grouping==subnames[i]]
    
    g9bound<-round2(ifelse(TargetDist[1]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[1])))]),10)
    g8bound<-round2(ifelse(TargetDist[2]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[2])))]),10)
    g7bound<-round2(ifelse(TargetDist[3]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[3])))]),10)
    g6bound<-round2(ifelse(TargetDist[4]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[4])))]),10)
    g5bound<-round2(ifelse(TargetDist[5]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[5])))]),10)
    g4bound<-round2(ifelse(TargetDist[6]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[6])))]),10)
    g3bound<-round2(ifelse(TargetDist[7]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[7])))]),10)
    g2bound<-round2(ifelse(TargetDist[8]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[8])))]),10)
    g1bound<-round2(ifelse(TargetDist[9]==0,1001,markssub$CandidateMark[which.min(abs(markssub$rankprop-as.numeric(TargetDist[9])))]),10)
    
    boundaries[nrow(boundaries)+1,]<-t(c(subnames[i],NMatched,g1bound,g2bound,g3bound,g4bound,g5bound,g6bound,g7bound,g8bound,g9bound))
    
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
  candidatemarks_smallsubs<-subset(candidatemarks, SubjectGroup%in%GCSESmallSubList)
  candidatemarks_othersubs<-subset(candidatemarks, !SubjectGroup%in%GCSESmallSubList)
  
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
    labels<-c("U", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    
    #replace bin numbers with grade labels
    markssub$ImputedGrade<-dplyr::recode(markssub$ImputedGrade, "10"=labels[10],"9"=labels[9],"8"=labels[8],"7"=labels[7], "6"=labels[6], "5"=labels[5], "4"=labels[4], "3"=labels[3], "2"=labels[2], "1"=labels[1])
    
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

