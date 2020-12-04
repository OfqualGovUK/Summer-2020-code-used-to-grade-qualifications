################################################################################################
##### Code to run the DCP approach - A level - updated 06/07/20 ######
###############################################################################################

#load libraries
library(tidyverse)

#set directory to save output
setwd("/path/to/folder")

#Define qualification level and awarding organisation for output
QualificationLevel<-"A Level"
AwardingOrganisation<-"National"

#Run model and save output
Alevelmodeloutput<-AlevelDCPmodel(currentyear_nonprivate, yearminus1and2and3, outputs=TRUE)




################ FUNCTIONS - RUN BELOW FIRST ########################################
#Standard rounding function
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

#DCP model function
AlevelDCPmodel<-function(currentdata, previousyear, outputs=FALSE){
  
  #### X2 - GENERATE PRIOR-ATTAINMENT CUT_OFFS
  #only keep 18 year old students for predictions with at least 3 GCSEs and prior attainment data
  previousyearsub<-subset(previousyear, NGCSEsTaken>2&YearEndAge==18&!is.na(NormalisedMeanGCSE_UK))
  
  #only one value per candidate to calculate cut scores
  previousyearunique<-previousyearsub[!duplicated(previousyearsub$UniqueCandidateIdentifier),]
  
  #create cut scores for deciles                         
  equalcut <- quantile(previousyearunique$NormalisedMeanGCSE_UK, prob = seq(0, 1, length = 11),type=5, na.rm=TRUE)
  equalcut[1]<-0
  equalcut[11]<-101
#  round cut scores to 2 dp
 equalcut<-round2(equalcut,2)
  
  #write output D24
  if (outputs==TRUE){
    outputD24<-NULL
    outputD24$QualificationLevel<-QualificationLevel
    outputD24$PriorCut1<-equalcut[10]
    outputD24$PriorCut2<-equalcut[9]
    outputD24$PriorCut3<-equalcut[8]
    outputD24$PriorCut4<-equalcut[7]
    outputD24$PriorCut5<-equalcut[6]
    outputD24$PriorCut6<-equalcut[5]
    outputD24$PriorCut7<-equalcut[4]
    outputD24$PriorCut8<-equalcut[3]
    outputD24$PriorCut9<-equalcut[2]
    outputD24<-as.data.frame(outputD24)
    
  write_csv(outputD24, paste(AwardingOrganisation, QualificationLevel, "outputD24.csv", sep="_"))   
  }
  
  
  ############ create prediction matrix using previous year/s #################################
  #### X3 - GENERATE SUBJECT-LEVEL HISTORICAL MATRICES

  #split data into deciles based on cut scores
  previousyearsub$equaldecile <- cut(previousyearsub$NormalisedMeanGCSE_UK, breaks = as.numeric(equalcut), include.lowest=TRUE, labels=c(10,9,8,7,6,5,4,3,2,1), right=FALSE)
  
  #create outcome matrices
  predictionmatrix<-previousyearsub%>%group_by(SubjectGroup, equaldecile)%>%
    summarise(NcandsPM=length(UniqueCandidateIdentifier),
              PMAStar=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*")])/NcandsPM,
              PMA=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*", "A")])/NcandsPM,
              PMB=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*", "A", "B")])/NcandsPM,
              PMC=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*","A","B","C")])/NcandsPM,
              PMD=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*","A","B","C","D")])/NcandsPM,
              PME=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*","A","B","C","D","E")])/NcandsPM,
              PMU=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*","A","B","C","D","E","U")])/NcandsPM)%>%
    ungroup()
  
#write output D25
  if (outputs==TRUE){
    outputD25<-left_join(predictionmatrix, qualtype, by="SubjectGroup")
    outputD25$QualificationLevel<-QualificationLevel
    outputD25<-outputD25%>%mutate(
      GradeAStar=round2(PMAStar*100,4),
      GradeA=round2(PMA*100,4),
      GradeB=round2(PMB*100,4),
      GradeC=round2(PMC*100,4),
      GradeD=round2(PMD*100,4),
      GradeE=round2(PME*100,4)
    )%>%group_by(SubjectGroup)%>%
      mutate(NcandsPM=sum(NcandsPM))%>%
      ungroup%>%
      select(QualificationLevel, Type, SubjectGroup, NMatchHistoricalCandidates=NcandsPM,
             Decile=equaldecile, GradeAStar, GradeA, GradeB, GradeC, GradeD, GradeE)
    
    write_csv(outputD25, paste(AwardingOrganisation, QualificationLevel, "outputD25.csv", sep="_"))   
  }
  
  ############# create predictions and outcomes for previous year/s ###########################
  #### X4 - GENERATE HISTORICAL PERFORMANCE RELATIVE TO PREDICTION
  previoussub<-previousyear
  
  #remove mean GCSE for candidates with less than 3 GCSEs and not 18 to match for predictions
  previoussub$NormalisedMeanGCSE_UK[previoussub$NGCSEsTaken<3|previoussub$YearEndAge!=18]<-NA
  
  #split into deciles using same cut scores as above 
  previoussub$equaldecile <- cut(previoussub$NormalisedMeanGCSE_UK, breaks = as.numeric(equalcut), include.lowest=TRUE, labels=c(10,9,8,7,6,5,4,3,2,1), right=FALSE)
  
    #merge in prediction for each candidate
  previoussub<-left_join(previoussub, predictionmatrix, by=c("SubjectGroup", "equaldecile"))
  
    #create centre level data for previous year/s- entry numbers, match rate, outcomes at each grade, predictions at each grade (weighted mean).
  #### X1 - GENERATE CENTRE-LEVEL HISTORICAL GRADE DISTRIBUTIONS
  previouscent<-previoussub%>%
    group_by(SubjectGroup, CentreNo)%>%
    summarise(totalcands_centprevious=length(UniqueCandidateIdentifier),
              matchedcands_centprevious=length(UniqueCandidateIdentifier[!is.na(NormalisedMeanGCSE_UK)]),      
              matchrate_centprevious=matchedcands_centprevious/totalcands_centprevious,
              PropAStar_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*")]))/(totalcands_centprevious),
              PropA_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*", "A")]))/(totalcands_centprevious),
              PropB_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*", "A", "B")]))/(totalcands_centprevious),
              PropC_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*","A","B","C")]))/(totalcands_centprevious),
              PropD_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*","A","B","C","D")]))/(totalcands_centprevious),
              PropE_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*","A","B","C","D","E")]))/(totalcands_centprevious),
              PropU_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("A*","A","B","C","D","E","U")]))/(totalcands_centprevious),
              PredAStar_centprevious=mean(PMAStar, na.rm = TRUE),
              PredA_centprevious=mean(PMA, na.rm = TRUE),
              PredB_centprevious=mean(PMB, na.rm = TRUE),
              PredC_centprevious=mean(PMC, na.rm = TRUE),
              PredD_centprevious=mean(PMD, na.rm = TRUE),
              PredE_centprevious=mean(PME, na.rm = TRUE),
              PredU_centprevious=mean(PMU, na.rm = TRUE)                                                                     
    )
 
  #write output D4 
  if (outputs==TRUE){
    outputD4<-left_join(previouscent, qualtype, by="SubjectGroup")
    outputD4$QualificationLevel<-QualificationLevel
    outputD4$AwardingOrganisation<-AwardingOrganisation
    outputD4<-outputD4%>%mutate(ActualHistStar=round2(PropAStar_centprevious*100,2),
                                ActualHistA=round2(PropA_centprevious*100,2),
                                ActualHistB=round2(PropB_centprevious*100,2),
                                ActualHistC=round2(PropC_centprevious*100,2),
                                ActualHistD=round2(PropD_centprevious*100,2),
                                ActualHistE=round2(PropE_centprevious*100,2))%>%
      select(QualificationLevel, Type, AwardingOrganisation,SubjectGroup, CentreNo,
             NHistoricalCandidates=totalcands_centprevious,
             NMatchHistoricalCandidates=matchedcands_centprevious,
             ActualHistStar,ActualHistA,ActualHistB, ActualHistC, ActualHistD, ActualHistE)
    
    write_csv(outputD4, paste(AwardingOrganisation, QualificationLevel,  "outputD4.csv", sep="_"))  
    
    # %>%
    #   mutate(Grouping=SubjectGroup)
    # outputD4%>%
    #   group_by(Grouping)%>%
    #   group_walk(~ write_csv(.x, paste(QualificationLevel, .y$Grouping, "outputD4.csv", sep="_")))   
  }
  
  ################# create predictions and outcomes for current year/s ###################################
  #### X5 - CALCULATE THE PREDICTED PERFORMANCE IN EACH CENTRE FOR THE CURRENT YEAR
  #create temporary dataset
  currentdatasub<-currentdata
  
  #remove mean GCSE score for those with less than 3 GCSEs and not 18 to only produce predictions for matched candidates
  currentdatasub$NormalisedMeanGCSE_UK[currentdatasub$NGCSEsTaken<3|currentdatasub$YearEndAge!=18]<-NA
  
  #split candidates into deciles based on cut offs calculated above
  currentdatasub$equaldecile <- cut(currentdatasub$NormalisedMeanGCSE_UK, breaks = as.numeric(equalcut), include.lowest=TRUE, labels=c(10,9,8,7,6,5,4,3,2,1), right=FALSE)
  
  #merge in candidate level predictions
  currentdatasub<-merge(currentdatasub, predictionmatrix, by=c("SubjectGroup", "equaldecile"), all.x=TRUE)
  
  #calculate centre level variables current - entry numbers, match rate, predictions
  currentcent<-currentdatasub%>%group_by(SubjectGroup,  CentreNo)%>%summarise(totalcands_centcurrent=length(UniqueCandidateIdentifier),
                                                                                matchedcands_centcurrent=length(UniqueCandidateIdentifier[!is.na(NormalisedMeanGCSE_UK)]),      
                                                                                matchrate_centcurrent=matchedcands_centcurrent/totalcands_centcurrent,
                                                                                PredAStar_centcurrent=mean(PMAStar, na.rm = TRUE),
                                                                                PredA_centcurrent=mean(PMA, na.rm = TRUE),
                                                                                PredB_centcurrent=mean(PMB, na.rm = TRUE),
                                                                                PredC_centcurrent=mean(PMC, na.rm = TRUE),
                                                                                PredD_centcurrent=mean(PMD, na.rm = TRUE),
                                                                                PredE_centcurrent=mean(PME, na.rm = TRUE),
                                                                                PredU_centcurrent=mean(PMU, na.rm = TRUE)
  )
  
  #merge current and previous centre data
  Alldata<-left_join(currentcent,previouscent, by = c("SubjectGroup","CentreNo"))
  
  #### X6 - CALCULATE THE LOWEST PRIOR ATTAINMENT MATCH RATE FOR EACH CENTRE
  #find smallest matchrate between current and previous year
  Alldata$minmatch<-pmin(Alldata$matchrate_centprevious, Alldata$matchrate_centcurrent)
  
  #Replace NA's with 0 otherwise cant carry out calculations
  Alldata<-Alldata%>%mutate_at(vars(-c(SubjectGroup, CentreNo, matchrate_centcurrent, matchrate_centprevious, minmatch)), ~replace_na(., 0))
  
  #### X7 - ADJUST THE CENTRE LEVEL GRADE DISTRIBUTION
  #calculate prediction for current year
  Alldata<-Alldata%>%mutate(PredAStarcurrent=PropAStar_centprevious+minmatch*(PredAStar_centcurrent-PredAStar_centprevious),
                            PredAcurrent=PropA_centprevious+minmatch*(PredA_centcurrent-PredA_centprevious),
                            PredBcurrent=PropB_centprevious+minmatch*(PredB_centcurrent-PredB_centprevious),
                            PredCcurrent=PropC_centprevious+minmatch*(PredC_centcurrent-PredC_centprevious),
                            PredDcurrent=PropD_centprevious+minmatch*(PredD_centcurrent-PredD_centprevious),
                            PredEcurrent=PropE_centprevious+minmatch*(PredE_centcurrent-PredE_centprevious),
                            PredUcurrent=PropU_centprevious+minmatch*(PredU_centcurrent-PredU_centprevious)
  )
  
  
  ################ truncate values to be between 0 and 1 ###########################
  Alldata<-Alldata%>%group_by(SubjectGroup, CentreNo, totalcands_centcurrent, totalcands_centprevious, matchedcands_centcurrent)%>%
                      summarise(PredAStar=ifelse(PredAStarcurrent<0, 0, ifelse(PredAStarcurrent>1, 1, PredAStarcurrent)),
                                PredA=ifelse(PredAcurrent<0, 0, ifelse(PredAcurrent>1, 1, PredAcurrent)),
                                PredB=ifelse(PredBcurrent<0, 0, ifelse(PredBcurrent>1, 1, PredBcurrent)),
                                PredC=ifelse(PredCcurrent<0, 0, ifelse(PredCcurrent>1, 1, PredCcurrent)),
                                PredD=ifelse(PredDcurrent<0, 0, ifelse(PredDcurrent>1, 1, PredDcurrent)),
                                PredE=ifelse(PredEcurrent<0, 0, ifelse(PredEcurrent>1, 1, PredEcurrent)),
                                PredU=ifelse(PredUcurrent<0, 0, ifelse(PredUcurrent>1, 1, PredUcurrent))
  )%>%ungroup()

  ############ correct for disordered cumulative frequencies by taking more generous when disordered #####################
  Alldata$PredAStar<-Alldata$PredAStar
  Alldata$PredA<-ifelse(Alldata$PredAStar>Alldata$PredA, Alldata$PredAStar, Alldata$PredA)
  Alldata$PredB<-ifelse(Alldata$PredA>Alldata$PredB, Alldata$PredA, Alldata$PredB)
  Alldata$PredC<-ifelse(Alldata$PredB>Alldata$PredC, Alldata$PredB, Alldata$PredC)
  Alldata$PredD<-ifelse(Alldata$PredC>Alldata$PredD, Alldata$PredC, Alldata$PredD)
  Alldata$PredE<-ifelse(Alldata$PredD>Alldata$PredE, Alldata$PredD, Alldata$PredE)
  Alldata$PredU<-ifelse(Alldata$PredE>Alldata$PredU, Alldata$PredE, Alldata$PredU)
  
  #calculate harmonic mean between previous N cands and current N cands within centres
  Alldata$Harmoniccands<-((0.5/Alldata$totalcands_centcurrent)+(0.5/  Alldata$totalcands_centprevious))^(-1)
  
  #write output D6
  if (outputs==TRUE){
    outputD6<-left_join(Alldata, centretype, by="CentreNo")
    outputD6<-left_join(outputD6, qualtype, by="SubjectGroup")
    outputD6$QualificationLevel<-QualificationLevel
    outputD6$AwardingOrganisation<-AwardingOrganisation
    outputD6<-outputD6%>%mutate(PredStar=round2(PredAStar*100,2),
                                PredA=round2(PredA*100,2),
                                PredB=round2(PredB*100,2),
                                PredC=round2(PredC*100,2),
                                PredD=round2(PredD*100,2),
                                PredE=round2(PredE*100,2))%>%
      select(QualificationLevel, Type, AwardingOrganisation, SubjectGroup, CentreNo, CentreType,
             NCurrentCandidates=totalcands_centcurrent,
             NMatchedCandidates=matchedcands_centcurrent,
             PredStar,PredA,PredB, PredC, PredD, PredE)
    write_csv(outputD6, paste(AwardingOrganisation, QualificationLevel, "outputD6.csv", sep="_"))
    
    # %>%
    #   mutate(Grouping=SubjectGroup)
    # outputD6%>%
    #   group_by(Grouping)%>%
    #   group_walk(~ write_csv(.x, paste(QualificationLevel, .y$Grouping, "outputD6.csv", sep="_")))   
  }
  
  return(Alldata)
}   

########################################################################################################################