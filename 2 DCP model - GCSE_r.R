###################################################################
##### Code to run the DCP approach - GCSE - updated 06/07/20 ######
###################################################################

#load libraries
library(tidyverse)

#set directory to save output
setwd("/path/to/folder")

#Define qualification level and awarding organisation for output
QualificationLevel<-"GCSE"
AwardingOrganisation<-"Ofqual"

#Run model and save output
GCSEmodeloutput<-GCSEDCPmodel(currentyear_nonprivate, yearminus1and2, outputs=TRUE)




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
GCSEDCPmodel<-function(currentdata, previousyear, outputs=FALSE){
  
  #### X2 - GENERATE PRIOR-ATTAINMENT CUT_OFFS
  #only keep 16 year old students for predictions with prior attainment data
  previousyearsub<-subset(previousyear, YearEndAge==16&!is.na(NormalisedKS2Score))
  
  #only one value per candidate to calculate cut scores
  previousyearunique<-previousyearsub[!duplicated(previousyearsub$UniqueCandidateIdentifier),]
  
  #create cut scores for deciles                         
  equalcut <- quantile(previousyearunique$NormalisedKS2Score, prob = seq(0, 1, length = 11),type=5, na.rm=TRUE)
  equalcut[1]<-0
  equalcut[11]<-101
  #round2 cut scores to 2 dp
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
  
  previousyearsub<-subset(previousyearsub, SubjectLevelGrade%in%c("9","8","7","6","5","4","3","2","1","U")&SubjectGroup!="Combined Science")
  
  ############ create prediction matrix using previous year/s #################################
  #### X3 - GENERATE SUBJECT-LEVEL HISTORICAL MATRICES
  
  #split data into deciles based on cut scores
  previousyearsub$equaldecile <- cut(previousyearsub$NormalisedKS2Score, breaks = as.numeric(equalcut), include.lowest=TRUE, labels=c(10,9,8,7,6,5,4,3,2,1), right=FALSE)
  
  #create outcome matrices
  predictionmatrix<-previousyearsub%>%group_by(SubjectGroup, equaldecile)%>%
    summarise(NcandsPM=length(UniqueCandidateIdentifier),
              PM9=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9")])/NcandsPM,
              PM8=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9", "8")])/NcandsPM,
              PM7=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9", "8", "7")])/NcandsPM,
              PM6=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9", "8", "7","6")])/NcandsPM,
              PM5=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5")])/NcandsPM,
              PM4=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5","4")])/NcandsPM,
              PM3=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5","4","3")])/NcandsPM,
              PM2=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5","4","3","2")])/NcandsPM,
              PM1=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5","4","3","2","1")])/NcandsPM,
              PMU=length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5","4","3","2","1","U")])/NcandsPM)%>%
    ungroup()
  
  #write output D25
  if (outputs==TRUE){
    outputD25<-left_join(predictionmatrix, qualtype, by="SubjectGroup")
    outputD25$QualificationLevel<-QualificationLevel
    outputD25<-outputD25%>%mutate(
      Grade9=round2(PM9*100,4),
      Grade8=round2(PM8*100,4),
      Grade7=round2(PM7*100,4),
      Grade6=round2(PM6*100,4),
      Grade5=round2(PM5*100,4),
      Grade4=round2(PM4*100,4),
      Grade3=round2(PM3*100,4),
      Grade2=round2(PM2*100,4),
      Grade1=round2(PM1*100,4)
    )%>%group_by(SubjectGroup)%>%
      mutate(NcandsPM=sum(NcandsPM))%>%
      ungroup%>%
      select(QualificationLevel, Type, SubjectGroup, NMatchHistoricalCandidates=NcandsPM,
             equaldecile, Grade9, Grade8, Grade7, Grade6, Grade5, Grade4, Grade3, Grade2, Grade1)

    write_csv(outputD25, paste(AwardingOrganisation, QualificationLevel, "outputD25.csv", sep="_"))   
  }
  
  ############# create predictions and outcomes for previous year/s ###########################
  #### X4 - GENERATE HISTORICAL PERFORMANCE RELATIVE TO PREDICTION
  previoussub<-subset(previousyear, SubjectLevelGrade%in%c("9","8","7","6","5","4","3","2","1","U")&SubjectGroup!="Combined Science")

  #remove mean GCSE for candidates not 16 to match for predictions
  previoussub$NormalisedKS2Score[previoussub$YearEndAge!=16]<-NA
  
  #split into deciles using same cut scores as above 
  previoussub$equaldecile <- cut(previoussub$NormalisedKS2Score, breaks = as.numeric(equalcut), include.lowest=TRUE, labels=c(10,9,8,7,6,5,4,3,2,1), right=FALSE)
  
   #merge in prediction for each candidate
  previoussub<-left_join(previoussub, predictionmatrix, by=c("SubjectGroup",  "equaldecile"))
  
  #create centre level data for previous year/s- entry numbers, match rate, outcomes at each grade, predictions at each grade (weighted mean).
  #### X1 - GENERATE CENTRE-LEVEL HISTORICAL GRADE DISTRIBUTIONS
  previouscent<-previoussub%>%
    group_by(SubjectGroup, CentreNo)%>%
    summarise(totalcands_centprevious=length(UniqueCandidateIdentifier),
              matchedcands_centprevious=length(UniqueCandidateIdentifier[!is.na(NormalisedKS2Score)]),      
              matchrate_centprevious=matchedcands_centprevious/totalcands_centprevious,
              Prop9_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9")]))/(totalcands_centprevious),
              Prop8_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8")]))/(totalcands_centprevious),
              Prop7_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7")]))/(totalcands_centprevious),
              Prop6_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6")]))/(totalcands_centprevious),
              Prop5_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5")]))/(totalcands_centprevious),
              Prop4_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5","4")]))/(totalcands_centprevious),
              Prop3_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5","4","3")]))/(totalcands_centprevious),
              Prop2_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5","4","3","2")]))/(totalcands_centprevious),
              Prop1_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5","4","3","2","1")]))/(totalcands_centprevious),
              PropU_centprevious=(length(UniqueCandidateIdentifier[SubjectLevelGrade%in%c("9","8","7","6","5","4","3","2","1","U")]))/(totalcands_centprevious),
              
              Pred9_centprevious=mean(PM9, na.rm = TRUE),
              Pred8_centprevious=mean(PM8, na.rm = TRUE),
              Pred7_centprevious=mean(PM7, na.rm = TRUE),
              Pred6_centprevious=mean(PM6, na.rm = TRUE),
              Pred5_centprevious=mean(PM5, na.rm = TRUE),
              Pred4_centprevious=mean(PM4, na.rm = TRUE),
              Pred3_centprevious=mean(PM3, na.rm = TRUE),
              Pred2_centprevious=mean(PM2, na.rm = TRUE),
              Pred1_centprevious=mean(PM1, na.rm = TRUE),
              PredU_centprevious=mean(PMU, na.rm = TRUE)                                                                  
    )
  
  #write output D4 
  if (outputs==TRUE){
    outputD4<-left_join(previouscent, qualtype, by="SubjectGroup")
    outputD4$QualificationLevel<-QualificationLevel
    outputD4$AwardingOrganisation<-AwardingOrganisation
    outputD4<-outputD4%>%mutate(ActualHist9=round2(Prop9_centprevious*100,2),
                                ActualHist8=round2(Prop8_centprevious*100,2),
                                ActualHist7=round2(Prop7_centprevious*100,2),
                                ActualHist6=round2(Prop6_centprevious*100,2),
                                ActualHist5=round2(Prop5_centprevious*100,2),
                                ActualHist4=round2(Prop4_centprevious*100,2),
                                ActualHist3=round2(Prop3_centprevious*100,2),
                                ActualHist2=round2(Prop2_centprevious*100,2),
                                ActualHist1=round2(Prop1_centprevious*100,2),
    )%>%
      select(QualificationLevel,Type, AwardingOrganisation,SubjectGroup, CentreNo,
             NHistoricalCandidates=totalcands_centprevious,
             NMatchHistoricalCandidates=matchedcands_centprevious,
             ActualHist9, ActualHist8, ActualHist7, ActualHist6, ActualHist5, ActualHist4, ActualHist3, ActualHist2, ActualHist1)
    
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
  
  #remove mean GCSE score for those not 16 to only produce predictions for matched candidates
  currentdatasub$NormalisedKS2Score[currentdatasub$YearEndAge!=16]<-NA
  
  #split candidates into deciles based on cut offs calculated above
  currentdatasub$equaldecile <- cut(currentdatasub$NormalisedKS2Score, breaks = as.numeric(equalcut), include.lowest=TRUE, labels=c(10,9,8,7,6,5,4,3,2,1), right=FALSE)
  
  #merge in candidate level predictions
  currentdatasub<-merge(currentdatasub, predictionmatrix, by=c("SubjectGroup", "equaldecile"), all.x=TRUE)
  
  #calculate centre level variables current - entry numbers, match rate, predictions
  currentcent<-currentdatasub%>%group_by(SubjectGroup, CentreNo)%>%summarise(totalcands_centcurrent=length(UniqueCandidateIdentifier),
                                                                                    matchedcands_centcurrent=length(UniqueCandidateIdentifier[!is.na(NormalisedKS2Score)]),      
                                                                                    matchrate_centcurrent=matchedcands_centcurrent/totalcands_centcurrent,
                                                                                    Pred9_centcurrent=mean(PM9, na.rm = TRUE),
                                                                                    Pred8_centcurrent=mean(PM8, na.rm = TRUE),
                                                                                    Pred7_centcurrent=mean(PM7, na.rm = TRUE),
                                                                                    Pred6_centcurrent=mean(PM6, na.rm = TRUE),
                                                                                    Pred5_centcurrent=mean(PM5, na.rm = TRUE),
                                                                                    Pred4_centcurrent=mean(PM4, na.rm = TRUE),
                                                                                    Pred3_centcurrent=mean(PM3, na.rm = TRUE),
                                                                                    Pred2_centcurrent=mean(PM2, na.rm = TRUE),
                                                                                    Pred1_centcurrent=mean(PM1, na.rm = TRUE),
                                                                                    PredU_centcurrent=mean(PMU, na.rm = TRUE)
  )
  
  #merge current and previous centre data
  Alldata<-left_join(currentcent, previouscent, by = c("SubjectGroup","CentreNo"))
  
  #### X6 - CALCULATE THE LOWEST PRIOR ATTAINMENT MATCH RATE FOR EACH CENTRE
  #find smallest matchrate between current and previous year
  Alldata$minmatch<-pmin(Alldata$matchrate_centprevious, Alldata$matchrate_centcurrent)
  
  #Replace NA's with 0 otherwise cant carry out calculations
  Alldata<-Alldata%>%mutate_at(vars(-c(SubjectGroup, CentreNo, matchrate_centcurrent, matchrate_centprevious, minmatch)), ~replace_na(., 0))
  
  #### X7 - ADJUST THE CENTRE LEVEL GRADE DISTRIBUTION
  #calculate prediction for current year
  Alldata<-Alldata%>%mutate(Pred9current=Prop9_centprevious+minmatch*(Pred9_centcurrent-Pred9_centprevious),
                            Pred8current=Prop8_centprevious+minmatch*(Pred8_centcurrent-Pred8_centprevious),
                            Pred7current=Prop7_centprevious+minmatch*(Pred7_centcurrent-Pred7_centprevious),
                            Pred6current=Prop6_centprevious+minmatch*(Pred6_centcurrent-Pred6_centprevious),
                            Pred5current=Prop5_centprevious+minmatch*(Pred5_centcurrent-Pred5_centprevious),
                            Pred4current=Prop4_centprevious+minmatch*(Pred4_centcurrent-Pred4_centprevious),
                            Pred3current=Prop3_centprevious+minmatch*(Pred3_centcurrent-Pred3_centprevious),
                            Pred2current=Prop2_centprevious+minmatch*(Pred2_centcurrent-Pred2_centprevious),
                            Pred1current=Prop1_centprevious+minmatch*(Pred1_centcurrent-Pred1_centprevious),
                            PredUcurrent=PropU_centprevious+minmatch*(PredU_centcurrent-PredU_centprevious)
  )
  
  ################ truncate values to be between 0 and 1 ###########################
  Alldata<-Alldata%>%group_by(SubjectGroup, CentreNo, totalcands_centcurrent, totalcands_centprevious, matchedcands_centcurrent)%>%
    summarise(Pred9=ifelse(Pred9current<0, 0, ifelse(Pred9current>1, 1, Pred9current)),
              Pred8=ifelse(Pred8current<0, 0, ifelse(Pred8current>1, 1, Pred8current)),
              Pred7=ifelse(Pred7current<0, 0, ifelse(Pred7current>1, 1, Pred7current)),
              Pred6=ifelse(Pred6current<0, 0, ifelse(Pred6current>1, 1, Pred6current)),
              Pred5=ifelse(Pred5current<0, 0, ifelse(Pred5current>1, 1, Pred5current)),
              Pred4=ifelse(Pred4current<0, 0, ifelse(Pred4current>1, 1, Pred4current)),
              Pred3=ifelse(Pred3current<0, 0, ifelse(Pred3current>1, 1, Pred3current)),
              Pred2=ifelse(Pred2current<0, 0, ifelse(Pred2current>1, 1, Pred2current)),
              Pred1=ifelse(Pred1current<0, 0, ifelse(Pred1current>1, 1, Pred1current)),
              PredU=ifelse(PredUcurrent<0, 0, ifelse(PredUcurrent>1, 1, PredUcurrent))
    )%>%ungroup()
  
  
  ############ correct for disordered cumulative frequencies by taking more generous when disordered #####################
  Alldata$Pred9<-Alldata$Pred9
  Alldata$Pred8<-ifelse(Alldata$Pred9>Alldata$Pred8, Alldata$Pred9, Alldata$Pred8)
  Alldata$Pred7<-ifelse(Alldata$Pred8>Alldata$Pred7, Alldata$Pred8, Alldata$Pred7)
  Alldata$Pred6<-ifelse(Alldata$Pred7>Alldata$Pred6, Alldata$Pred7, Alldata$Pred6)
  Alldata$Pred5<-ifelse(Alldata$Pred6>Alldata$Pred5, Alldata$Pred6, Alldata$Pred5)
  Alldata$Pred4<-ifelse(Alldata$Pred5>Alldata$Pred4, Alldata$Pred5, Alldata$Pred4)
  Alldata$Pred3<-ifelse(Alldata$Pred4>Alldata$Pred3, Alldata$Pred4, Alldata$Pred3)
  Alldata$Pred2<-ifelse(Alldata$Pred3>Alldata$Pred2, Alldata$Pred3, Alldata$Pred2)
  Alldata$Pred1<-ifelse(Alldata$Pred2>Alldata$Pred1, Alldata$Pred2, Alldata$Pred1)
  Alldata$PredU<-ifelse(Alldata$Pred1>Alldata$PredU, Alldata$Pred1, Alldata$PredU)
  
  
  #calculate harmonic mean between previous N cands and current N cands within centres
  Alldata$Harmoniccands<-((0.5/Alldata$totalcands_centcurrent)+(0.5/  Alldata$totalcands_centprevious))^(-1)
  
  #write output D6
  if (outputs==TRUE){
    outputD6<-left_join(Alldata, centretype, by="CentreNo")
    outputD6<-left_join(outputD6, qualtype, by="SubjectGroup")
    outputD6$QualificationLevel<-QualificationLevel
    outputD6$AwardingOrganisation<-AwardingOrganisation
    outputD6<-outputD6%>%mutate(Pred9=round2(Pred9*100,2),
                                Pred8=round2(Pred8*100,2),
                                Pred7=round2(Pred7*100,2),
                                Pred6=round2(Pred6*100,2),
                                Pred5=round2(Pred5*100,2),
                                Pred4=round2(Pred4*100,2),
                                Pred3=round2(Pred3*100,2),
                                Pred2=round2(Pred2*100,2),
                                Pred1=round2(Pred1*100,2),
    )%>%
      select(QualificationLevel,Type, AwardingOrganisation, SubjectGroup, CentreNo, CentreType,
             NCurrentCandidates=totalcands_centcurrent,
             NMatchedCandidates=matchedcands_centcurrent,
             Pred9,Pred8,Pred7,Pred6,Pred5,Pred4,Pred3,Pred2,Pred1)    
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