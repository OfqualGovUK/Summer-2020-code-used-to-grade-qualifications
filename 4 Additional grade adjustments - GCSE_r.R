############################################################################################################################
##### Code for assigning grades to small centres, very large centres and private candidates - GCSE - updated 06/07/20 ######
############################################################################################################################

#load libraries
library(zoo)

#list of large centres
setwd("/path/to/folder")
Largecentrelist<-read.csv("/path/to/file.csv", stringsAsFactors=FALSE)
Largecentrelist<-Largecentrelist%>%select(SubjectGroup=OMTitle, CentreNo)
Largecentrelist$CentreNo<-as.character(Largecentrelist$CentreNo)

#Small centres thresholds
Nthresh<-5
Nsmall<-15

#set directory to save output
setwd("/path/to/folder")


#run small centre function
currentyear_nonprivate_imputedgrades_small<-Imputegrades_smallcentres(currentyear_nonprivate_imputedgrades, outputs=FALSE)

#run private cand function
currentyear_imputedgrades_private<-Imputegrades_privatecentres(currentyear_private, currentyear_nonprivate_imputedgrades_small, outputs=FALSE)

#run large centre function
currentyear_imputedgrades_large<-Imputegrades_largecentres(currentyear_imputedgrades_private, outputs=TRUE)




######################## FUNCTIONS - RUN FIRST ###################################################

################### small centre function #############################################
Imputegrades_smallcentres<-function(currentdata, modeloutput, outputs=FALSE){
  
  #split small and not small centres
  smallcentres<-subset(currentdata, Harmoniccands<=Nsmall) 
  notsmallcentres<-subset(currentdata, Harmoniccands>Nsmall) 
  
  #calculate centre level distributions based on CAGs and imputed grades
  smallCAGs<-smallcentres%>%group_by(SubjectGroup, CentreNo)%>%
    summarise(totalcands=length(CentreAssessedGrade),
              Harmoniccands=max(Harmoniccands),
              Imputed9=length(UniqueCandidateIdentifier[ImputedGrade%in%c("9")])/totalcands,
              Imputed8=length(UniqueCandidateIdentifier[ImputedGrade%in%c("9", "8")])/totalcands,
              Imputed7=length(UniqueCandidateIdentifier[ImputedGrade%in%c("9", "8", "7")])/totalcands,
              Imputed6=length(UniqueCandidateIdentifier[ImputedGrade%in%c("9", "8", "7","6")])/totalcands,
              Imputed5=length(UniqueCandidateIdentifier[ImputedGrade%in%c("9","8","7","6","5")])/totalcands,
              Imputed4=length(UniqueCandidateIdentifier[ImputedGrade%in%c("9","8","7","6","5","4")])/totalcands,
              Imputed3=length(UniqueCandidateIdentifier[ImputedGrade%in%c("9","8","7","6","5","4","3")])/totalcands,
              Imputed2=length(UniqueCandidateIdentifier[ImputedGrade%in%c("9","8","7","6","5","4","3","2")])/totalcands,
              Imputed1=length(UniqueCandidateIdentifier[ImputedGrade%in%c("9","8","7","6","5","4","3","2","1")])/totalcands,
              ImputedU=length(UniqueCandidateIdentifier[ImputedGrade%in%c("9","8","7","6","5","4","3","2","1","U")])/totalcands,
              CAG9=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("9")])/totalcands,
              CAG8=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("9", "8")])/totalcands,
              CAG7=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("9", "8", "7")])/totalcands,
              CAG6=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("9", "8", "7","6")])/totalcands,
              CAG5=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("9","8","7","6","5")])/totalcands,
              CAG4=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("9","8","7","6","5","4")])/totalcands,
              CAG3=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("9","8","7","6","5","4","3")])/totalcands,
              CAG2=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("9","8","7","6","5","4","3","2")])/totalcands,
              CAG1=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("9","8","7","6","5","4","3","2","1")])/totalcands,
              CAGU=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("9","8","7","6","5","4","3","2","1","U")])/totalcands,
              Threshmod=ifelse(Harmoniccands<=Nthresh, 1, (Nsmall-Harmoniccands)/(Nsmall-Nthresh))  
    )%>%ungroup()
  
  #calculate adjusted distributions for small centres
  smallmodel<-smallCAGs %>%group_by(SubjectGroup, CentreNo, totalcands)%>% 
    summarise(Pred9small=(Threshmod*CAG9)+((1-Threshmod)*(Imputed9)),
              Pred8small=(Threshmod*CAG8)+((1-Threshmod)*(Imputed8)),
              Pred7small=(Threshmod*CAG7)+((1-Threshmod)*(Imputed7)),
              Pred6small=(Threshmod*CAG6)+((1-Threshmod)*(Imputed6)),
              Pred5small=(Threshmod*CAG5)+((1-Threshmod)*(Imputed5)),
              Pred4small=(Threshmod*CAG4)+((1-Threshmod)*(Imputed4)),
              Pred3small=(Threshmod*CAG3)+((1-Threshmod)*(Imputed3)),
              Pred2small=(Threshmod*CAG2)+((1-Threshmod)*(Imputed2)),
              Pred1small=(Threshmod*CAG1)+((1-Threshmod)*(Imputed1)),
              PredUsmall=(Threshmod*CAGU)+((1-Threshmod)*(ImputedU))
    )%>%ungroup()
  
  
  smalldata<-left_join(smallcentres, smallmodel, by = c("SubjectGroup", "CentreNo"))
  
  #assign grades to candidates based on rank order and adjusted centre predicted outcomes         
  smalldata<-smalldata%>%mutate(rankperc=(rank_noprivate-0.5)/totalcands,
                                          maxgrade = case_when(
                                            rankperc <= Pred9small ~ 9, 
                                            rankperc <= Pred8small ~ 8,
                                            rankperc <= Pred7small ~ 7,
                                            rankperc <= Pred6small ~ 6,
                                            rankperc <= Pred5small ~ 5,
                                            rankperc <= Pred4small ~ 4,
                                            rankperc <= Pred3small ~ 3,
                                            rankperc <= Pred2small ~ 2,
                                            rankperc <= Pred1small ~ 1,
                                            rankperc <= PredUsmall ~ 0,
                                            TRUE ~ NA_real_), 
                                          ImputedGrade=dplyr::recode(maxgrade, "9"="9","8"="8","7"="7","6"="6", "5"="5", "4"="4","3"="3","2"="2","1"="1", "0"="U")
                                          #remove variables                                        
  )
  
  #bind back together all centres 
  alldata<-plyr::rbind.fill(smalldata, notsmallcentres)
  
  alldata<-alldata%>%select(-c(totalcands, rankperc, maxgrade, Harmoniccands,
                Pred9small,Pred8small,Pred7small,Pred6small,Pred5small,Pred4small,Pred3small,Pred2small,Pred1small,PredUsmall,
                rank_noprivate))
  
  #output D17 with small centre adjustment
  if (outputs==TRUE){
    write_csv(alldata, paste(AwardingOrganisation, QualificationLevel, "outputD17_small.csv", sep="_"))   
  }
  
  return(alldata)
}




########################## Private candidate function ########################################
Imputegrades_privatecentres<-function(currentdata_private, currentdata, outputs=FALSE){
  
  #bind in private candidate data
  allcanddata<-plyr::rbind.fill(currentdata, currentdata_private)
  
  #create rank order including private candidates
  allcanddata<-allcanddata%>%group_by(SubjectGroup, CentreNo)%>%
    arrange(factor(CentreAssessedGrade, levels=c("9", "8", "7", "6", "5", "4","3","2","1", "U")), CentreRankOrder)%>%
    mutate(fullrank=1:n())%>%
    ungroup()
  
  #create numeric imputed grades an CAGs
  allcanddata$NumericImputed<-as.numeric(dplyr::recode(allcanddata$ImputedGrade, "9"=9,"8"=8,"7"=7,"6"=6, "5"=5, "4"=4, "3"=3,"2"=2,"1"=1,"U"=0))
  allcanddata$NumericCAG<-as.numeric(dplyr::recode(allcanddata$CentreAssessedGrade, "9"=9,"8"=8,"7"=7,"6"=6, "5"=5, "4"=4, "3"=3,"2"=2,"1"=1,"U"=0))
  
  #impute grades for private candidates
  allcanddata<-allcanddata %>% group_by(SubjectGroup, CentreNo)%>%arrange(SubjectGroup, CentreNo, fullrank)%>%
    mutate(Nextvalue=na.locf0(NumericImputed, fromLast=TRUE),
           Previousvalue=na.locf0(NumericImputed),
           Privategrade = case_when(
             PrivateCandidate == 1 & Previousvalue==Nextvalue ~ Previousvalue,
             PrivateCandidate == 1 & Previousvalue!=Nextvalue & NumericCAG>=Previousvalue ~ Previousvalue,
             PrivateCandidate == 1 & Previousvalue!=Nextvalue & NumericCAG<=Nextvalue ~ Nextvalue,
             PrivateCandidate == 1 & Previousvalue!=Nextvalue & NumericCAG>Nextvalue & NumericCAG<Previousvalue ~ NumericCAG,
             PrivateCandidate == 1 & is.na(Previousvalue) & NumericCAG>=Nextvalue ~ NumericCAG,
             PrivateCandidate == 1 & is.na(Previousvalue) & NumericCAG<Nextvalue ~ Nextvalue,
             PrivateCandidate == 1 & is.na(Nextvalue) & NumericCAG<=Previousvalue ~ NumericCAG,
             PrivateCandidate == 1 & is.na(Nextvalue) & NumericCAG>Previousvalue ~ Previousvalue,
            PrivateCandidate == 1 & is.na(Nextvalue) & is.na(Previousvalue) ~ NumericCAG,
             TRUE ~ NumericImputed
           ))%>%ungroup()%>%mutate(
             ImputedGrade=dplyr::recode(Privategrade, `9`="9",`8`="8",`7`="7",`6`="6", `5`="5", `4`="4",`3`="3",`2`="2",`1`="1", `0`="U"))%>%
    select(-c(NumericImputed, NumericCAG, Nextvalue, Previousvalue, Privategrade, fullrank))
  
  #output D17 with private candidates
  if (outputs==TRUE){
    write_csv(allcanddata, paste(AwardingOrganisation, QualificationLevel, "outputD17_independent.csv", sep="_"))   
  }
  
  return(allcanddata)
}


########################### Large Centre function ##########################
Imputegrades_largecentres<-function(currentdata, outputs=FALSE){
  
  #select large centre
  largedata<-subset(currentdata, SubjectGroup%in%c("English Language", "Mathematics")&CentreAssessedGrade%in%c("4", "3", "2", "1"))
  largedata<-inner_join(largedata, Largecentrelist, by = c("SubjectGroup", "CentreNo"))
  
  #select other data
  otherdata<-anti_join(currentdata, largedata)
  
  #convert imputed grades to numeric
  largedata$NumericGrade<-as.numeric(dplyr::recode(largedata$ImputedGrade, "9"=9,"8"=8,"7"=7,"6"=6, "5"=5, "4"=4, "3"=3,"2"=2,"1"=1,"U"=0))
  
  #group students and assign max grade in group to all
  largedata<-largedata%>%group_by(SubjectGroup, CentreNo, CentreAssessedGrade)%>%
    mutate(tiedgroups=ceiling(CentreRankOrder/10))%>%
    group_by(SubjectGroup, CentreNo, CentreAssessedGrade,tiedgroups)%>%
    mutate(AssignedGrade=max(NumericGrade))
      
  #convert back to grades    
  largedata$ImputedGrade<-dplyr::recode(largedata$AssignedGrade, `9`="9",`8`="8",`7`="7",`6`="6", `5`="5", `4`="4",`3`="3",`2`="2",`1`="1", `0`="U")
  
  #bind back together all centres 
  alldata<-plyr::rbind.fill(largedata, otherdata)
  
  alldata<-alldata%>%select(-c(NumericGrade, AssignedGrade, tiedgroups))
  
  #output D17 with large centre adjustment
  if (outputs==TRUE){
    write_csv(alldata, paste(AwardingOrganisation, QualificationLevel, "outputD17_final.csv", sep="_"))   
  }
  return(alldata)
  
}
