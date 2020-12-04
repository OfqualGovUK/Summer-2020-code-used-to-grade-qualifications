#########################################################################################################
##### Code for assigning grades to small centres and private candidates A level - updated 06/07/20 ######
#########################################################################################################

#load libraries
library(zoo)

#Small centres thresholds
Nthresh<-5
Nsmall<-15

#run small centre function
currentyear_nonprivate_imputedgrades_small<-Imputegrades_smallcentres(currentyear_nonprivate_imputedgrades, outputs=FALSE)

#run private cand function
currentyear_imputedgrades_private<-Imputegrades_privatecentres(currentyear_private, currentyear_nonprivate_imputedgrades_small, outputs=TRUE)



######################## FUNCTIONS - RUN FIRST ###################################################

#small centre function
Imputegrades_smallcentres<-function(currentdata, outputs=FALSE){
  
#split small and not small centres based on harmonic mean candidates
 smallcentres<-subset(currentdata, Harmoniccands<=Nsmall) 
 notsmallcentres<-subset(currentdata, Harmoniccands>Nsmall) 
 
 #calculate centre level distributions based on CAGs and current imputed grades
 smallCAGs<-smallcentres%>%group_by(SubjectGroup, CentreNo)%>%
   summarise(totalcands=length(CentreAssessedGrade),
             Harmoniccands=max(Harmoniccands),
             ImputedAStar=length(UniqueCandidateIdentifier[ImputedGrade%in%c("A*")])/totalcands,
             ImputedA=length(UniqueCandidateIdentifier[ImputedGrade%in%c("A*", "A")])/totalcands,
             ImputedB=length(UniqueCandidateIdentifier[ImputedGrade%in%c("A*", "A", "B")])/totalcands,
             ImputedC=length(UniqueCandidateIdentifier[ImputedGrade%in%c("A*","A","B","C")])/totalcands,
             ImputedD=length(UniqueCandidateIdentifier[ImputedGrade%in%c("A*","A","B","C","D")])/totalcands,
             ImputedE=length(UniqueCandidateIdentifier[ImputedGrade%in%c("A*","A","B","C","D","E")])/totalcands,
             ImputedU=length(UniqueCandidateIdentifier[ImputedGrade%in%c("A*","A","B","C","D","E","U")])/totalcands,
             CAGAStar=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("A*")])/totalcands,
             CAGA=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("A*", "A")])/totalcands,
             CAGB=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("A*", "A", "B")])/totalcands,
             CAGC=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("A*","A","B","C")])/totalcands,
             CAGD=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("A*","A","B","C","D")])/totalcands,
             CAGE=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("A*","A","B","C","D","E")])/totalcands,
             CAGU=length(UniqueCandidateIdentifier[CentreAssessedGrade%in%c("A*","A","B","C","D","E","U")])/totalcands,
             Threshmod=ifelse(Harmoniccands<=Nthresh, 1, (Nsmall-Harmoniccands)/(Nsmall-Nthresh))
   )%>%ungroup()

  #calculate adjusted distributions for small centres
 smallmodel<-smallCAGs %>%group_by(SubjectGroup, CentreNo, totalcands)%>% 
   summarise(PredAStarsmall=(Threshmod*CAGAStar)+((1-Threshmod)*(ImputedAStar)),
               PredAsmall=(Threshmod*CAGA)+((1-Threshmod)*(ImputedA)),
               PredBsmall=(Threshmod*CAGB)+((1-Threshmod)*(ImputedB)),
               PredCsmall=(Threshmod*CAGC)+((1-Threshmod)*(ImputedC)),
               PredDsmall=(Threshmod*CAGD)+((1-Threshmod)*(ImputedD)),
               PredEsmall=(Threshmod*CAGE)+((1-Threshmod)*(ImputedE)),
               PredUsmall=(Threshmod*CAGU)+((1-Threshmod)*(ImputedU))
   )%>%ungroup()
 
 #merge with candidate data for small centres
 smalldata<-left_join(smallcentres, smallmodel, by = c("SubjectGroup", "CentreNo"))
 
  #assign grades to candidates based on rank order and adjusted centre predicted outcomes         
 smalldata<-smalldata%>%mutate(rankperc=(rank_noprivate-0.5)/totalcands,
                                          maxgrade = case_when(
                                            rankperc <= PredAStarsmall ~ 6, 
                                            rankperc <= PredAsmall ~ 5,
                                            rankperc <= PredBsmall ~ 4,
                                            rankperc <= PredCsmall ~ 3,
                                            rankperc <= PredDsmall ~ 2,
                                            rankperc <= PredEsmall ~ 1,
                                            rankperc <= PredUsmall ~ 0,
                                            TRUE ~ NA_real_), 
                                          ImputedGrade=dplyr::recode(maxgrade, "6"="A*","5"="A","4"="B","3"="C", "2"="D", "1"="E", "0"="U")
 )

  #bind back together all centres 
 alldata<-plyr::rbind.fill(smalldata, notsmallcentres)

 #remove variables                                        
 alldata<-alldata%>%select(-c(totalcands, rankperc, maxgrade, Harmoniccands,
               PredAStarsmall,PredAsmall,PredBsmall,PredCsmall,PredDsmall,PredEsmall,PredUsmall,
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
  arrange(factor(CentreAssessedGrade, levels=c("A*", "A", "B", "C", "D", "E","U")), CentreRankOrder)%>%
  mutate(fullrank=1:n())%>%
  ungroup()

#create numeric imputed grades an CAGs
allcanddata$NumericImputed<-as.numeric(dplyr::recode(allcanddata$ImputedGrade, "A*"=6,"A"=5,"B"=4,"C"=3, "D"=2, "E"=1, "U"=0))
allcanddata$NumericCAG<-as.numeric(dplyr::recode(allcanddata$CentreAssessedGrade, "A*"=6,"A"=5,"B"=4,"C"=3, "D"=2, "E"=1, "U"=0))

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
    ImputedGrade=dplyr::recode(Privategrade, `6`="A*",`5`="A",`4`="B",`3`="C", `2`="D", `1`="E", `0`="U"))%>%
  select(-c(NumericImputed, NumericCAG, Nextvalue, Previousvalue, Privategrade, fullrank))

#output D17 with private candidates
if (outputs==TRUE){
  write_csv(allcanddata, paste(AwardingOrganisation, QualificationLevel, "outputD17_final.csv", sep="_"))   
}

return(allcanddata)
}



