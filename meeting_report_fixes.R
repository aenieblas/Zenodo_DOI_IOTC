# meeting_report_fixes.R

meeting_report_fixes<-function(this_meeting){
  this_meeting$Type<-'report'
  
  this_meeting$All.taxonomy.terms<-gsub('Anonymous','IOTC Secretariat',this_meeting$All.taxonomy.terms)
  
  ## rename meeting for IOTC-2017-WPTT19-R[FR]
  this_meeting[which(this_meeting$Reference== "IOTC-2017-WPTT19-R" & this_meeting$Language=='French'),'Meeting']<-'Working Party on Tropical Tunas (WPTT)'
  
  this_meeting$Authors<-'IOTC Secretariat'
  ## rename reference names
  ## IOTC-06-01-R
  this_meeting[which(this_meeting$IRD_reference=='IOTC-06-01-R'),'IRD_reference']<-'IOTC-S-06-01-RF'
  this_meeting[which(this_meeting$IRD_reference=='IOTC-S-06-01R'),'IRD_reference']<-'IOTC-S-06-01-RE'
  # IOTC-CS-02-05# 
  this_meeting[which(this_meeting$IRD_reference=='IOTC-CS-02-05'),'IRD_reference']<-'IOTC-SC-02-05-RF'
  this_meeting[which(this_meeting$IRD_reference=='IOTC-SC-02-05'),'IRD_reference']<-'IOTC-SC-02-05-RE'
  # IOTC-S-08-03R
  this_meeting[which(this_meeting$IRD_reference=='IOTC-S-08-03R'),'IRD_reference']<-'IOTC-S-08-03-RE'
  this_meeting[which(this_meeting$IRD_reference=='IOTC-S-08-03-R'),'IRD_reference']<-'IOTC-S-08-03-RF'
  #IOTC-2005-WPTT-R
  this_meeting[which(this_meeting$IRD_reference=='IOTC-2005-WPTT-R'),'IRD_reference']<-'IOTC-2005-WPTT-RE'
  
  #IOTC-2015-WPTmT06-R - wrong year in the reference name?
  this_meeting[which(this_meeting$IRD_reference=='IOTC-2015-WPTmT06-R'),'IRD_reference']<-'IOTC-2016-WPTmT06-RF'
  this_meeting[which(this_meeting$IRD_reference=='IOTC-2016-WPTmT06-R'),'IRD_reference']<-'IOTC-2016-WPTmT06-RE'
  
  ## IOTC-2018-SC21-R - add lang identifier (why isn't this already done? there's one on the original... check language())
  this_meeting[which(this_meeting$IRD_reference=='IOTC-2018-SC21-R'),'IRD_reference']<-'IOTC-2018-SC21-RF'
  
  ## IOTC-2011-S15_R[F] - add lang identifier
  this_meeting[which(this_meeting$Reference=='IOTC-2011-S15_R[F]'),'IRD_reference']<-'IOTC-2011-S15-RF'
  
  ## CTOI-CS-01-07- add lang identifier
  this_meeting[which(this_meeting$Reference=='CTOI-CS-01-07'),'IRD_reference']<-'IOTC-CS-01-07-RF'
  
  # CTOI-CS-02-05[F] - add lang identifier
  this_meeting[which(this_meeting$Reference=='CTOI-CS-02-05[F]'),'IRD_reference']<-'IOTC-CS-02-05-RF'
  
  # CTOI-S7-02-06
  this_meeting[which(this_meeting$Reference=='CTOI-S7-02-06'),'IRD_reference']<-'IOTC-S7-02-06-RF'
  
  ## CTOI-CS-01-07
  this_meeting[which(this_meeting$Reference=='CTOI-CS-01-07'),'IRD_reference']<-'IOTC-CS-01-07-RF'
  
  # IOTC-2014-WPM05-R Rev_1
  this_meeting[which(this_meeting$Reference=='IOTC-2014-WPM05-R Rev_1' & this_meeting$Language=='French'),'IRD_reference']<-'IOTC-2014-WPM05-RF_Rev1'
  this_meeting[which(this_meeting$Reference=='IOTC-2014-WPM05-R Rev_1' & this_meeting$Language=='English'),'IRD_reference']<-'IOTC-2014-WPM05-RE_Rev1'
  
  ## IOTC-2014-WPB12-RF - retitle - remove the colon
  this_meeting[which(this_meeting$Reference=='IOTC-2014-WPB12-R'),'Title']<-gsub(': ','',this_meeting[which(this_meeting$Reference=='IOTC-2014-WPB12-R'),'Title'])
  
  ## IOTC-2013-WPDCS09-RF - change path and file
  this_meeting[which(this_meeting$Reference=='IOTC-2013-WPDCS09-R' & this_meeting$Language=='French'),'File']<-'https://iotc.org/sites/default/files/documents/2019/06/IOTC-2013-WPDCS09-RF.pdf'
  this_meeting[which(this_meeting$Reference=='IOTC-2013-WPDCS09-R' & this_meeting$Language=='French'),'Path']<-'fr/documents/rapport-de-la-neuvi%C3%A8me-session-du-groupe-de-travail-de-la-ctoi-sur-la-collecte-des-donn%C3%A9es'
  
  ## File paths don't match. Fix manually.
  this_meeting[which(this_meeting$Reference=='IOTC-2004-WPT-R[EN]'),'File']<-'https://iotc.org/sites/default/files/documents/proceedings/2004/wpt/IOTC-2004-WPT-R%5BEN%5D.pdf'
  
  this_meeting[which(this_meeting$Reference=='IOTC-2010-WPDCS-R[F]'),'File']<-'https://iotc.org/sites/default/files/documents/2019/06/IOTC-2010-WPDCS07-RF.pdf'
  
  # this_meeting[which(this_meeting$Reference=='FIPL/R564(Bi)' & this_meeting$Language=='French'),'File']<-'https://iotc.org/sites/default/files/documents/proceedings/1997/s/IOTC-1997-S02-R%5BEN%2BFR%5D.pdf'
  this_meeting[which(this_meeting$Reference=='FIPL/R564(Bi)' & this_meeting$Language=='English'),'IRD_reference']<-'FIPL-R564(Bi)EF'
  this_meeting[which(this_meeting$Reference=='FIPL/R551(Bi)' & this_meeting$Language=='English'),'IRD_reference']<-'FIPL-R551(Bi)EF'
  
  this_meeting[which(this_meeting$Reference=='IOTC-2010-WPDCS-R[F]'),'File']<-'https://iotc.org/sites/default/files/documents/2019/06/IOTC-2010-WPDCS07-RF.pdf'
  
  return(this_meeting)
}