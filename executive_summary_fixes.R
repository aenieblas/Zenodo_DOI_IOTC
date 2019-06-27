# meeting_report_fixes.R

executive_summary_fixes<-function(this_meeting){
  # define zenodo doc type
  this_meeting$Type<-'report'
  
  # reassign authors to iotc secretariat
  this_meeting[which(this_meeting$Authors=='Anonymous'),'Authors']<-'IOTC Secretariat'
  this_meeting[which(this_meeting$Authors==''),'Authors']<-'IOTC Secretariat'
  this_meeting$All.taxonomy.terms<-gsub('Anonymous','IOTC Secretariat',this_meeting$All.taxonomy.terms)
  
  # standardise the reference names with engligh AND french
  this_meeting[grep('\\+',this_meeting$IRD_reference),'IRD_reference']<-gsub('\\+','',this_meeting[grep('\\+',this_meeting$Reference),'IRD_reference'])
  this_meeting[,'IRD_reference']<-gsub('\\&','',this_meeting[,'IRD_reference'])
  this_meeting$Title<-gsub('\\[E\\+F\\] ','',this_meeting$Title)
  
  ## capitalisation
  this_meeting[which(this_meeting$Language=='English'),'Title']<-toTitleCase(this_meeting[which(this_meeting$Language=='English'),'Title'])
  
  ### !!!!!! <COMMENTED OUT> STILL TO BE AGREED UPON WITH IOTC!!!!!
  # ## IOTC-2014-SC17-ES05 : Wrong title to doc 
  # this_meeting[which(this_meeting$Reference=='IOTC-2014-SC17-ES05'),'Title'] <-'Report on Biology, Stock Status and Management of Southern Bluefin Tuna: 2014' 
  # 
  # ## IOTC-2014-SC17-ES05 : Wrong title to doc 
  # this_meeting[which(this_meeting$Reference=='IOTC-2013-SC16-ES05'),'Title'] <-'Report on Biology, Stock Status and Management of Southern Bluefin Tuna: 2013' 
  
  
  ### metadata adjustments.
  ## IOTC-2017-SC19-ES13 : French metadata title, English file
  this_meeting[which(this_meeting$Reference=='IOTC-2016-SC19-ES13' & this_meeting$Language=='French'),'File']<-'https://iotc.org/sites/default/files/documents/2019/06/IOTC-2016-SC19-ES13F_-_marlin_bleu.pdf'
  this_meeting[which(this_meeting$Reference=='IOTC-2014-SC17-ES11'),'Abstract']<-''
  # IOTC-2009-SC-03E
  this_meeting[which(this_meeting$Reference=='IOTC-2009-SC-03[E]'),'Title']<-gsub('\\. IOTC Secretariat','',this_meeting[which(this_meeting$Reference=='IOTC-2009-SC-03[E]'),'Title'])
  # IOTC-2008-SC-06[E+F]-rev1
  this_meeting[which(this_meeting$Reference=='IOTC-2008-SC-06[E+F]-rev1'),'Title']<-gsub('\\. IOTC Secretariat','',this_meeting[which(this_meeting$Reference=='IOTC-2008-SC-06[E+F]-rev1'),'Title'])
  this_meeting[which(this_meeting$Reference=='IOTC-2008-SC-06[E+F]-rev1'),'Title']<-gsub('/ Secrétariat De La CTOI','',this_meeting[which(this_meeting$Reference=='IOTC-2008-SC-06[E+F]-rev1'),'Title'])
  
  # renaming titles to a 'standard' format for titles without 'Executive Summary' already l
  es_pattern<-c('Executive','cutif')
  es_i<-grepl(paste(es_pattern,collapse='|'),this_meeting$Title,ignore.case = T)
  no_es<-which(es_i==FALSE)
  for(e in 1:length(no_es)){
    if(this_meeting[no_es[e],'Language']=='French'){this_meeting[no_es[e],'Title']<-paste0('Résumé Exécutif : ',this_meeting[no_es[e],'Title'])  }
    
    if(this_meeting[no_es[e],'Language']=='English'){this_meeting[no_es[e],'Title']<-paste0('Executive Summary : ',this_meeting[no_es[e],'Title']) }
  }
  
  ## remove draft from the titles of executive summaries
  this_meeting[,'Title']<-gsub('Draft: ','',this_meeting$Title,ignore.case=T)
  this_meeting[,'Title']<-gsub('Draft ','',this_meeting$Title,ignore.case=T)
  this_meeting[,'Title']<-gsub('_Draft','',this_meeting$Title,ignore.case=T)
  
  return(this_meeting)
}