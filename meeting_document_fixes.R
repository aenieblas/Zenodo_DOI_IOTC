# meeting_report_fixes.R

executive_summary_fixes<-function(this_meeting){
  # define zenodo doc type
  this_meeting$Type<-'workingpaper'
  ## retitle : 
  this_meeting[which(this_meeting$Title=='FINAL'),'Title']<-'Report of the Tenth Session of the IOTC Working Party on Billfish' ## AE: <------ this can be moved, no?
  
  ## 2 diff docs with same reference. ref WPB16-22 was derived from the file
  this_meeting[which(this_meeting$Reference=='IOTC-2018-WPB16-09' & this_meeting$Authors=='IOTC Secretariat'),'IRD_reference']<-'IOTC-2018-WPB16-22' 
  
  ## doc duplicate was a presentation
  # this_meeting[which(this_meeting$Reference=='IOTC-2011-SC14-45'),'IRD_reference']<-paste('IOTC-2011-SC14-45','-Pres') 
  
  
  return(this_meeting)
}