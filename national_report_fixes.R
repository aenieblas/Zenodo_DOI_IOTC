# meeting_report_fixes.R

national_report_fixes<-function(this_meeting){
  # this_meeting[190,'IRD_reference']<-'IOTC-2016-SC18-NR01'
  
  ## metadata adjustments
  this_meeting[which(this_meeting$Reference=='IOTC-2015-SC18-NR01' & this_meeting$Meeting.year=='2016'),'Reference']<-'IOTC-2016-SC19-NR01'
  this_meeting[which(this_meeting$IRD_reference=='IOTC-2015-SC18-NR01' & this_meeting$Meeting.year=='2016'),'IRD_reference']<-'IOTC-2016-SC19-NR01'
  
  this_meeting[which(this_meeting$Reference=='IOTC-2009-SC-INF04'),'Language']<-'French'
  this_meeting[which(this_meeting$Reference=='IOTC-2009-SC-INF04'),'IRD_reference']<-paste0(this_meeting[which(this_meeting$Reference=='IOTC-2009-SC-INF04'),'Reference'],'F')
  
  this_meeting[which(this_meeting$Reference=='IOTC-2018-SC21-NR14'),'Language']<-'French'
  this_meeting[which(this_meeting$Reference=='IOTC-2018-SC21-NR14'),'IRD_reference']<-paste0(this_meeting[which(this_meeting$Reference=='IOTC-2018-SC21-NR14'),'Reference'],'F')
  
  this_meeting[which(this_meeting$Reference=='IOTC-2016-SC19-NR15 Rev_1'),'Language']<-'French'
  this_meeting[which(this_meeting$Reference=='IOTC-2016-SC19-NR15 Rev_1'),'IRD_reference']<-paste0(this_meeting[which(this_meeting$Reference=='IOTC-2016-SC19-NR15 Rev_1'),'Reference'],'F')
  
  this_meeting$Title<-gsub('Koorea','Korea',this_meeting$Title)
  
  return(this_meeting)
}