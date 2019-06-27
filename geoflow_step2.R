## geoflow_step2.R
## AE Nieblas
## 27/6/2019
## DESCRIPTION : Runs the geoflow workflow step 2 : upload and publish files on Zenodo/Sandbox (depending on config file!) 
## OUTPUTS : sharelink_published to the <iotc_doc_type>_published gsheet with the concept doi added as a new column. To be used by the IOTC to update their website.
## INPUTS  : wd : char working directory
##           sharelink_publish : char derived from the geoflow_step1. this is the gsheet sharelink of the documents and dois that have been deposited on Zenodo/Sandbox
##           token     : char personal token for Zenodo/Sandbox
##           iotc_doc_type : char controlled vocabulary of the iotc document types, options being 
##                           'meeting_documents','meeting_reports','executive_summaries','national_reports','datasets','publications','expert_consultations'
##           new_sheet : boolean if TRUE, will create a new gsheet for the geoflow_step2. if FALSE, will write over existing gsheet for this step                 




##### -----------------------------------------  GEOFLOW WORKFLOW ------------------------------------------- #####


## 2. UPLOAD AND PUBLISH THE FILES FROM GSHEET ONTO ZENODO (SANDBOX)

geoflow_step2<-function(wd,sharelink_topublish,xy,token,iotc_doc_type,new_sheet=TRUE){
  
  source(paste0(wd,"make_json_step2.R") )        # function to create config file - step 2 : upload and publish
  ## create new json for publication step
  make_json_step2(sharelink_topublish,token)
  executeWorkflow('iotc_config_step2.json')
  
  sysdate<-paste0(strsplit(paste0(strsplit(paste0(strsplit(as.character(Sys.time()),'-')[[1]],collapse=''),' ')[[1]],collapse=''),':')[[1]],collapse='')
  ## curl error solutions:
  # handle_setopt(handle, http_version = 0L)
  httr::set_config(httr::config(http_version = 0))
  
  ### CREATE A FINALISED PUBLICATION GSHEET AND ADD THE CONCEPT DOI
  ## locally download the content of the <iotc_doc_type>_uploaded gsheet with a date stamp
  gs_download(xy,to=paste0(wd,'published_csv/',iotc_doc_type,'_published_',sysdate,'.csv'),overwrite = T)
  
  ## read the local csv into the R environment
  gsheet_published<-read.csv(paste0(wd,'published_csv/',iotc_doc_type,'_published_',sysdate,'.csv'))
  
  ## add the Concept DOI (original version DOI - 1 )<------------------------ WARNING : this assumes the recently published DOI is the FIRST VERSION
  for(g in 1:dim(gsheet_published)[1]){
    gsheet_published$Concept_DOI[g]<-paste0(unlist(strsplit(gsheet_published$DOI[g],'\\.'))[1],'.',unlist(strsplit(gsheet_published$DOI[g],'\\.'))[2],'.',
                                            as.character((as.numeric(unlist(strsplit(gsheet_published$DOI[g],'\\.'))[3])-1)))
  }
  
  ## write local csv file
  write.csv(gsheet_published,file=paste0(wd,'published_csv/',iotc_doc_type,'_published_',sysdate,'_withconceptdoi.csv'))
  
  ##read local csv into R environment
  # gsheet_published_with_DOIs<-read.csv(paste0(wd,'published_csv/',iotc_doc_type,'_published_',paste0(strsplit(as.character(Sys.Date()),'-')[[1]],collapse=''),'_withconceptdoi.csv'))
  
  ## add new file to gdrive
  if(new_sheet==TRUE){
    gsheet_published_wDOI <- gs_new(paste0(iotc_doc_type,'_published_',sysdate,'_withconceptdoi'), 
                                    ws_title = paste0(iotc_doc_type,'_published'), 
                                    input = gsheet_published,trim = TRUE, verbose = FALSE)
  }else{
    gsheet_published_wDOI <- gs_upload(paste0(iotc_doc_type,'_published_',sysdate,'_withconceptdoi'),overwrite=TRUE)}
  
  ## move file to shared folder to enable the "share by link" functionality of the googlesheet
  drive_mv(file = paste0(iotc_doc_type,'_published_',sysdate,'_withconceptdoi'), path = "published/")
  # drive_mv(file = paste0(iotc_doc_type,'_published'), path = "published/")
  
  ## get the share link of the gsheet
  x1<-gs_gs(gsheet_published_wDOI)
  sharelink_published<-paste0(x1$browser_url,'edit?usp=sharing')
  
  return(gsheet_published_wDOI)
}
