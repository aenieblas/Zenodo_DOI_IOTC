## geoflow_step1.R
## AE Nieblas
## 27/6/2019
## DESCRIPTION : Runs the geoflow workflow step 1 : deposit of metadata on Zenodo/Sandbox (depending on config file!) and reserves DOIs
## OUTPUTS : sharelink_publish to the <iotc_doc_type>_uploaded gsheet with the doi added to the identifier column, and the data column filled. To be used in the geoflow workflow step 2.
## INPUTS  : wd : char working directory
##           sharelink : char derived from the filtering step. this is the gsheet sharelink of the filtered documents to be deposited
##           token     : char personal token for Zenodo/Sandbox
##           iotc_doc_type : char controlled vocabulary of the iotc document types, options being 
##                           'meeting_documents','meeting_reports','executive_summaries','national_reports','datasets','publications','expert_consultations'
##           new_sheet : boolean if TRUE, will create a new gsheet for the geoflow_step2. if FALSE, will write over existing gsheet for this step                 




##### -----------------------------------------  GEOFLOW WORKFLOW ------------------------------------------- #####

## 1. UPLOAD THE METADATA FROM THE GSHEET ONTO ZENODO (SANDBOX) AND ASSIGN DOIs
## make the json file using the iotc googlesheet just created

geoflow_step1<-function(wd,sharelink,token,iotc_doc_type,new_sheet=FALSE,xx){
  
  source(paste0(wd,"make_json_step1.R")  )       # function to create config file - step 1 : deposit
  
  ## create SANDBOX json file : iotc_config_step1.json
  make_json_step1(sharelink,token)
  
  ## GEOFLOW STEP 1 STARTS HERE :  deposits file metadata and reserves a doi
  executeWorkflow('iotc_config_step1.json')
  
  ## repopulate gsheets with the DOIs 
  # find the DOIs reserved for the documents in the newest 'jobs/metadata/zenodo_dois.csv' :
  ll<-list.files(paste0(wd,'/jobs'))
  zenodo_doi<-read.csv(paste0(wd,'jobs/',ll[length(ll)],'/metadata/zenodo_dois.csv'),sep=',')
  
  ## curl error solutions:
  # handle_setopt(handle, http_version = 0L)
  httr::set_config(httr::config(http_version = 0))
  
  ## replace "DOI" column of <iotc_doc_type>_filtered with zenodo_doi[,2]
  gs_edit_cells(xx,input=zenodo_doi[,2],anchor='R2',byrow=FALSE)## <---------AE to improve : THIS WILL CHANGE IF YOU CHANGE THE COLUMNS IN THE GSHEET
  
  
  ## create new/overwrite <iotc_doc_type>_uploaded gsheet with the reserved dois in the identifier column, and the data column filled to download file from URL
  ## download locally the content of the <iotc_doc_type>_filtered from step 1
  gs_download(xx,to=paste0(iotc_doc_type,'_uploaded.csv'),overwrite = T)
  
  ## read local csv into the R environment
  gsheet_pub<-read.csv(paste0(iotc_doc_type,'_uploaded.csv'))
  
  
  file_to_upload<-paste0(iotc_doc_type,'_uploaded.csv')
  
  ## add new/overwrite file to gdrive
  if(new_sheet==TRUE){
    gsheet_topublish <- gs_new(paste0(iotc_doc_type,'_uploaded'), ws_title = paste0(iotc_doc_type,'_uploaded'), input = gsheet_pub,
                               trim = TRUE, verbose = FALSE)
    # }else{gsheet_topublish<-gs_upload(paste0(iotc_doc_type,'_uploaded.csv'),overwrite=TRUE)}
  }else{gsheet_topublish<-gs_upload(file_to_upload,overwrite=TRUE)}
  
  ## move file to shared folder to enable the "share by link" functionality of the googlesheet
  drive_mv(file = paste0(iotc_doc_type,'_uploaded'), path = "iotc_csv_share/")  
  
  ## get the share link of the <iotc_doc_type>_uploaded gsheet
  x1<-gs_gs(gsheet_topublish)
  sharelink_publish<-paste0(x1$browser_url,'edit?usp=sharing')
  
  ## modify the <iotc_doc_type>_uploaded for the publication step (i.e. add dois)
  ii<-NULL
  for(c in 1:dim(zenodo_doi)[1]){
    ii<-as.data.frame(rbind(ii,paste0('id:',zenodo_doi[c,1],';\ndoi:',zenodo_doi[c,2])))
  }
  gs_edit_cells(gsheet_topublish,input=ii[1:(dim(ii)[1]),],anchor='A2',byrow=FALSE)
  
  
  ## add the Data column to the <iotc_doc_type>_uploaded gsheet for the publishing step
  tt<-NULL
  for(c in 1:dim(gsheet_pub)[1]){
    tmp<-gsub('id:','',strsplit(gsheet_pub$Identifier[c],';')[[1]])[1]
    tmp<-gsub(';','',tmp)
    ext<-unlist(strsplit(gsheet_pub$File[c],'\\.'))[3]
    tt<-as.data.frame(rbind(tt,paste0('identifier:',tmp,'.',ext,';\n',paste0('source:',gsheet_pub$File[c]),';\n',
                                      paste0('sourceName:',tmp),'.',ext,';\n',paste0('type:other'))))
  }
  gs_edit_cells(gsheet_topublish,input=tt[1:(dim(tt)[1]),],anchor='O2',byrow=FALSE)## <-------------------------THIS WILL CHANGE IF YOU CHANGE THE COLUMNS IN THE GSHEET
  
  return(gsheet_topublish)
}
