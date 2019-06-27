## zenodo_doi_iotc.R
## AE Nieblas
## 27/6/2019

## DESCRIPTION : the wrapper script that launches the workflow to assign DOIs to IOTC documents and publish them on Zenodo.
## This workflow requires 4 steps 
## 1) The filtering process : applies the selection criteria of IOTC documents, as discussed in the OpenAIRE workshop in 
## Victoria, Seychelles 28/4/2019 (add meeting report link) and described in detail in the filtering protocol (add link).
##   - filters files by IOTC document type (meeting_documents, meeting_reports, executive_summaries, national_reports, datasets, publications, expert_consulations)
##   - creates gsheets of the IOTC document metadata, following the structure required by geoflow
## 2) Step 1 of the geoflow workflow : deposits metadata on Zenodo/Sandbox and reserves DOIs
## 3) Step 2 of the geoflow workflow : uploads and publishes files on Zenodo/Sandbox, then creates a final gsheet with 
##    Version DOIs and Concept DOIs for publication on IOTC website.

################## SOME MANUAL GDRIVE SET UP REQUIRED #####################
## GDRIVE
# create a gdrive folder in the first instance ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MANUAL SET UP REQUIRED HERE 
# drive_mkdir("iotc_csv_share")  # make folder in home Drive directory (uncomment if first instance)
# drive_mkdir("published")       # make folder in home Drive directory (uncomment if first instance)

# ### !!!!  MUST MANUALLY TURN ON SHARE BY LINK FUNCTION OF THE DRIVE DIRECTORY !!!!### ## ~~~~~~~ MANUAL SET UP REQUIRED HERE 
# navigate to the new folder in your gdrive and share the FOLDER.
############### END MANUAL SET UP ###########


library(pacman)
p_load('googledrive','googlesheets','curl','tidyverse','tools','dplyr','RecordLinkage')
library(zen4R)
library(geoflow)

## DEFINE HERE the IOTC document type for this workflow, using controlled vocabulary: 
## 'meeting_documents','meeting_reports','executive_summaries','national_reports','datasets','publications','expert_consultations'
iotc_doc_type='national_reports'

token='<YOUR_TOKEN>'

## set directory and source functions
wd<-'/home/ae/Documents/PERSONAL/IOTC_DOI/zenodo_doi_iotc/'
setwd(wd)
source(paste0(wd,"iotc_2_gsheet_filters_v2.R"))
source(paste0(wd,"geoflow_step1.R"))
source(paste0(wd,"geoflow_step2.R"))


## 1) filter documents according to selection criteria
## this step outputs the info of the filtered gsheet to be used in the geoflow_step1 (next)
filtered_files_gsheet_info<-iotc_filters(wd,iotc_doc_type,check_files=FALSE,new_sheet=FALSE,check_file=FALSE)

## make the sharelink using the gsheet info
x1<-gs_gs(filtered_files_gsheet_info)
sharelink_for_step1<-paste0(x1$browser_url,'edit?usp=sharing')

## 2) geoflow_step1
## requires sharelink and gsheet info from filtered gsheet (above) and TOKEN (specific to user)
## this step is will create and launch a SANDBOX configuration file : iotc_config_step1.json
## example config file to publish in ZENODO is given in this file : iotc_config_step1_real.json
## NOTE : you can ignore the Warning message: In read.table(file = file, header = header, sep = sep, quote = quote,  :
#                 incomplete final line found by readTableHeader on '<iotc_doc_type>_uploaded.csv'
gsheet_info_step1<-geoflow_step1(wd,sharelink=sharelink_for_step1,xx=filtered_files_gsheet_info,
                                 token,
                                 iotc_doc_type,new_sheet=FALSE)

## make the sharelink using the gsheet info
x2<-gs_gs(gsheet_info_step1)
sharelink_for_step2<-paste0(x2$browser_url,'edit?usp=sharing')

## 3) geoflow_step2
## requires sharelink and gsheet info (xx) from uploaded gsheet (geoflow_step1)
## this step will create and launch a SANDBOX configuration file : iotc_config_step2.json
## example config file to publish in ZENODO is given in this file : iotc_config_step2_real.json
## NOTE : you can ignore the Warning message: In read.table(file = file, header = header, sep = sep, quote = quote,  :
#                 incomplete final line found by readTableHeader on '<iotc_doc_type>_published_<systime>.csv'

gsheet_info_step2<-geoflow_step2(wd,sharelink_topublish=sharelink_for_step2,xy=gsheet_info_step1,
                                   token,
                                   iotc_doc_type,new_sheet=TRUE)

## make the sharelink using the gsheet info
x3<-gs_gs(gsheet_info_step2)
sharelink_for_published_docs<-paste0(x3$browser_url,'edit?usp=sharing')




