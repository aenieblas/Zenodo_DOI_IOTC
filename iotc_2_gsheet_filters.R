# iotc_2_gsheet_filters.R
# a.e. nieblas
# 11/4/19

# description: This script uses the iotc doc type metadata csvs locally downloaded from https://iotc.org/test/biblio/docs (username: BIBLIO_BROWSER, pw: JLBorges4leph),
# these are then uploaded to a googlesheet, minor changes are made to the column names/inputs to adapt to geoflow syntax, moved to a shared google
# drive, and then the share link is derived and used as input to write a new iotc_config_tmp.json file to be read into the executeWorkflow function of 
# the geoflow package. The doi's and reference id's are then appended to the googlesheet for transfer back to IOTC. the executeWorkflow step should 
# also upload the documents to zenodo sandbox.

## note: common errors arise from the curl package. troubleshoot by detach/reload package, or even reinstall.
##       if documents don't appear to upload to zenodo, CHANGE YOUR TOKEN

## improvements: separate the workflow in 3 main steps- 1) filter 2) metadata manipulation for geoflow and in gsheet to a) upload and b) publish. 4) launch geoflow
##               remove as many for loops as possible
##               add functions to neaten code


################## SOME MANUAL GDRIVE SET UP REQUIRED #####################
## GDRIVE
# create a gdrive folder in the first instance ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MANUAL SET UP REQUIRED HERE 
# drive_mkdir("iotc_csv_share")  # make folder in home Drive directory (uncomment if first instance)

# ### !!!!  MUST MANUALLY TURN ON SHARE BY LINK FUNCTION OF THE DRIVE DIRECTORY !!!!### ## ~~~~~~~ MANUAL SET UP REQUIRED HERE 
# navigate to the new folder in your gdrive and share the FOLDER.
############### END MANUAL SET UP ###########


######### INSTALL AND/OR SET LIBRARIES, SOURCE FUNCTIONS, SET WD ############
# library(devtools)
# install_github("eblondel/zen4R")
# install_github("eblondel/geoflow")

iotc_filters<-function(wd,iotc_doc_type,check_files=FALSE,new_sheet=FALSE,check_file=FALSE){
  
  source(paste0(wd,"filter_iotc_docs.R"))        # script with all the filter functions
  source(paste0(wd,"meeting_report_fixes.R") )   # function to create config file
  source(paste0(wd,"national_report_fixes.R") )  # function to create config file
  source(paste0(wd,"executive_summary_fixes.R") )# function to create config file
  source(paste0(wd,"meeting_document_fixes.R"))  # function to create config file
  
  ## curl error solutions:
  # handle_setopt(handle, http_version = 0L)
  httr::set_config(httr::config(http_version = 0))
  
  ## SET LOCAL WD
  ## download iotc source csvs to a directory and set the directory as 'home'
  # home='/home/ae/Documents/PERSONAL/IOTC_DOI/iotc2gsheet/'
  # setwd(home)
  options(stringsAsFactors=FALSE)
  ##############################################################################################
  
  
  
  ## IOTC DOC TYPE SELECTION
  ## this is the naming convention for the iotc doc types. 
  # iotc_doc_type<-c('meeting_documents','meeting_reports','executive_summaries','national_reports','datasets','publications','expert_consultations')## GUIDELINES HAS BEEN REMOVED AS A PRIORITY DOCUMENT TYPE
  
  
  # for (i in 2){## <--------------------- select which doc type you want to process (iotc_doc_type) (AE:improve here)
  iotc_rem<-NULL
  iotc_filt<-NULL
  
  iotc_dl<-read.csv(paste0(wd,'source_csv/',iotc_doc_type,'.csv'))
  
  
  ###### ---------------------------------------- RECLASSIFY DOCUMENT TYPES -------------------------------------- ######
  ### reclassify EXECUTIVE SUMMARIES 
  if(iotc_doc_type!='executive_summaries'){ 
    iotc_dl<-reclass(x='executive_summaries',y='executive summar',z='Executive Summaries',df=iotc_dl,column='Title')
    iotc_dl<-reclass(x='executive_summaries',y='cutif',z='Executive Summaries',df=iotc_dl,column='Title')
    iotc_dl<-reclass(x='executive_summaries',y='IOTC-2012-SC15-08',z='Executive Summaries',df=iotc_dl,column='Reference')
  }
  
  ### reclassify NATIONAL REPORTS  
  if(iotc_doc_type!='national_reports'){ 
    iotc_dl<-reclass(x='national_reports',y='national report', z='National Reports',df=iotc_dl,column='Title')
    iotc_dl<-reclass(x='national_reports',y='national tuna fishery report', z='National Reports',df=iotc_dl,column='Title')
    iotc_dl<-reclass(x='national_reports',y='national fishery report', z='National Reports',df=iotc_dl,column='Title')
    iotc_dl<-reclass(x='national_reports',y='Rapport National', z='National Reports',df=iotc_dl,column='Title')
    iotc_dl<-reclass(x='national_reports',y='Country Report', z='National Reports',df=iotc_dl,column='Title')
  }
  
  ### reclassify MEETING REPORTS
  if(iotc_doc_type!='meeting_reports'){ 
    iotc_dl<-reclass(x='meeting_reports',y=c('Report of the ','Session'), z='Meeting Reports',df=iotc_dl,column='Title')
    iotc_dl<-reclass(x='meeting_reports',y=c('Rapport de la','Session'), z='Meeting Reports',df=iotc_dl,column='Title')
  }
  
  if(iotc_doc_type!='expert_consultations'){ 
    if(iotc_doc_type!='national_reports'){
      iotc_dl<-reclass(x='expert_consultations',y='expert consult', z='Expert Consultations',df=iotc_dl,column='Meeting')
    }}
  
  # ## EXPERT CONSULTATIONS file had to be written as it is not an IOTC "document type". After being written, it was added to the reclassify steps above)
  # iotc_ec<-iotc_dl[grep('expert consult',iotc_dl$Meeting,ignore.case = T),]## <---------- !!! this might not be the case for doc types other than Meeting Docs
  # iotc_ec$Type<-"Expert Consultations"
  # write.csv(iotc_ec,file=paste0('expert_consultations.csv'),row.names = F) ## <---------- !!! if these are found in doc types, must find an option to file exists, overwrite, etc.
  # ## take expert consultations out of meeting docs
  # if(length(grep('expert consult',iotc_dl$Meeting,ignore.case = T))>0){iotc_dl<-iotc_dl[-grep('expert consult',iotc_dl$Meeting,ignore.case = T),]}
  
  
  ##### ------------------------------------- SELECTION CRITERIA APPLIED ----------------------------------- #####
  
  ##### ------------------------------------ ADD LANGUAGES TO EMPTY CELLS ---------------------------------- #####
  
  if(iotc_doc_type=='meeting_reports'){ ## add French to meeting reports where Language column is empty
    for (r in 1:dim(iotc_dl)[1]){
      if(length(grep('Rapport',iotc_dl$Title[r]))>0 & iotc_dl$Language[r]==''){   iotc_dl$Language[r]<-'French'      }
      if(length(grep('Rapport',iotc_dl$Title[r]))>0 & iotc_dl$Language[r]=='English'){   print(paste(r,'warning!'))  }
    }
  }
  
  ## THEN add languages to empty cells by 1) determining if there is a language identifier in the reference name and 
  ## 2) assuming non-indicated docs are in English (default lang)
  iotc_dl<-fill_empty_lang(iotc_dl)
  
  #### ------------------------------------ STANDARDIZE REFERENCE NAME SYNTAX ------------------------------ #####
  if(length(which(colnames(iotc_dl)=='IRD_reference'))==0){iotc_dl$IRD_reference<-iotc_dl$Reference}
  iotc_dl[,'IRD_reference']<-gsub('–','-',iotc_dl[,'IRD_reference'])
  iotc_dl[,'IRD_reference']<-gsub('/','-',iotc_dl[,'IRD_reference'])
  
  
  #### -------------------------------------- FILTER HIGH PRIORITY MEETINGS -------------------------------- #####
  not_this_meeting<-NULL
  if(iotc_doc_type=='meeting_documents'|iotc_doc_type=='meeting_reports'|iotc_doc_type=='executive_summaries'|iotc_doc_type=='national_reports'){
    high_priority_meetings<-c('Scientific committee',
                              'Commission',
                              'Working Party on Temperate Tuna',
                              'Working Party on Data Collection and Statistics',
                              'Working Party on Tropical Tunas', 
                              'Working Party on Billfish', 
                              'Working Party on Methods',
                              'Working Party on Ecosystems and Bycatch', 
                              'Working Party on Neritic Tunas',
                              'IOTC ad hoc Working Group on FADs',
                              'Indian Ocean Tuna Tagging Symposium')
    
    if(iotc_doc_type=='national_reports'){high_priority_meetings<-c(high_priority_meetings,'Expert consultations')} ## National reports that were stored as ECs
    if(iotc_doc_type=='meeting_documents'){high_priority_meetings<-c(high_priority_meetings[-grep('Commission',high_priority_meetings)])} 
    
    i_meeting<-grep(paste(high_priority_meetings,collapse='|'),iotc_dl$Meeting,ignore.case = T)
    
    ## remove special session of the commission, but keep the commission
    if(length(grep('Commission',high_priority_meetings))>0){special<-grep('Special session',iotc_dl$Meeting,ignore.case = T) 
    if(length(special)>0){i_meeting<-i_meeting[-grep(paste(special,collapse='|'),i_meeting)]}}
    
    this_meeting<-iotc_dl[i_meeting,]
    
    if(!is.null(i_meeting)){not_this_meeting<-cbind(iotc_dl[-i_meeting,],data.frame(Filter_used='Low priority meeting'))}
    
  }else{this_meeting<-iotc_dl}
  
  #### ------------------------------------- REMOVE COMPLETE LINE DUPLICATES ------------------------------- ##### 
  total_duplicates<-NULL
  if(dim(this_meeting[duplicated(this_meeting),])[1]>0){total_duplicates<-cbind(this_meeting[duplicated(this_meeting),],data.frame(Filter_used='Complete duplicate'))}
  this_meeting<-this_meeting %>% distinct()
  
  
  #### --------------------------------------- REMOVE SECRETARIAT DOCUMENTS -------------------------------- ##### 
  sec_removals<-secretariat(this_meeting)
  
  #### ------------------------------------------- REMOVE INFO PAPERS -------------------------------------- #####
  other_filters<-NULL       ## will return data frame of indices to be removed
  
  ########### ~~~~~~~~~~~~~ NATIONAL REPORTS REQUIRE SPECIFIC FILTERS FOR INFO PAPERS ~~~~~~~~~~~~~~~~~~ #########  
  if(iotc_doc_type=='national_reports'){
    i_inf<-grep('INF',this_meeting$Reference,ignore.case = T)
    inf<-this_meeting[i_inf,]
    
    # does 'national report' appear in the title of these info papers? 
    nat_pat<-c('national report','national tuna fishery report','Rapport National','national fishery report','country report')
    nat_rep<-grep(paste(nat_pat,collapse='|'),inf$Title,ignore.case = T)
    # if so, do not remove due to it being an INFO paper, if not, remove it
    not_nat_rep<-i_inf[-nat_rep]
    other_filters<-rbind(nonzero_filters(cbind(not_nat_rep,'Info paper')),other_filters)
    
    ## remove IOTC Secretariat from authors list
    this_meeting[grep('IOTC Secretariat',this_meeting$Authors),'Authors']<-''
    
    ## change the author to the country for the national reports
    cpc_caps_lock<-cbind(rbind('UK','EU'),rbind('United Kingdom OT','European Union'))
    for(c in 1:dim(cpc_caps_lock)[1]){ this_meeting[grep(cpc_caps_lock[c,1],this_meeting$Title),'Authors']<-cpc_caps_lock[c,2]}
    
    cpc_ignore_case<-c('Spain','France','Australia','Madagascar','Comores','China','South Africa','Japan','Korea','Senegal','Vanuatu','Mozambique',
                       'Sri Lanka','Seychelles','Mauritius','Maldives','Malaysia','Kenya','Iran','Indonesia','European Union','Belize',
                       'Thailand','United Kingdom','Philippines','Pakistan','Bangladesh','Somalia','Oman','Comoros','Tanzania','Eritrea','Taiwan','European Community')
    for(c in 1:length(cpc_ignore_case)){ this_meeting[grep(cpc_ignore_case[c],this_meeting$Title,ignore.case = T),'Authors']<-cpc_ignore_case[c]}
    
    fr_ot<-c('territories','territoires','France \\(OT\\)','France OT','\\(TOM\\)','Mayotte')
    for(c in 1:length(fr_ot)){ this_meeting[grep(fr_ot[c],this_meeting$Title),'Authors']<-'France OT'}
    
    uk_ot<-c('United Kingdom \\(OT\\)','UK OT','UK\\(BIOT\\)')
    for(c in 1:length(uk_ot)){ this_meeting[grep(uk_ot[c],this_meeting$Title),'Authors']<-'United Kingdom OT'}
    
    ## individual renaming of AUTHORS for flagged files
    this_meeting[grep('United Kingdom',this_meeting$Authors),'Authors']<-'United Kingdom OT'
    this_meeting[grep('Koorea',this_meeting$Title),'Authors']<-'Korea'
    this_meeting[grep('EC7-07',this_meeting$Reference),'Authors']<-'Saudi Arabia'
    this_meeting[which(this_meeting$Authors=='')[grep('India',this_meeting$Title[which(this_meeting$Authors=='')],ignore.case=T)],'Authors']<-'India'
    
    this_meeting[which(this_meeting$Title=='National report on tuna fishery - 2007'),'Authors']<-'Kenya'
    
    ## double check empty author files and remove any remaining info papers at this step 
    empty_author<-which(this_meeting$Authors=='')
    other_filters<-rbind(nonzero_filters(cbind(empty_author[grep('INF',this_meeting[empty_author,'Reference'],ignore.case = T)],'Info paper')),other_filters)
  }else{
    other_filters<-rbind(nonzero_filters(cbind(grep('INF',this_meeting$Reference,ignore.case = T),'Info paper')),other_filters)                   
  }
  
  other_filters<-rbind(nonzero_filters(cbind(grep('presentation',this_meeting$Title,ignore.case = T), 'Presentation file')),other_filters)    # returns data frame of indices to be removed
  other_filters<-rbind(nonzero_filters(cbind(grep('withdrawn',this_meeting$Title,ignore.case = T), 'Withdrawn')),other_filters)               # returns data frame of indices to be removed
  other_filters<-rbind(nonzero_filters(cbind(grep('withdrawn',this_meeting$Reference,ignore.case = T), 'Withdrawn')),other_filters)           # returns data frame of indices to be removed
  other_filters<-rbind(nonzero_filters(cbind(grep('Pres',this_meeting$Reference,ignore.case = T), 'Presentation')),other_filters)             # returns data frame of indices to be removed
  other_filters<-rbind(nonzero_filters(cbind(grep('zip',this_meeting$File,ignore.case = T), 'Zip file')),other_filters)                       # returns data frame of indices to be removed
  
  ## make a df of these filters
  other_removals<-data.frame(Index=as.numeric(other_filters[,1]),Filter_used=as.character(other_filters[,2])) 
  
  
  ##### --------------- IDENTIFY DUPLICATES, FILTER BY MEETING, LANGUAGE, SESSION, ABSTRACT (DATE) --------------------- #####
  ### duplicates by reference name are then differentiated by other metadata, including
  ## MEETING : if one of the meetings is SC and the other is a WP, and the doc is a report, remove the SC doc.
  ## LANGUAGE : keeps the most recent of each language
  ## MEETING SESSION : keeps the most recent, then the one that aligns with the reference name indicator of meeting session
  ## AUTHOR : FLAGS where authors are different between duplicates and notes the the most recent (to keep) and the 
  ## similarity of the character string where the date of availability is the same
  ## ABSTRACT : keeps the most recent, and if there are multiple most recent, keeps the one with the abstract
  meeting_removals<-meeting(this_meeting,action='indices')                                 # returns data frame of MEETING indices to be removed
  language_removals<-language(this_meeting,action='indices',doc_type=iotc_doc_type)     # returns data frame of LANGUAGE indices to be removed
  this_meeting<-language(this_meeting,action='change.df',doc_type=iotc_doc_type)        # returns df with the IRD reference names altered to the correct language identifier
  meeting_session_removals<-meeting_session(this_meeting,action='indices')                 # returns data frame of MEETING SESSION indices to be removed
  author_flags<-authors(this_meeting,reference='Reference',action='indices')               # identifies where AUTHORS are different between duplicate reference names, corrects or flags, returns df
  abstract_removals<-abstract(this_meeting,reference='IRD_reference')                      # identifies where ABSTRACTS are different between duplicate IRD_reference names(i.e. already adjusted for language), flags, returns df 
  
  
  ##### ----------------------------------- DEALING WITH FLAGGED FILES FOR EACH DOC TYPE -------------------------------- #####
  date_removal<-NULL
  if(iotc_doc_type=='meeting_documents'){
    
    this_meeting<-meeting_document_fixes(this_meeting)
    
    # identify flagged files to be removed : 
    ## same docs, diff dates, keep the most recent:
    date_removal<-data.frame(Index=which(this_meeting$Availability=='2018-08-20' & this_meeting$IRD_reference=='IOTC-2018-WPB16-22'),Filter_used='Date')
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Availability=='2016-07-07' & this_meeting$IRD_reference=='IOTC-2016-WPTmT06-26'),Filter_used='Date'))
    # date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Availability=='2012-11-13' & this_meeting$IRD_reference=='IOTC-2012-SC15-08'),Filter_used='Date'))
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='IOTC-2007-WPTT-03'),Filter_used='Date'))
    ## remove presentations
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='IOTC-2011-SC14-45'),Filter_used='Date'))
    ## IOTC-2012-SC15-38[E] - same as publication file  IOTC-2013-Pilot_data_collection
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='IOTC-2012-SC15-38[E]' & this_meeting$Language=='English'),Filter_used='Duplicate of publication'))
    
    
  }
  
  if(iotc_doc_type=='meeting_reports'){
    ## modifying metadata of flagged files
    this_meeting<-meeting_report_fixes(this_meeting)
    
    ## identifies removals of flagged files
    ## doc incorrectly labeled 'French', when actually English, but there are 2 other duplicates of the English, so remove this one
    date_removal<-data.frame(Index=which(this_meeting$Reference=="IOTC-2006-SC-R[FR]" & this_meeting$Node.ID==1324),Filter_used='Duplicate doc')
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference== "IOTC-2006-SC-R[EN]" & this_meeting$Node.ID==1322),Filter_used='Duplicate doc'))
    # IOTC-2013-WSIR-R
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Meeting== 'Regional workshop to support the implementation of the Resolutions of the Indian Ocean Tuna Commission'),Filter_used='Low priority meeting'))
    ## same file, different identifiers due to language:
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='FIPL/R564(Bi)' & this_meeting$Language=='French'),Filter_used='Consolidate 2 lang to 1 file'))
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='FIPL/R551(Bi)' & this_meeting$Language=='French'),Filter_used='Consolidate 2 lang to 1 file'))
    ## zip file   IOTC-2014-SC17-RE
    date_removal<-rbind(date_removal,data.frame(Index=grep('.zip',this_meeting$File),Filter_used='Zip file'))
    
    ## ZENODO API REMOVALS <- files that did not pass the REAL zenodo publishing process
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='IOTC-2011-WPTT13-R[F]'),Filter_used='Zenodo API'))
  }
  
  
  if(iotc_doc_type=='executive_summaries'){
    this_meeting<-executive_summary_fixes(this_meeting)
    ## identifies removals of flagged files
    ## IOTC-2017-SC20-ES20 : French metadata title, English file
    date_removal<-data.frame(Index=which(this_meeting$Reference=='IOTC-2017-SC20-ES20' & this_meeting$Language=='French'),Filter_used='Incorrect language file')
    # IOTC-2014-SC17-ES05
    # this_meeting[which(this_meeting$Reference=='IOTC-2014-SC17-ES05' & this_meeting$Language=='French'),'Title'] <-'Report on Biology, Stock Status and Management of Southern Bluefin Tuna: 2014' 
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='IOTC-2014-SC17-ES05' & this_meeting$Language=='French'),Filter_used='Incorrect language file'))
    ## IOTC–2015–SC18–13 :   working papers (to be published with working papers)
    date_removal<-rbind(date_removal,data.frame(Index=grep('IOTC-2015-SC18-13',this_meeting$IRD_reference),Filter_used='working paper (not ES)'))
    ## IOTC–2015–SC19-23 :  note 2 files - these are working papers (to be published with working papers)
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='IOTC-2017-WPTT19-23_Rev3'),Filter_used='working paper (not ES)'))
    ## IOTC-2017-WPM08-16 : Revision of Supporting information for IOTC species Executive Summaries (not ES)
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='IOTC-2017-WPM08-16' ),Filter_used='working paper (not ES)'))
    ## IOTC-2017-S21-13 : Principes pour une procédure amendée de sélection et de nomination du Secrétaire exécutif de la CTOI (secretariat doc)
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference== "IOTC-2017-S21-13"),Filter_used='Secretariat'))
    ##IOTC-2018-S22-03a : not ES - secreariate doc
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='IOTC-2018-S22-03a' ),Filter_used='Secretariat'))
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='IOTC-2018-S22-03b' ),Filter_used='Secretariat'))
    # IOTC-2011-WPTT13-09 : Template for a resource Executive Summary (not ES )- secreariate doc
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$Reference=='IOTC-2018-S22-03b' ),Filter_used='Secretariat'))
    
    ## SBT docs written by CCSBT
    sbt=c('Southern bluefin tuna','SBT','CCSBT','thon rouge','thons rouges')
    this_meeting[grep(paste(sbt,collapse='|'),this_meeting$Title,ignore.case = T),'Title']
    date_removal<-rbind(date_removal,data.frame(Index=grep(paste(sbt,collapse='|'),this_meeting$Title,ignore.case = T),Filter_used='CCSBT report'))
    
    ### species keywords - adding species keywords - AE : next steps ...
    # utitle<-unique(this_meeting$Title)
    # not_species<-c('Executive','Summary','Summaries','cutif','of ','the ','status','report','stock','management',':','Projet','de ','du ','la ','resource',
    #                'pour','biology','and ','Indian','Ocean ','dans ','Indien','ressource','tat',' . - Dated 11 July 2007','Major','2011','2013')
    # ocean<-"l’océan"
    # fr_out<-c(ocean,'Résumé Exé  É  ','Résumé Exé  É ','Résumé Exé  ',' Synthèses Sur L',' Des ',' . IOTC Secretariat / Secrétariat CTOI',' on ,    ',
    #           ' in  Ocean','Species   - ','Résumé exé ','Synthese Sur L',' é  ')
    # utitle<-unique(gsub(paste(not_species,collapse='|'),'',this_meeting$Title,ignore.case = T))
    # 
    # unique(gsub(paste(fr_out,collapse='|'),'',utitle,ignore.case = T))
  }
  
  if(iotc_doc_type=='national_reports'){
    this_meeting$Type<-'report'
    
    # this_meeting[190,'IRD_reference']<-'IOTC-2016-SC18-NR01'
    
    # d <-this_meeting[which(duplicated(this_meeting[,c('Title','Meeting.year')])),]
    # f=9; d_flag<-which(this_meeting$Title==d$Title[f] & this_meeting$Meeting.year==d$Meeting.year[f])
    
    ## metadata fixes
    this_meeting<-national_report_fixes(this_meeting) 
    
    ## flagged files marked for removal 
    date_removal<-data.frame(Index=which(this_meeting$IRD_reference=='IOTC-2009-SC-INF15'),Filter_used='Flagged doc')                                ## d_flag 1 : flagged as 2 korean national reports, but this one is actually an INFO paper to remove
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$IRD_reference=='EC601-23' & this_meeting$Abstract==''),Filter_used='Date')) ## d_flag 3 : almost total duplicate except abstract 1 is NA and abstract 2 is ''
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$IRD_reference=='EC601-21' & this_meeting$Abstract==''),Filter_used='Date')) ## d_flag 3 : almost total duplicate except abstract 1 is NA and abstract 2 is ''
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$IRD_reference=='EC601-13' & this_meeting$Abstract==''),Filter_used='Date')) ## d_flag 3 : almost total duplicate except abstract 1 is NA and abstract 2 is ''
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$IRD_reference=='EC601-04' & this_meeting$Abstract==''),Filter_used='Date')) ## d_flag 3 : almost total duplicate except abstract 1 is NA and abstract 2 is ''
    
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$IRD_reference=='IOTC-2013-SC16-NR28'),Filter_used='Date'))                  ## d_flag 3 : old version removed
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$IRD_reference=='IOTC-2013-SC16-NR15'),Filter_used='Date'))                  ## d_flag 4 : old version removed
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$IRD_reference=='IOTC-2013-SC16-NR14'),Filter_used='Date'))                  ## d_flag 5 : old version removed
    
  }
  
  if(iotc_doc_type=='publications'){
    date_removal<-data.frame(Index=which(this_meeting$IRD_reference=='ID-THONS[EN]'),Filter_used='No file')
    date_removal<-rbind(date_removal,data.frame(Index=which(this_meeting$IRD_reference=='IPTP-90-WP-20 -Jan'),Filter_used='Not appropriate')) ## d_flag 5 : old version removed
    
    ##rename IRD_reference for FAO-SMARTFISH-GUIDE to match each section
    for(x in 1:5){
      this_meeting[which(this_meeting$Title==paste0('On board guide for the indetification of Pelagic Sharks and Rays. Western Indian Ocean (Part ',x,')')),'IRD_reference']<- paste0(this_meeting[which(this_meeting$Title==paste0('On board guide for the indetification of Pelagic Sharks and Rays. Western Indian Ocean (Part ',x,')')),'IRD_reference'],'-',x)
    }
  }
  
  
  if(iotc_doc_type=='expert_consultations'){
    date_removal<-data.frame(Index=grep('EXPERT CONSULTATION ON INDIAN OCEAN TUNAS',this_meeting$Title,ignore.case = T),Filter_used='EC Report')
    date_removal<-rbind(date_removal,data.frame(Index=grep("Consultation d'Experts sur les",this_meeting$Title,ignore.case = T),Filter_used='EC Report')) ## d_flag 5 : old version removed
    other_filters<-other_filters[-which(this_meeting[other_filters,'Title']=='Analysis of Tag Recoveries in Mauritius (1988-1993) and Presentation of Codification Procedure in Use.'),]
    
    ## language 
    this_meeting[which(this_meeting$Reference=='EC7-38'),'Language']<-'French'
    this_meeting[which(this_meeting$Reference=='EC601-08'),'Language']<-'French'
    
    # title
    this_meeting[,'Title']<-gsub('koingseer','kingseer',this_meeting[,'Title'])
    # grep('koingseer',this_meeting[,'Title'])
    
  }
  
  if(iotc_doc_type=='datasets'){
    code_removals<-NULL; param_removals<-NULL; catalog_removals<-NULL; equation_removals<-NULL; form_removals<-NULL;ce_all_removals<-NULL
    stock_ass_removals<-NULL; reference_removals<-NULL
    
    ## REMOVE 'CODES'
    code_removals<-data.frame(Index=grep('Code',this_meeting$Reference,ignore.case = T),Filter_used='Code')  
    code_removals<-unique(rbind(code_removals,data.frame(Index=grep('Code',this_meeting$Title,ignore.case = T),Filter_used='Code')  ))
    code_removals<-unique(rbind(code_removals,data.frame(Index=grep('script',this_meeting$Title,ignore.case = T),Filter_used='Code')  ))
    
    ## REMOVE MODEL PARAMETERISATIONS
    param_removals<-data.frame(Index=grep('Paramet',this_meeting$Title,ignore.case = T),Filter_used='Model parameterisations')  
    
    ## REMOVE DATA CATALOGS
    catalog_removals<-data.frame(Index=grep('Catalog',this_meeting$Title,ignore.case = T),Filter_used='Data catalogs')  
    catalog_removals<-unique(rbind(catalog_removals,data.frame(Index=grep('Catolog',this_meeting$Title,ignore.case = T),Filter_used='Data catalogs'))  )
    
    ## DATASET AVAILABILITY
    catalog_removals<-unique(rbind(catalog_removals,data.frame(Index=grep('availability of data',this_meeting$Title,ignore.case = T),Filter_used='Data catalogs'))  )
    # catalog_removals<-unique(rbind(catalog_removals,data.frame(Index=grep('datasets available',this_meeting$Title,ignore.case = T),Filter_used='Data catalogs'))  )
    catalog_removals<-unique(rbind(catalog_removals,data.frame(Index=grep('availabl',this_meeting$Reference,ignore.case = T),Filter_used='Data catalogs'))  )
    catalog_removals<-unique(rbind(catalog_removals,data.frame(Index=grep('datasets available',this_meeting$Title,ignore.case = T),Filter_used='Data catalogs'))  )
    
    
    ## REMOVE EQUATIONS (TO KEEP WHEN UPDATED)
    equation_removals<-data.frame(Index=grep('Equa',this_meeting$Title,ignore.case = T),Filter_used='Equations')  
    equation_removals<-unique(rbind(equation_removals,data.frame(Index=grep('Equa',this_meeting$Reference,ignore.case = T),Filter_used='Equations')  ))
    
    ## REMOVE DATA FORMS
    form_removals<-data.frame(Index=grep('Form',this_meeting$Title,ignore.case = F),Filter_used='Forms')  
    
    ## REMOVE IOTC SECRETARIAT STOCK ASSESSMENTS (FOR THE FUTURE)
    stock_ass_removals<-data.frame(Index=grep('Stock',this_meeting$Title,ignore.case = T),Filter_used='Stock assessment data and inputs')  
    
    ## REMOVE REFERENCE DATA
    reference_removals<-data.frame(Index=grep('reference',this_meeting$Title,ignore.case = T),Filter_used='Reference') 
    reference_removals<-unique(rbind(reference_removals,data.frame(Index=grep('CE',this_meeting$Title,ignore.case = F),Filter_used='Reference')))
    
    ## STANDARDISED CPUEs should have their authors changed (MED/LOW PRIORITY)
    ## awaiting instruction
    
    ## REMOVE CE ALL-GEARS
    ce_all_removals<-data.frame(Index=grep('all vessels',this_meeting$Title,ignore.case = T),Filter_used='CE ALL') 
    ce_all_removals<-unique(rbind(ce_all_removals,data.frame(Index=grep('all gears',this_meeting$Title,ignore.case = T),Filter_used='CE ALL') ))
    
    ce_all_removals<-unique(rbind(ce_all_removals,data.frame(Index=grep('SFALL',this_meeting$File,ignore.case = T),Filter_used='SF ALL') ))
    ce_all_removals<-unique(rbind(ce_all_removals,data.frame(Index=grep('SF_ALL',this_meeting$File,ignore.case = T),Filter_used='SF ALL') ))
    
    ce_all_removals<-unique(rbind(ce_all_removals,data.frame(Index=grep('FL_ALL',this_meeting$File,ignore.case = T),Filter_used='FL ALL') ))
    # ce_all_removals<-unique(rbind(ce_all_removals,data.frame(Index=grep('FLALL',this_meeting$File,ignore.case = T),Filter_used='FL ALL') ))
    
    ce_all_removals<-unique(rbind(ce_all_removals,data.frame(Index=grep('CASALL',this_meeting$File,ignore.case = T),Filter_used='CAS ALL') ))
    
    rem<-rbind(ce_all_removals,reference_removals,stock_ass_removals,form_removals,code_removals,param_removals,catalog_removals,equation_removals)
    i_rem_dupes<-which(duplicated(rem[,'Index'])==TRUE) # remove duplicate removal indices
    if(length(i_rem_dupes)>0){rem<-rem[-i_rem_dupes,]}else{rem<-rem} # remove from data frame
    
  }
  
  ##### ----------------------------------- CREATE GSHEET OF ALL REMOVED FILES -------------------------------- #####
  ## combine all the removal indices (except if datasets, which is done just above)
  if(iotc_doc_type!='datasets'){
    rem<-rbind(sec_removals,other_removals,meeting_session_removals,language_removals,abstract_removals,meeting_removals,date_removal)
    i_rem_dupes<-which(duplicated(rem[,'Index'])==TRUE) # remove duplicate removal indices
    if(length(i_rem_dupes)>0){rem<-rem[-i_rem_dupes,]}else{rem<-rem} # remove from data frame
  }
  
  
  ## create data frame of removed indices
  iotc_rem<-this_meeting[as.numeric(rem[,1]),]
  iotc_rem<-cbind(iotc_rem,rem[,2])
  colnames(iotc_rem)<-colnames(not_this_meeting)
  iotc_rem<-rbind(iotc_rem,not_this_meeting,total_duplicates)
  
  ## write a local csv of removed indices
  write.csv(iotc_rem,file=paste0(iotc_doc_type,'_removed.csv'),row.names = F)
  
  ## use the locally downloaded csv to populate gsheet
  removals_to_upload<-paste0(iotc_doc_type,'_removed.csv')
  rr<-gs_upload(removals_to_upload,overwrite=TRUE)
  
  ## move file to shared folder to enable the "share by link" functionality of the googlesheet
  drive_mv(file = paste0(iotc_doc_type,'_removed'), path = "iotc_csv_share/")
  
  ## get the share link of the gsheet
  x1<-gs_gs(rr)
  sharelink_removals<-paste0(x1$browser_url,'edit?usp=sharing')
  
  
  ##### ----------------------------------- ADJUST SYNTAX OF ALL REMAINING FILES -------------------------------- #####
  ## removal indices removed from the data fram
  if(dim(rem)[1]>0){iotc_filt<-this_meeting[-as.numeric(na.omit(rem[,1])),]}else{iotc_filt<-this_meeting}
  
  ## adjust syntax of the column names
  ## change the heading of gsheet to fill in spaces
  colnames(iotc_filt)<-gsub('\\.','_',colnames(iotc_filt))
  
  # fills in blanks with NAs for meeting_year # <-------------------------- AE: can delete?
  iotc_filt[which(is.na(iotc_filt$Meeting_year)),'Meeting_year']<-'9999'
  
  ## adjust syntax of reference names
  iotc_filt$IRD_reference<-gsub('_R\\[','-R\\[',iotc_filt$IRD_reference) 
  iotc_filt$IRD_reference<-gsub('rev','Rev',iotc_filt$IRD_reference)
  iotc_filt$IRD_reference<-gsub(' Rev','_Rev',iotc_filt$IRD_reference)
  iotc_filt$IRD_reference<-gsub('Rev_','Rev',iotc_filt$IRD_reference)
  iotc_filt$IRD_reference<-gsub('-Rev','_Rev',iotc_filt$IRD_reference)
  
  iotc_filt$IRD_reference<-gsub('FR','F', iotc_filt$IRD_reference)
  iotc_filt$IRD_reference<-gsub('EN','E', iotc_filt$IRD_reference)
  
  iotc_filt$IRD_reference<-gsub('\\[','', iotc_filt$IRD_reference)
  iotc_filt$IRD_reference<-gsub('\\]','', iotc_filt$IRD_reference)
  
  iotc_filt$IRD_reference<-gsub(' ','',iotc_filt$IRD_reference)
  
  iotc_filt$IRD_reference<-gsub('CTOI','IOTC', iotc_filt$IRD_reference)
  
  
  ##### ----------------------------------- ADD STANDARD KEYWORDS TO REMAINING FILES  -------------------------------- #####
  for(t in 1:dim(iotc_filt)[1]){
    if(iotc_doc_type=='expert_consultations'){
      iotc_filt$All_taxonomy_terms[t]<-paste('IOTC, IOTC Secretariat, Indian Ocean', iotc_filt$Meeting[t],sep=', ')
    }else{      iotc_filt$All_taxonomy_terms[t]<-paste('IOTC, IOTC Secretariat, Indian Ocean', iotc_filt$Meeting[t],unique(iotc_dl$Type),sep=', ')}
  }
  
  ##### ----------------------------------- CREATE ISO STANDARD LANGUAGE COLUMN -------------------------------- #####
  ## create a ISO standard language identifier column (format required by Zenodo)
  iotc_filt$lang<-NULL
  iotc_filt[which(iotc_filt$Language=='French'),'lang']<-'fra'
  iotc_filt[which(iotc_filt$Language=='English'),'lang']<-'eng'
  
  ##### ----------------------------------- ADD MISSING LANGUAGE IDENTIFIERS TO REFERENCE NAMES -------------------------------- #####
  ## add in language identifiers for meeting reports and executive summaries where they were missed in the language() step (AE : ------------------ why were they missed??)
  if(iotc_doc_type=='meeting_reports'){
    yeslang<-grep('R\\[',iotc_filt$Reference)
    for(l in 1:length(yeslang)){
      if(length(yeslang[l])>0 & iotc_filt[yeslang[l],'Language']=='French' & length(grep('-RF',iotc_filt[yeslang[l],'IRD_reference']))==0){
        iotc_filt[yeslang[l],'IRD_reference']<-gsub('-R','-RF',iotc_filt[yeslang[l],'IRD_reference'])      }
      if(length(yeslang[l])>0 & iotc_filt[yeslang[l],'Language']=='English' & length(grep('-RE',iotc_filt[yeslang[l],'IRD_reference']))==0){
        iotc_filt[yeslang[l],'IRD_reference']<-gsub('-R','-RE',iotc_filt[yeslang[l],'IRD_reference'])      }
    }
  }
  
  if(iotc_doc_type=='executive_summaries'){
    yeslang<-grep('\\[',iotc_filt$Reference)
    for(l in 1:dim(iotc_filt)[1]){
      if(iotc_filt[l,'Language']=='French' & length(grep('F',iotc_filt[l,'IRD_reference']))==0){ 
        iotc_filt[l,'IRD_reference']<-paste0(iotc_filt[l,'IRD_reference'],'F')      }
      test<-gsub('ES','',iotc_filt[l,'IRD_reference'])
      if(iotc_filt[l,'Language']=='English' & length(grep('E',test))==0){
        iotc_filt[l,'IRD_reference']<-paste0(iotc_filt[l,'IRD_reference'],'E')      }
    }
  }
  
  ##### ----------------------------------- FINAL CHECK FOR DUPLICATES -------------------------------- #####
  ##  check for any other duplicates in IRD reference name (adjusted ref name - there should be no duplicates here)
  if(dim(iotc_filt[duplicated(iotc_filt$IRD_reference),])[1]>0){
    print(paste('ATTENTION: THERE ARE STILL ',dim(iotc_filt[duplicated(iotc_filt$IRD_reference),])[1],'IRD REFERENCE DUPLICATES'))
    # identifies where AUTHORS are different between duplicate reference names, corrects or flags, returns df
    author_flags<-authors(iotc_filt,reference='IRD_reference',action='indices')       
    print(paste('ATTENTION: THERE ARE STILL ',dim(iotc_filt[duplicated(iotc_filt$File),])[1],'FILE DUPLICATES'))
    
    date_flags<-date_availability(iotc_filt,action='indices')
    
    ## find the metadata that differs between the duplicates
    i_remove<-NULL
    metadata_to_print<-c(5,8,9, 10, 11,12)#'Reference','Title','Language','Meeting session','Availability')
    
    d <- iotc_filt[duplicated(iotc_filt$IRD_reference),'IRD_reference']
    m <- match(iotc_filt$IRD_reference,d)
    
    if(length(which(is.na(m)==FALSE))>0){
      for(x in 1:max(m,na.rm=T)){# for each match set, find the differences between the entries
        id<-which(m==x)  # the match set
        
        meta_diffs<-metadata_difference(iotc_filt[id,])
        print(paste(x,': ',paste(meta_diffs,collapse=', ')))
      }
    }
  }
  
  if(check_files==TRUE){check_files(iotc_filt)}
  
  ##### ----------------------------------- ADJUST FINAL DATA FRAME TO GEOFLOW GSHEET STRUCTURE -------------------------------- #####
  ## adjust data frame to match geoflow gsheet structure
  iotc_filt<-data.frame(Identifier=iotc_filt$IRD_reference,
                        Title=iotc_filt$Title,
                        Description=NA,
                        Subject=NA,
                        Creator=NA,
                        Date=iotc_filt$Availability,
                        Type=iotc_filt$Type,
                        Standard_language=iotc_filt$lang,
                        Language=iotc_filt$Language,
                        SpatialCoverage=NA,
                        TemporalCoverage=NA,
                        Relation=NA,
                        Rights=NA,
                        Provenance=NA,
                        Data=NA,
                        Node_ID=iotc_filt$Node_ID,
                        Vid=iotc_filt$Vid,
                        DOI=iotc_filt$DOI,
                        Reference=iotc_filt$Reference,
                        IRD_reference=iotc_filt$IRD_reference,
                        IOTC_title=iotc_filt$Title,
                        Meeting=iotc_filt$Meeting,
                        Meeting_session=iotc_filt$Meeting_session,
                        Meeting_year=iotc_filt$Meeting_year,
                        Authors=iotc_filt$Authors,
                        All_taxonomy_terms=iotc_filt$All_taxonomy_terms,
                        File=iotc_filt$File,
                        Path=iotc_filt$Path,
                        Abstract=iotc_filt$Abstract
                        
  )
  
  ##### ----------------------------------- CREATE GSHEET OF REMAINING FILES -------------------------------- #####
  # write a local csv of the remaining docs
  write.csv(iotc_filt,file=paste0(iotc_doc_type,'_filtered.csv'),row.names = F)
  
  ## use the locally downloaded csvs to populate gsheet
  file_to_upload<-paste0(iotc_doc_type,'_filtered.csv')
  if(new_sheet==TRUE){
    xx<-gs_new(paste0(iotc_doc_type,'_filtered'), ws_title = paste0(iotc_doc_type,'_filtered'), input = iotc_filt,
               trim = TRUE, verbose = FALSE)
  }else{xx<-gs_upload(file_to_upload,overwrite=TRUE)}
  
  ## move file to shared folder to enable the "share by link" functionality of the googlesheet
  drive_mv(file = paste0(iotc_doc_type,'_filtered'), path = "iotc_csv_share/")  
  
  ## get the share link of the gsheet
  x1<-gs_gs(xx)
  sharelink<-paste0(x1$browser_url,'edit?usp=sharing')
  
  ##### ----------------------------------- DYNAMICALLY UPDATE THE GSHEET TO MATCH GEOFLOW -------------------------------- #####
  ##  <-------------------------------- AE to improve : doesn't work when edited directly in csv
  ## Identifier
  ii<-NULL
  for(c in 1:dim(iotc_filt)[1]){
    tmp<-gsub('\\[','',iotc_filt$Identifier[c])
    tmp<-gsub('\\]','',tmp)
    tmp<-gsub('\\(','',iotc_filt$Identifier[c])
    tmp<-gsub('\\)','',tmp)
    ii<-as.data.frame(rbind(ii,paste0('id:',tmp,';\n')))
  }
  gs_edit_cells(xx,input=ii[1:(dim(ii)[1]),],anchor='A2',byrow=FALSE)## <---------AE to improve : THIS WILL CHANGE IF YOU CHANGE THE COLUMNS IN THE GSHEET
  
  
  ## Relation
  pp<-NULL
  for(c in 1:dim(iotc_filt)[1]){
    tmp<-gsub('\\[','',iotc_filt$Path[c])
    tmp<-gsub('\\]','',tmp)
    tmp<-gsub('\\(','',iotc_filt$Path[c])
    tmp<-gsub('\\)','',tmp)
    pp<-as.data.frame(rbind(pp,paste0('http:website@',paste0('https://iotc.org',iotc_filt$Path[c]))))
  }
  gs_edit_cells(xx,input=pp[1:(dim(pp)[1]),],anchor='L2',byrow=FALSE)## <---------AE to improve : THIS WILL CHANGE IF YOU CHANGE THE COLUMNS IN THE GSHEET
  
  
  
  ## Rights
  rr<-NULL
  for(c in 1:dim(iotc_filt)[1]){
    rr<-as.data.frame(rbind(rr,
                            paste0('use:terms1;\nuse:citation1;\nuse:disclaimer1;\nuseConstraint:copyright;\nuseConstraint:license;\naccessConstraint:copyright;\notherConstraint:web use;')))
  }
  gs_edit_cells(xx,input=rr[1:(dim(rr)[1]),],anchor='M2',byrow=FALSE)## <---------AE to improve : THIS WILL CHANGE IF YOU CHANGE THE COLUMNS IN THE GSHEET
  
  ## Creator
  cc<-NULL
  for(c in 1:dim(iotc_filt)[1]){
    if(iotc_doc_type=='meeting_reports'|iotc_doc_type=='executive_summaries'){
      cc<-as.data.frame(rbind(cc,paste0('owner:IOTC-Secretariat@fao.org;\npointOfContact:IOTC-Secretariat@fao.org;')))
    }
    
    if(iotc_doc_type=='national_reports'){
      cc<-as.data.frame(rbind(cc,paste0('owner:nation',iotc_filt$Authors[c],';\npointOfContact:IOTC-Secretariat@fao.org;')))
    }
  }
  if(length(cc)>0){gs_edit_cells(xx,input=cc[1:(dim(cc)[1]),],anchor='E2',byrow=FALSE)}## <---------AE to improve : THIS WILL CHANGE IF YOU CHANGE THE COLUMNS IN THE GSHEET
  
  ## Description
  dd<-NULL; tmp=NULL
  iotc_filt[which(is.na(iotc_filt$Abstract)==TRUE),'Abstract']<-''
  for(c in 1:dim(iotc_filt)[1]){
    tmp<-iotc_filt$Abstract[c]
    tmp<-gsub(',','',tmp)
    if(is.na(tmp)==TRUE|tmp==''){
      dd<-as.data.frame(rbind(dd,paste0("abstract:Abstract not available;\npurpose:Some purpose for this metadata 2;\ninfo:some supplemental info 2")))
    }else{
      dd<-as.data.frame(rbind(dd,paste0("abstract:",tmp,";\npurpose:Abstract ;\ninfo:some supplemental info 2")))
    }
  }
  gs_edit_cells(xx,input=dd[1:(dim(dd)[1]),],anchor='C2',byrow=FALSE)## <---------AE to improve : THIS WILL CHANGE IF YOU CHANGE THE COLUMNS IN THE GSHEET
  
  ## Subject/Keywords
  kk<-NULL; tmp=NULL
  for(c in 1:dim(iotc_filt)[1]){
    tmp<-iotc_filt$All_taxonomy_terms[c]
    # tmp<-gsub(',','',tmp)
    if(tmp!=''){
      kk<-as.data.frame(rbind(kk,paste0("TH2:",tmp,";")))
    }else{kk<-as.data.frame(rbind(kk,paste0("TH2:NA;")))
    }
  }
  gs_edit_cells(xx,input=kk[1:(dim(kk)[1]),],anchor='D2',byrow=FALSE)## <---------AE to improve : THIS WILL CHANGE IF YOU CHANGE THE COLUMNS IN THE GSHEET
  
  return(xx)
} 















