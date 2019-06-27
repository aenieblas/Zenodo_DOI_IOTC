# filter_iotc_docs.R
# description: this script can be sourced for the functions to filter the IOTC documents such that they can be assigned DOIs
# 22/5/2019
# inputs : these scripts use df as their primary input. df is a data frame of iotc documents downloaded from the iotc website containing specific
# metadata, including 
# "Node.ID"            "Vid"                "DOI"                "Meeting"            "Meeting.session"    "Meeting.year"       
# "Type"        "Reference"          "Language"           "Title"              "Availability"       "Authors"            "Tags" 
# "All.taxonomy.terms"     "File"               "Path"               "Abstract"

# required actions : an extra column is added to the orginal IRD data frame : "IRD_reference", such that any changes to align the reference name to the
# selection criteria can be referenced back to the original reference name.

fill_empty_lang<-function(df){
  ## function to add languages to empty cells 1) by determining if there is a language identifier in the reference name and 
  ## 2) assuming non-indicated docs are in English (default lang)
  ## inputs : df is a dataframe of iotc documents as described above
  ## output : df with the empty language cells filled
  ## useage : iotc_dl<-fill_empty_lang(iotc_dl)
  
  # find empty language cells by determining if the reference name contains a language identifier. fill.
  empty_lang<-which(df$Language=='')
  df$Language[empty_lang][grep('\\[E\\]',df$Reference[empty_lang])]<-'English'
  df$Language[empty_lang][grep('\\[EN\\]',df$Reference[empty_lang])]<-'English'
  df$Language[empty_lang][grep('\\[F\\]',df$Reference[empty_lang])]<-'French'
  df$Language[empty_lang][grep('\\[FR\\]',df$Reference[empty_lang])]<-'French'
  
  # if no language identifier is available, fill empty language cells with English (default language)
  df[which(df$Language==''),'Language']<-'English'
  
  return(df)
}


strip_lang<-function(df){
  ## strip language identifiers from reference name
  ## inputs :  df is a data frame of iotc documents, but must have added the column "IRD_reference" to the df.
  ## useage : df$IRD_reference<-strip_lang(df)
  
  df$IRD_reference<-gsub('\\[E\\]','',df$IRD_reference)
  df$IRD_reference<-gsub('\\[EN\\]','',df$IRD_reference)
  df$IRD_reference<-gsub('\\[FR\\]','',df$IRD_reference)
  df$IRD_reference<-gsub('\\[F\\]','',df$IRD_reference)

  return(df$IRD_reference)
}

add_lang<-function(df_line,force_add=FALSE,doc_type='meeting_documents'){
  ## function to add a language identifier to the reference name
  ## inputs : df_line is a single row of the data.frame of iotc documents, as above
  ##        : doc_type is the iotc_doc_type[i], specifically added so that the -R is only added to meeting reports
  ## ouputs : the new reference name with a language identifier that matches the df_line$Language
  ## useage : df[x,'IRD_reference]<-add_lang(df[x,])
  if(doc_type=='meeting_reports'){
    if(force_add==FALSE){
      if(df_line$Language=='English' & length(grep('\\[',df_line$Reference))>0){new_reference_name<-paste(df_line$IRD_reference,'[EN]',sep='')
      return(new_reference_name)}
      if(df_line$Language=='French'& length(grep('\\[',df_line$Reference))>0){new_reference_name<-paste(df_line$IRD_reference,'[FR]',sep='')
      return(new_reference_name)
      }else{new_reference_name<-df_line$IRD_reference
      return(new_reference_name)}
    }
    
    if(force_add==TRUE){
      if(df_line$Language=='English'){new_reference_name<-paste(df_line$IRD_reference,'[EN]',sep='')
      return(new_reference_name)}
      if(df_line$Language=='French'){new_reference_name<-paste(df_line$IRD_reference,'[FR]',sep='')
      return(new_reference_name)
      }else{new_reference_name<-df_line$IRD_reference
      return(new_reference_name)}
    }
  }else{
    if(force_add==FALSE){
      if(df_line$Language=='English' & length(grep('\\[',df_line$Reference))>0){new_reference_name<-paste(df_line$IRD_reference,'[EN]',sep='')
      return(new_reference_name)}
      if(df_line$Language=='French'& length(grep('\\[',df_line$Reference))>0){new_reference_name<-paste(df_line$IRD_reference,'[FR]',sep='')
      return(new_reference_name)
      }else{new_reference_name<-df_line$IRD_reference
      return(new_reference_name)}
    }
    
    if(force_add==TRUE){
      if(df_line$Language=='English'){new_reference_name<-paste(df_line$IRD_reference,'[EN]',sep='')
      return(new_reference_name)}
      if(df_line$Language=='French'){new_reference_name<-paste(df_line$IRD_reference,'[FR]',sep='')
      return(new_reference_name)
      }else{new_reference_name<-df_line$IRD_reference
      return(new_reference_name)}
    }
}
}

add_lang_tmp<-function(df_line,force_add=FALSE,doc_type='meeting_documents'){
  ## function to add a language identifier to the reference name
  ## inputs : df_line is a single row of the data.frame of iotc documents, as above
  ##        : doc_type is the iotc_doc_type[i], specifically added so that the -R is only added to meeting reports
  ## ouputs : the new reference name with a language identifier that matches the df_line$Language
  ## useage : df[x,'IRD_reference_tmp]<-add_lang(df[x,])
  if(doc_type=='meeting_reports'){
    if(force_add==FALSE){
      if(df_line$Language=='English' & length(grep('\\[',df_line$Reference))>0){new_reference_name<-paste(df_line$IRD_reference_tmp,'[EN]',sep='')
      return(new_reference_name)}
      if(df_line$Language=='French'& length(grep('\\[',df_line$Reference))>0){new_reference_name<-paste(df_line$IRD_reference_tmp,'[FR]',sep='')
      return(new_reference_name)
      }else{new_reference_name<-df_line$IRD_reference_tmp
      return(new_reference_name)}
    }
    
    if(force_add==TRUE){
      if(df_line$Language=='English'){new_reference_name<-paste(df_line$IRD_reference_tmp,'[EN]',sep='')
      return(new_reference_name)}
      if(df_line$Language=='French'){new_reference_name<-paste(df_line$IRD_reference_tmp,'[FR]',sep='')
      return(new_reference_name)
      }else{new_reference_name<-df_line$IRD_reference_tmp
      return(new_reference_name)}
    }
  }else{
    if(force_add==FALSE){
      if(df_line$Language=='English' & length(grep('\\[',df_line$Reference))>0){new_reference_name<-paste(df_line$IRD_reference_tmp,'[EN]',sep='')
      return(new_reference_name)}
      if(df_line$Language=='French'& length(grep('\\[',df_line$Reference))>0){new_reference_name<-paste(df_line$IRD_reference_tmp,'[FR]',sep='')
      return(new_reference_name)
      }else{new_reference_name<-df_line$IRD_reference_tmp
      return(new_reference_name)}
    }
    
    if(force_add==TRUE){
      if(df_line$Language=='English'){new_reference_name<-paste(df_line$IRD_reference_tmp,'[EN]',sep='')
      return(new_reference_name)}
      if(df_line$Language=='French'){new_reference_name<-paste(df_line$IRD_reference_tmp,'[FR]',sep='')
      return(new_reference_name)
      }else{new_reference_name<-df_line$IRD_reference_tmp
      return(new_reference_name)}
    }
  }
}


reclass<-function(x,y,z,df,column){
  ## reclassify documents from a meeting type in meeting documents a different (already existing) document type
  ## inputs : x is a character string that matches the required iotc document type (doc_types below)
  ##          y is a character string for the search pattern 
  ##          z is a character string to classify the document type in the new df
  ##          df is a data.frame of meeting documents within which to search
  ## useage : iotc_docs<-reclass(x='executive_summaries',y='executive summar',z='Executive Summaries',df=iotc_docs)
  home<-paste0(wd,'source_csv/')
  doc_types<-c('meeting_reports','meeting_documents','executive_summaries','datasets','national_reports','guidelines', 'publications','expert_consultations')
  
  
  iotc_reclass<-read.csv(paste0(home,doc_types[which(doc_types==x)],'.csv'))
  
  ## two search terms
  if(length(y)==2){ ireclass<-intersect( grep(df[,column] , pattern = y[1],ignore.case = T) , grep(df[,column] , pattern = y[2],ignore.case = T) )
  }else{
    ireclass<-grep(y,df[,column],ignore.case = T) 
  }
  
  if(x=='executive_summaries'){
    if(length(grep('marsac',df[ireclass,'Authors']))>0){
      ireclass<-ireclass[-grep('marsac',df[ireclass,'Authors'],ignore.case = T)]
    }
    }
  ## add these documents to iotc_np file
  iotc_reclass<-rbind(df[ireclass,],iotc_reclass)
  iotc_reclass$Type<-z
  
  if(length(which(duplicated(iotc_reclass)==TRUE))>0){
    i_dupe<-which(duplicated(iotc_reclass)==TRUE)
    iotc_reclass<-iotc_reclass[-i_dupe,]}
  
  ## rewrite np file
  write.csv(iotc_reclass,file=paste0(x,'.csv'),row.names = F)
  
  if(length(ireclass)>0){df<-df[-ireclass,]
  }else{df<-df}
  return(df)
}


## iotc secretariat document filters 

## function to find the filters that have documents (i.e. non zero indices)
nonzero_filters<-function(x) if(dim(x)[2]>1){x}


secretariat<-function(df){
  ## function that filters out documents only of interest to the secretariat including: Agenda, Progress report, List of documents, Outcomes, 
  ## Recommendations, Report on the IOTC xxx, Capacity building, and Program of work
  ## inputs : df is a data frame of iotc documents, as above
  ## output : 'removals' is a dataframe with col1: indices of the documents to be removed from df, and col2: the selection criteria for this removal 
  ## useage : removals<- secretariat(iotc_docs)
  
  
  ## filter out Secretariat documents
  i_sec <-grep('IOTC Secretariat|Anonymous',df$Authors)
  i_sec <-unique(c(i_sec,which(df$Authors==''))) 
  
  filter=NULL
  filter<-rbind(nonzero_filters(cbind(grep('Agenda',df$Title[i_sec],ignore.case = T),'Agenda')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('Ordre du Jour',df$Title[i_sec],ignore.case = T),'Agenda')),filter)
  
  filter<-rbind(nonzero_filters(cbind(grep('Proposition de calendrier',df$Title[i_sec],ignore.case = T),'Proposed schedule')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('Proposed schedule of',df$Title[i_sec],ignore.case = T),'Proposed schedule')),filter)  
  
  # filter<-rbind(nonzero_filters(cbind(grep('Progress report of the Secretariat',df$Title,ignore.case = T),'Progress report')),filter)
  
  results<-grep('progress ',df$Title[i_sec],ignore.case = T) ## AE: ridiculously long process to get to the document number in the reference name.
  if(length(results)>0){info<-grep('INF',df$Reference[i_sec[results]],ignore.case=T)
  if(length(info)>0){results<-results[-info]}
  rr<-strip_lang(df[i_sec[results],])
  ss<-unlist(strsplit(rr,'-'))
  # tt<-unlist(ss)
  is<-seq(4,length(ss),by=4)
  xx<-unlist(strsplit(ss[is],' '))
  # xx<-gsub("[^0-9.-]", "", tt[is])
  if(length(which(na.omit(as.numeric(xx))>9))>0){results<-results[-(which(na.omit(as.numeric(xx))>9))]}
  if(length(results)>0){filter<-rbind(nonzero_filters(cbind(results,'Progress report')),filter)} }
  
  
  # filter<-rbind(nonzero_filters(cbind(grep('Progress ',df$Title[i_sec],ignore.case = T),'Progress report')),filter)#<------------------------this doesn't seem specific enough
  filter<-rbind(nonzero_filters(cbind(grep('Accomplis',df$Title[i_sec],ignore.case = T),'Progress report')),filter)
  
  filter<-rbind(nonzero_filters(cbind(grep('list of participants',df$Title[i_sec],ignore.case = T),'List of participants')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('list of participants',df$Reference[i_sec],ignore.case = T),'List of participants')),filter)
  
  filter<-rbind(nonzero_filters(cbind(grep('previous decisions',df$Title[i_sec],ignore.case = T),'Performance review')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('rieures de la commission',df$Title[i_sec],ignore.case = T),'Performance review')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('cisions de la Commission',df$Title[i_sec],ignore.case = T),'Performance review')),filter)
  
  filter<-rbind(nonzero_filters(cbind(grep('performance review',df$Title[i_sec],ignore.case = T),'Performance review')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('valuation des performances',df$Title[i_sec],ignore.case = T),'Performance review')),filter)
  
  doc_list<-c('List of documents','Liste des documents','Liste provisoire des documents','Lists of documents')  
  filter<-rbind(nonzero_filters(cbind(grep(paste(doc_list,collapse='|'),df$Title[i_sec],ignore.case = T),'List of documents')),filter)
   
  filter<-rbind(nonzero_filters(cbind(grep('Template for a resource Executive',df$Title[i_sec],ignore.case = T),'Template ES')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('TEMPLATE FOR RESOURCE EXECUTIV',df$Title[i_sec],ignore.case = T),'Template ES')),filter)
  
  filter<-rbind(nonzero_filters(cbind(grep('credentials',df$Title[i_sec],ignore.case = T),'Credentials')),filter)
  
  # filter<-rbind(nonzero_filters(cbind(grep('Outcomes of the ',df$Title,ignore.case = T),'Outcomes of the ')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('Outcome',df$Title[i_sec],ignore.case = T),'Outcomes of the ')),filter)
  
   
  results1<-grep('sultats ',df$Title[i_sec],ignore.case = T) ## AE: ridiculously long process to get to the document number in the reference name.
  if(length(results1)>0){info1<-grep('INF',df$Reference[i_sec[results1]],ignore.case=T)
  if(length(info1)>0){results1<-results1[-info1]}
  rr1<-strip_lang(df[i_sec[results1],])
  ss1<-unlist(strsplit(rr1,'-'))
  is1<-seq(4,length(ss1),by=4)
  xx1<-unlist(strsplit(ss1[is1],' '))
  if(length(which(na.omit(as.numeric(xx1))>9))>0){results1<-results1[-(which(na.omit(as.numeric(xx1))>9))]}
  if(length(results1)>0){filter<-rbind(nonzero_filters(cbind(results1,'Outcomes of the ')),filter)} }
  
  filter<-rbind(nonzero_filters(cbind(grep(' on the recommendation ',df$Title[i_sec],ignore.case = T), 'Recommendations')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('Recommendations from ',df$Title[i_sec],ignore.case = T), 'Recommendations')),filter)
  
  filter<-rbind(nonzero_filters(cbind(grep('Rules for the appointment',df$Title[i_sec],ignore.case = T), 'Rules of appointment')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('Règles pour la sélection',df$Title[i_sec],ignore.case = T), 'Rules of appointment')),filter)
  
  filter<-rbind(nonzero_filters(cbind(grep('Report on IOTC ',df$Title[i_sec],ignore.case = T), 'Report on IOTC ')),filter)
  
  filter<-rbind(nonzero_filters(cbind(grep('Rapport du Secrétariat',df$Title[i_sec],ignore.case = T), 'Report of the Secretariat ')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('Report of the Secretariat',df$Title[i_sec],ignore.case = T), 'Report of the Secretariat')),filter)
  
  filter<-rbind(nonzero_filters(cbind(grep('capacity building',df$Title[i_sec],ignore.case = T), 'capacity building')),filter)
  
  pow<-c('Program of Work','Programme of Work','Programme de travail','work plan','Etat des PAN')
  filter<-rbind(nonzero_filters(cbind(grep(paste(pow,collapse='|'),df$Title[i_sec],ignore.case = T), 'Program of Work')),filter)
  filter<-rbind(nonzero_filters(cbind(grep('PoW',df$Title[i_sec],ignore.case = F), 'Program of Work')),filter)
  
  com<-c('finance','technique sur les crit','Technical Committee on Allocation Criteria','Administration')
  filter<-rbind(nonzero_filters(cbind(grep(paste(com,collapse='|'),df$Title[i_sec],ignore.case = T), 'Commission')),filter)
    
    y<-c('me session du comit', 'application')
    
  filter<-rbind(nonzero_filters(cbind(intersect( grep(df[i_sec,'Title'] , pattern = y[1],ignore.case = T) , grep(df[i_sec,'Title'] , pattern = y[2],ignore.case = T)),'Commission')),filter)
  
   # reassign secretariat documents into this meeting's documents
  filter[,1]<-i_sec[as.numeric(filter[,1])]
  
  
  # removals<-filter[order(as.numeric(filter[,1])),] ## AE make a function for the Secretariat filters
  removals<-data.frame(Index=as.numeric(filter[,1]),Filter_used=as.character(filter[,2])) 
  
  return(removals)
}



## duplicates ##


metadata_difference<-function(duplicates){
  ## function to find the metadata that are different between duplicated documents (based on reference name)
  ## inputs : duplicates is a data frame of duplicated documents,as in duplicated(df$Reference)
  
  # tt<-as.data.frame(apply(duplicates,2,function(y) if(anyDuplicated(y)) NA else y[which.min(is.na(y))]))
  tt<-as.data.frame(apply(duplicates,2,function(y) if(length(which(duplicated(y)==TRUE))==(length(y)-1)) NA else y[which.min(is.na(y))]))
  
  
  diff_cols<-rownames(tt)
  diff_cols<-diff_cols[which(is.na(tt)==FALSE)]
  
  ## ignore differences in Node.ID, Vid, All.taxonomy.terms, File, Path
  ignore<-c('Node.ID','Vid','All.taxonomy.terms','File','Path','DOI','Tags','IRD_reference')
  
  meta_diffs<-diff_cols[!(diff_cols %in% ignore)]
  return(meta_diffs)
}



meeting_session<-function(df,action='indices'){
  ## function to identify whether the duplicate reference name is due to different meeting sessions. 
  ## find the most recent doc, remove the others. rename Meeting.Session with correct meeting session.
  ## inputs : duplicates is a data frame of duplicated documents,as in duplicated(df$Reference)
  ## output : 'removals' is a dataframe with col1: indices of the documents to be removed from df, and col2: the selection criteria for this removal
  library(stringr)  
  if(length(which(colnames(df)=='IRD_reference'))==0){df$IRD_reference<-df$Reference}
  i_remove<-NULL
  metadata_to_print<-c(5,8,9, 10, 11)#'Reference','Title','Language','Meeting session','Availability')
  
  d <- df[duplicated(df$Reference),'Reference']
  m <- match(df$Reference,d)
  if(length(which(!is.na(m)))>0){
    for(x in 1:max(m,na.rm=T)){# for each match set, find the differences between the entries
      id<-which(m==x)  # the match set
      # print(x)
      meta_diffs<-metadata_difference(df[id,]) # what metadata are different between these duplicates?
      if(length(which(meta_diffs=='Language'))==0){
        if(length(which(meta_diffs=='Meeting.session'))>0){ 
          print(paste(x,': yes meeting session '))
          
          # find most recent document
          ref_dates<-as.Date(as.character(df[id,'Availability']), format = "%Y-%m-%d")  ## keep most recent date for each language
          
          
          #find most recent date of the id indices
          recent<-max(ref_dates)
          rd<-which(ref_dates==recent)
          old<-which(ref_dates!=recent)
          
          # find meeting session of the reference name
          ref_meetingsession <- as.numeric(str_extract(unlist(strsplit(as.character(unique(df$Reference[id[rd]])),'-'))[3], "[0-9]+"))
          
          # which meeting session DOES NOT match the reference name meeting session?
          if(length(id[rd])>1){
            if(length(which(df[id[rd],'Meeting.session']!=ref_meetingsession))>0){
              is<-id[rd]
              remove<-is[which(df[is,'Meeting.session']!=ref_meetingsession)]
              i_remove<-c(i_remove,remove)
              
              print(paste('kept ',is[-which(df[is,'Meeting.session']!=ref_meetingsession)],':',paste(df[is[-which(df[is,'Meeting.session']!=ref_meetingsession)],metadata_to_print],collapse=';')))
              print(paste('removed ',remove,':',paste(df[remove,metadata_to_print],collapse=';')))
              # i_remove<-c(i_remove,id[which(df[id[rd],'Meeting.session']!=ref_meetingsession)])
              # df<-df[-which(df[id,'Meeting.session']==ref_meetingsession),]
            } }else{
              # df[id[rd],'Meeting.session']<-  ref_meetingsession
              i_remove<-c(i_remove,id[old])
              print(paste('kept ',id[rd],':',paste(df[id[rd],metadata_to_print],collapse=';')))
              print(paste('removed ',id[old],':',paste(df[id[old],metadata_to_print],collapse=';')))
            }
          
        }    
      }
      # else{df<-df}
    }  
  }
  # if(length(i_remove)>0){df<-df[-i_remove,]}
  if(action=='indices'){# return(df)
    
    if(length(i_remove)>0){  removals<-data.frame(Index=as.numeric(i_remove),Filter_used='Meeting session') 
    return(removals)}
  }
  
  
  if(action=='change.df'){
    return(df)
  }
  
  # return(i_remove)
}


meeting<-function(df,action='indices'){
  ## function to identify whether the duplicate reference name is due to different meetings. If one of the meeting is the scientific committee;
  ## it can be assumed that the report is a complete duplicate for the scientific committee meeting and those can be removed.
  
  if(length(which(colnames(df)=='IRD_reference'))==0){df$IRD_reference<-df$Reference}
  i_remove<-NULL
  metadata_to_print<-c(4,8,9, 10, 11,18)#'Reference','Title','Language','Meeting session','Availability')
  
  df[,'IRD_reference']<-strip_lang(df)
  d <- df[duplicated(df$IRD_reference),'IRD_reference']
  m <- match(df$IRD_reference,d)
  if(length(which(!is.na(m)))>0){
    for(x in 1:max(m,na.rm=T)){# for each match set, find the differences between the entries
      id<-which(m==x)  # the match set
      
      meta_diffs<-metadata_difference(df[id,]) # what metadata are different between these duplicates?
      
      
      if(length(which(meta_diffs=='Meeting'))>0){
        if(length(unique(df[id,'Meeting']))>1 & length(grep('Scientific committee',unique(df[id,'Meeting']),ignore.case=T))==1){
          print(paste(x,' at least one scientific committee doc double'))
          i_sc<-id[grep('Scientific committee',df[id,'Meeting'],ignore.case=T)]
          i_remove<-c(i_remove,i_sc)
          
          print(paste('kept ',paste(id[-grep('Scientific committee',df[id,'Meeting'],ignore.case=T)],collapse=':'),': ',paste(df[id[-grep('Scientific committee',df[id,'Meeting'],ignore.case=T)],metadata_to_print],collapse=' '),collapse = '/n'))
          print(paste('removed ',paste(i_sc,collapse=':'),': ',paste(df[i_sc,metadata_to_print],collapse=' '),collapse = '/n'))
          
        }
      }
    }
  }
  # 
  if(action=='change.df'){return(df)}
  
  if(action=='activate.filter'){
    if(length(i_remove)>0){df<-df[-i_remove,]
    return(df)
    }else{return(df)}
  }
  # return(i_remove)
  
  if(action=='indices'){
    if(length(i_remove)>0){removals<-data.frame(Index=as.numeric(i_remove),Filter_used='Meeting duplicate (SC)') 
    return(removals)}}
  
}



date_availability<-function(df,action){
  library(stringr)
  if(length(which(colnames(df)=='IRD_reference'))==0){df$IRD_reference<-df$Reference}
  i_remove<-NULL
  metadata_to_print<-c(4,5,8,9, 10, 11,18)#'Reference','Title','Language','Meeting session','Availability')
  
  # df[,'IRD_reference']<-strip_lang(df)
  d <- df[duplicated(df$IRD_reference),'IRD_reference']
  m <- match(df$IRD_reference,d)
  if(length(which(!is.na(m)))>0){
    for(x in 1:max(m,na.rm=T)){# for each match set, find the differences between the entries
      id<-which(m==x)  # the match set
      
      meta_diffs<-metadata_difference(df[id,]) # what metadata are different between these duplicates?
      
      if(length(which(meta_diffs=='Availability'))>0){
        print(paste(x,': DATE DUPLICATES'))
        ref_dates<-as.Date(as.character(df[id,'Availability']), format = "%Y-%m-%d")  ## keep most recent date for each language
        
        
        #find most recent date of the id indices
        recent<-max(ref_dates)
        rd<-which(ref_dates==recent)
        old<-which(ref_dates!=recent)
        
        ## keep most recent, remove old
        i_remove<-c(i_remove,id[old])
        
        print(paste('kept ',paste(id[rd],collapse=':'),': ',paste(df[id[rd],metadata_to_print],collapse=' '),collapse = '/n'))
        print(paste('removed ',paste(id[old],collapse=':'),': ',paste(df[id[old],metadata_to_print],collapse=' '),collapse = '/n'))
        
        if(action=='change.df'){return(df)}
        
        if(action=='activate.filter'){
          if(length(i_remove)>0){df<-df[-i_remove,]
          return(df)
          }else{return(df)}
        }
        # return(i_remove)
        
        if(action=='indices'){
          if(length(i_remove)>0){removals<-data.frame(Index=as.numeric(i_remove),Filter_used='Date duplicate') 
          return(removals)}}
      }
    }
  }
}



language<-function(df,action,doc_type){
  ## function to identify whether the duplicate reference name is due to different languages, if so, the most recent doc of each lang is kept
  ## inputs : df is a data frame of duplicated documents,as in duplicated(df$Reference)
  ##          action is a character string to indicate the outputs that are required 
  ## outputs : options based on the 'action' taken above and including: 
  ##           'change.df' (to return the full df with an extra column - IRD_reference - of reference names with the correct language identifier), 
  ##           'activate.filter' (which returns the df with indices removed where languages are duplicated), and 
  ##           'indices' is a dataframe with col1: indices of the documents to be removed from df, and col2: the selection criteria for this removal
  
  
  
  library(stringr)
  if(length(which(colnames(df)=='IRD_reference'))==0){df$IRD_reference<-df$Reference}
  i_remove<-NULL
  metadata_to_print<-c(4,5,8,9, 10, 11,18)#'Reference','Title','Language','Meeting session','Availability')
  
  df[,'IRD_reference']<-strip_lang(df)
  d <- df[duplicated(df$IRD_reference),'IRD_reference']
  m <- match(df$IRD_reference,d)
  if(length(which(!is.na(m)))>0){
    for(x in 1:max(m,na.rm=T)){# for each match set, find the differences between the entries
      id<-which(m==x)  # the match set
      
      meta_diffs<-metadata_difference(df[id,]) # what metadata are different between these duplicates?
      
      if(length(which(meta_diffs=='Language'))>0){
        
        if(length(unique(df[id,'Language']))!=length(df[id,'Language'])){           ## are there more than one of each language?
          
          print(paste(x,': MULTIPLE LANGUAGE DUPLICATES'))
          ref_dates<-as.Date(as.character(df[id,'Availability']), format = "%Y-%m-%d")  ## keep most recent date for each language
          
          
          #find most recent date of the id indices
          recent<-max(ref_dates)
          rd<-which(ref_dates==recent)
          old<-which(ref_dates!=recent)
          
          ## are the most recent dates also the different refs with diff languages?
          if(length(unique(df[id[rd],'Language']))==length(unique(df[id,'Language']))){
            print(paste0(x,' the different languages are found on the most recent date '))
            
            # replace any language identifier in reference name with that which aligns to the language in the Language column, i.e. [E],[EN],[F], or [FR]
            df[id,'IRD_reference']<-strip_lang(df[id,]) # first strip any language identifier already present in the reference name and put in IRD_reference column
            for(i in 1:length(id)){df[id[i],'IRD_reference']<-add_lang(df[id[i],],force_add=TRUE,doc_type=doc_type)} # add correct lang id to ref name in the IRD_reference column
            
            if(length(old)>0){ i_remove<-c(i_remove,id[old])}else{
              print(paste('FLAGGED ',paste(id[old],collapse=':'),': ',paste(df[id[old],metadata_to_print],collapse=' '),collapse = '/n'))
            }
            
            print(paste('kept ',paste(id[rd],collapse=':'),': ',paste(df[id[rd],metadata_to_print],collapse=' '),collapse = '/n'))
            print(paste('removed ',paste(id[old],collapse=':'),': ',paste(df[id[old],metadata_to_print],collapse=' '),collapse = '/n'))
            
          }else{
            print(paste0(x,': different language per date...'))
            ## keep the most recent version of each language
            
            # replace any language identifier in reference name with that which aligns to the language in the Language column, i.e. [E],[EN],[F], or [FR]
            # df[id,'IRD_reference']<-strip_lang(df[id,]) # first strip any language identifier already present in the reference name and put in IRD_reference column
            for(i in 1:length(id)){df[id[i],'IRD_reference']<-add_lang(df[id[i],],doc_type = doc_type)} # add correct lang id to ref name in the IRD_reference column
            
            # same_lang_old<-which(df[id[old],'Language']==df[id[rd],'Language'])# alternatively, could sort by dates, then language. ...
            same_lang_old<-which(df[id[old],'Language']==df[id[rd],'Language'])
            if(length(same_lang_old)==0){
              print(paste(x,' FLAGGED:  no obvious language difference in these duplicates'))
              print(paste('kept ',paste(id[rd],collapse=':'),': ',paste(df[id[rd],metadata_to_print],collapse=' '),collapse = '/n'))
              
              print(paste('FLAGGED ',paste(id[old],collapse=':'),': ',paste(df[id[old],metadata_to_print],collapse=' '),collapse = '/n'))
              
              df[id,'IRD_reference']<-strip_lang(df[id,]) # first strip any language identifier already present in the reference name and put in IRD_reference column
              for(i in 1:length(id)){df[id[i],'IRD_reference']<-add_lang(df[id[i],],force_add=TRUE,doc_type = doc_type)} # add correct lang id to ref name in the IRD_reference column
              
              # print(df[id[old],metadata_to_print])
            }else{
              i_remove<-c(i_remove,id[old[same_lang_old]])
              other_lang_old<-which(df[id[old],'Language']!=df[id[rd],'Language'])
              
              df[id,'IRD_reference']<-strip_lang(df[id,]) # first strip any language identifier already present in the reference name and put in IRD_reference column
              for(i in 1:length(id)){df[id[i],'IRD_reference']<-add_lang(df[id[i],],force_add=TRUE,doc_type = doc_type)} # add correct lang id to ref name in the IRD_reference column
              
              
              # i_id<-seq(1,length(id),by=1)
              # remove<-i_id[!(i_id %in% c(other_lang_old,rd))]
              # i_remove<-c(i_remove,id[remove])
              
              # i_remove<-c(i_remove,id[old[same_lang_old]])
              
              # print(paste('kept ',paste(id[rd],collapse=':'),': ',paste(df[id[rd],metadata_to_print],collapse=' '),collapse = '/n'))
              print(paste('kept ',paste(id[rd],collapse=':'),': ',paste(df[id[rd],metadata_to_print],collapse=' '),collapse = '/n'))
              print(paste('kept ',paste(id[old[other_lang_old]],collapse=':'),': ',paste(df[id[old[other_lang_old]],metadata_to_print],collapse=' '),collapse = '/n'))
              
              print(paste('removed ',paste(id[old[same_lang_old]],collapse=':'),': ',paste(df[id[old[same_lang_old]],metadata_to_print],collapse=' '),collapse = '/n'))
            }
            
            # df<-df[-id[remove],]
          }
          ## old rule: only keep the entries with the most recent date
          # if(length(old)>0){ df<-df[-id[old],]}
          
        }else{## assign new reference name with correct naming convention for language
          #1) replace any language identifier in reference name with that which aligns to the language in the Language column, i.e. [E],[EN],[F], or [FR]
          
          df[id,'IRD_reference']<-strip_lang(df[id,]) # first strip any language identifier already present in the reference name and put in IRD_reference column
          for(i in 1:length(id)){df[id[i],'IRD_reference']<-add_lang(df[id[i],],force_add=TRUE,doc_type=doc_type)} # add correct lang id to ref name in the IRD_reference column
          # print(df[id,'IRD_reference'])
        }
      }
    }}
  
  
  # 
  if(action=='change.df'){return(df)}
  
  if(action=='activate.filter'){
    if(length(i_remove)>0){df<-df[-i_remove,]
    return(df)
    }else{return(df)}
  }
  # return(i_remove)
  
  if(action=='indices'){
    if(length(i_remove)>0){removals<-data.frame(Index=as.numeric(i_remove),Filter_used='Language duplicate') 
    return(removals)}}
  
}


authors<-function(df,reference,action){
  ## this funcion identifies whether the cause of duplication of the reference name is due to Authors 
  ## inputs: df is the data frame of iotc documents
  ## output : filtered df
  
  library(stringr)
  library(RecordLinkage)
  if(length(which(colnames(df)=='IRD_reference'))==0){df$IRD_reference<-df$Reference}
  i_remove<-NULL
  metadata_to_print<-c(5,8,9, 10, 11,12)#'Reference','Title','Language','Meeting session','Availability')
  
  d <- df[duplicated(eval(parse(text=paste0('df$',reference)))),reference]
  m <- match(eval(parse(text=paste0('df$',reference))),d)
  
  if(length(which(is.na(m)==FALSE))>0){
    for(x in 1:max(m,na.rm=T)){# for each match set, find the differences between the entries
      id<-which(m==x)  # the match set
      
      meta_diffs<-metadata_difference(df[id,]) # what metadata are different between these duplicates?
      
        if(length(which(meta_diffs=='Authors'))>0){
          print(paste(x,': duplicate difference in authors!'))
          if(length(which(meta_diffs=='Language'))>0){print(paste(x,': FYI there is also a LANGUAGE difference!'))}
          #find most recent date of the id indices
          ref_dates<-as.Date(as.character(df[id,'Availability']), format = "%Y-%m-%d")  ## keep most recent date for each language
          
          recent<-max(ref_dates)
          rd<-which(ref_dates==recent)
          old<-which(ref_dates!=recent)
          
          if(length(rd)==1){i_remove<-c(i_remove,id[old]) ## there is only one documen with the most recent date (keep most recent)
          print(paste('to keep ',paste(id[rd],collapse=':'),': ',paste(df[id[rd],metadata_to_print],collapse='/'),collapse = ' '))
          print(paste('flagged for removal ',paste(id[old],collapse=':'),': ',paste(df[id[old],metadata_to_print],collapse='/'),collapse = ''))
          
          }
          if(length(rd)>1){## there is more than one document with the most recent date (further filtering required)
            
            if(length(grep('Secretariat',df[id[rd],'Authors'],ignore.case=T))>0){
              secr<-grep('Secretariat',df[id[rd],'Authors'],ignore.case=T)
              other<-id[rd[-secr]]
              if(df[other,'Authors']==''){df[other,'Authors']=='IOTC Secretariat'
                print(paste(x,' FLAGGED TO CHANGE DF : AUTHOR DUPLICATE SHOULD BE ASSIGNED TO SECRETARIAT'))}
              if(length(grep('Anon',df[other,'Authors'],ignore.case = T))>0){df[other,'Authors']=='IOTC Secretariat'
                print(paste(x,' FLAGGED TO CHANGE DF : AUTHOR DUPLICATE SHOULD BE ASSIGNED TO SECRETARIAT'))}
            }
            
            
            ## find similarity score between strings
            if(length(id[rd])==2){similarity<-levenshteinSim(df[id[rd][1],'Authors'],df[id[rd][2],'Authors'])}
            if(length(id[rd])==3){similarity<-levenshteinSim(df[id[rd][1],'Authors'],df[id[rd][2],'Authors'],df[id[rd][3],'Authors'])}
            
            if(similarity>0.7){print(paste('good chance these are the same author for ',paste(df[id[rd],'Reference'])," : ",paste(df[id[rd],'Authors'],collapse=' - OR - ')))
              df[id[rd],'Authors']<-df[id[rd][1],'Authors']}
            if(similarity>0.5 & similarity<0.7){print(paste('look more closely at these authors for ',paste(df[id[rd],'Reference'])," : ",paste(df[id[rd],'Authors'],collapse=' - OR - ')))}
            if(similarity<0.5){print(paste('probably not same author for ',paste(df[id[rd],'Reference'])," : ",paste(df[id[rd],'Authors'],collapse=' - OR - ')))}
          }
        # }
      }
    }
  }
  if(action=='change.df'){return(df)}## note : here, nothing changes. just returns flag of authors and the original df
  
  if(action=='activate.filter'){
    if(length(i_remove)>0){df<-df[-i_remove,]
    return(df)
    }else{return(df)}
  }
  # return(i_remove)
  
  if(action=='indices'){
    if(length(i_remove)>0){removals<-data.frame(Index=as.numeric(i_remove),Filter_used='Author duplicate') 
    return(removals)}}
  
}



abstract<-function(df,reference){
  ## this funcion identifies whether the cause of duplication of the reference name is due to Authors 
  ## inputs: df is the data frame of iotc documents
  ## output : filtered df
  
  if(length(which(colnames(df)=='IRD_reference'))==0){df$IRD_reference<-df$Reference}
  i_remove<-NULL
  metadata_to_print<-c(5,8,9, 10, 11,17)#'Reference','Title','Language','Meeting session','Availability','Abstract')
  
  
  d <- df[duplicated(eval(parse(text=paste0('df$',reference)))),reference]
  m <- match(eval(parse(text=paste0('df$',reference))),d)
  
  if(length(which(is.na(m)==FALSE))>0){
    for(x in 1:max(m,na.rm=T)){# for each match set, find the differences between the entries
      id<-which(m==x)  # the match set
      
      meta_diffs<-metadata_difference(df[id,]) # what metadata are different between these duplicates?
      if(length(which(meta_diffs=='Language'))==0){
        if(length(which(meta_diffs=='Abstract'))>0){
          print(paste(x,': duplicate difference in abstract!'))
          
          #find most recent date of the id indices
          ref_dates<-as.Date(as.character(df[id,'Availability']), format = "%Y-%m-%d")  ## keep most recent date for each language
          
          recent<-max(ref_dates)
          rd<-which(ref_dates==recent)
          old<-which(ref_dates!=recent)
          
          # if there is just one recent index, remove all others
          if(length(rd)==1){i_remove<-c(i_remove,id[old])
          print(paste('kept ',paste(id[rd],collapse=':'),': ',paste(df[id[rd],metadata_to_print],collapse='/'),collapse = ' '))
          print(paste('removed ',paste(id[old],collapse=':'),': ',paste(df[id[old],metadata_to_print],collapse='/'),collapse = ''))
          
          }
          #if there is more than one of the most recent indices, keep those with an abstract. remove others
          if(length(rd)>1){i_remove<-c(i_remove,c(id[rd[which(nchar(df[id[rd],'Abstract'])==0)]],id[old]))
          i_kept<-id[rd[which(nchar(df[id[rd],'Abstract'])!=0)]]
          
          print(paste('kept ',paste(i_kept,collapse=':'),': ',paste(df[i_kept,metadata_to_print],collapse='/'),collapse = ' '))
          print(paste('removed ',paste(id[rd[which(nchar(df[id[rd],'Abstract'])==0)]],collapse=':'),': ',paste(df[id[rd[which(nchar(df[id[rd],'Abstract'])==0)]],metadata_to_print],collapse='/'),collapse = ''))
          
          }
        }
      }
    }
  }
  if(length(i_remove>0)){
    removals<-data.frame(Index=as.numeric(i_remove),Filter_used='Abstract duplicate') 
    return(removals)
  }
}


