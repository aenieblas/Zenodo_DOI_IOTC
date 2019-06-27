## check pdfs against titles

##1) read pdf.
check_files<-function(DF){
# DF<-iotc_filt;#iotc_dl; #iotc_filt

library(pdftools)
library(RecordLinkage)
library(qdapTools)

flags<-NULL; docx<-NULL
for(f in 23:dim(DF)[1]){
  
  # for (l in 10:length(flags)){
  # f<-flags[l]
  # temp <- tempfile()
  # download.file("http://www.newcl.org/data/zipfiles/a1.zip",temp)
  # data <- read.table(unz(temp, "a1.dat"))
  # unlink(temp)
  
  
  if(length(grep('\\.zip',DF$File[f]))>0){
    print('ZIP FILE')
    flags<-c(flags,f)
    # zip.file.extract(temp, zipname =DF$File[f] , unzip = getOption("unzip"))
  }else{
    
    download.file(DF$File[f],'temp_try')
    
    if(length(grep('\\.doc',DF$File[f]))>0){docx<-c(docx,f);
    text<-read_docx('temp_try')}
    
    # download.file(DF$File[f],'temp_try')
    if(length(grep('\\.pdf',DF$File[f]))>0){text <- pdf_text("temp_try")}
    
    text2 <- strsplit(text, "\n")
    text2 <- gsub('–','-',text2)
    text2 <- gsub('\\[','',text2)
    text2 <- gsub('\\]','',text2)
    text2<-gsub('\\(','',text2)
    text2<-gsub('\\)','',text2)
    
    ref<-gsub('–','-',DF$Reference[f])
    ref<-gsub('\\[','',ref)
    ref<-gsub('\\]','',ref)
    ref<-gsub('\\(','',ref)
    ref<-gsub('\\)','',ref)
    ref<-gsub('_Rev1','',ref)
    ref<-gsub('_Rev_1','',ref)
    yes_match<-grep(ref,text2)
    
    # similarity<-levenshteinSim(DF$Reference[f]],'IOTC-2011-WPTT13-R\\[F\\]')
    
    if(length(yes_match)>0){print(paste(f,':', DF$Identifier[f],'/',DF$Reference[f],': ',text2[yes_match[1]]))}
    
    text2[1]
    DF[f,'Title']
    
    lang_change<-NULL
    title_check<-NULL
    if(length(yes_match)==0 & length(grep('Identifier',colnames(DF))>0)){
      lang_change<- grep(DF$Identifier[f],text2)
      if(length(lang_change)>0){print(paste(f,': CHANGE in Identifier - yes match'))}
    }
    ## check title
    if(is.null(lang_change) |length(lang_change)==0){
      title_check<-grep(DF$Title[f],text2,ignore.case = T)
      print(paste(DF$Reference[f],paste(DF$Title[f],text2[1])))
      if(length(title_check)>0){print(paste(f,': Title checks out'))}
    }
    # }
    
    
    if(length(yes_match)==0 & length(lang_change)==0 & length(title_check)==0){flags<-c(flags,f)}else{print(paste('YES! MATCH!'))}
    
    
    question1 <- readline("Check next file? (Y/N)")
    if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
      continue = TRUE
      next
    } else if (regexpr(question1, 'n', ignore.case = TRUE) == 1){
      question2 <- readline("Would you like to set another criteria? (Y/N)")
      if(regexpr(question2, 'y', ignore.case = TRUE) == 1){
        some_criteria <-  readline("Enter the new criteria:")
        continue = FALSE
      } else {
        break  
      }
    }
  }
}
}


### CHECKING THE FLAGS!
for (l in 24:length(unique(flags))){
  flags<-unique(flags)
  f<-flags[l]
  # temp <- tempfile()
  # download.file("http://www.newcl.org/data/zipfiles/a1.zip",temp)
  # data <- read.table(unz(temp, "a1.dat"))
  # unlink(temp)
  
  
  if(length(grep('\\.zip',DF$File[f]))>0){
    print('ZIP FILE')
    flags<-c(flags,f)
    # zip.file.extract(temp, zipname =DF$File[f] , unzip = getOption("unzip"))
  }else{
    
    download.file(DF$File[f],'temp_try')
    
    if(length(grep('\\.doc',DF$File[f]))>0){docx<-c(docx,f);
    text<-read_docx('temp_try')}
    
    # download.file(DF$File[f],'temp_try')
    if(length(grep('\\.pdf',DF$File[f]))>0){text <- pdf_text("temp_try")}
    
    text2 <- strsplit(text, "\n")
    text2 <- gsub('–','-',text2)
    text2 <- gsub('\\[','',text2)
    text2 <- gsub('\\]','',text2)
    text2<-gsub('\\(','',text2)
    text2<-gsub('\\)','',text2)
    
    ref<-gsub('–','-',DF$Reference[f])
    ref<-gsub('\\[','',ref)
    ref<-gsub('\\]','',ref)
    ref<-gsub('\\(','',ref)
    ref<-gsub('\\)','',ref)
    ref<-gsub('_Rev1','',ref)
    ref<-gsub('_Rev_1','',ref)
    yes_match<-grep(ref,text2)
    
    # similarity<-levenshteinSim(DF$Reference[f]],'IOTC-2011-WPTT13-R\\[F\\]')
    
    if(length(yes_match)>0){print(paste(f,':', DF$Identifier[f],'/',DF$Reference[f],': ',text2[yes_match[1]]))}
    
    text2[1]
    DF[f,'Title']
    
    lang_change<-NULL
    title_check<-NULL
    if(length(yes_match)==0 & length(grep('Identifier',colnames(DF))>0)){
      lang_change<- grep(DF$Identifier[f],text2,ignore.case = T)
      if(length(lang_change)>0){print(paste('CHANGE in Identifier - yes match'))}
    }
    ## check title
    if(is.null(lang_change) |length(lang_change)==0){
      title_check<-grep(DF$Title[f],text2,ignore.case = T)
      print(paste(DF$Reference[f],paste(DF$Title[f],text2[1])))
      if(length(title_check)>0){print(paste(f,'Title checks out'))}
    }
    # }
    
    
    if(length(yes_match)==0 & length(lang_change)==0 & length(title_check)==0){flags<-c(flags,f)}else{print(paste('YES! MATCH!'))}
  }
}

## EXPERT CONSULTATIONS
# Ok : 1,


## NAT report 
## 6 - IOTC/CTOI

# l=3,4,6 9,11,12 (ok) ...
