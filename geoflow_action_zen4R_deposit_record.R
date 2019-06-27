zen4R_deposit_record <- function(entity, config, options){
  library(tools)
  if(!require("zen4R")){
    stop("This action requires the 'zen4R' package")
  }
  
  ZENODO <- config$software$output$zenodo
  
  if(is.null(ZENODO)){
    errMsg <- "This action requires the Zenodo API to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  
  #options
  depositWithFiles <- if(!is.null(options$depositWithFiles)) options$depositWithFiles else FALSE
  publish <- if(!is.null(options$publish) & depositWithFiles) options$publish else FALSE
  deleteOldFiles <- if(!is.null(options$deleteOldFiles)) options$deleteOldFiles else TRUE
  communities <- if(!is.null(options$communities)) options$communities else NULL
  
  #create empty record
  #how to deal with existing records / new versions
  #this approach that the Zenodo record has a related identifier as URN
  #e.g. urn:my-metadata-identifier
  
  zenodo_metadata <- NULL
  deposits <- ZENODO$getDepositions()
  if(length(deposits)>0){
    invisible(lapply(deposits, function(deposit){
      related_identifiers <- deposit$metadata$related_identifiers
      if(!is.null(related_identifiers)){
        for(related_identifier in related_identifiers){
          if(startsWith(related_identifier$identifier,"urn")){
            related_id <- unlist(strsplit(related_identifier$identifier, "urn:"))[2]
            if(related_id == entity$identifiers[["id"]] &
               related_identifier$relation == "isIdenticalTo"){
              zenodo_metadata <<- deposit
              break
            }
          }
        }
      }
    }))
  }
  
  
  # if(is.null(zenodo_metadata)){
  #   zenodo_metadata <- ZENODO$createEmptyRecord()
  #   # zenodo_metadata$addRelatedIdentifier("isAlternateIdentifier", entity$identifiers[["id"]])
  #   # if(publish==TRUE){
  #     zenodo_metadata$addRelatedIdentifier("isAlternateIdentifier", entity$identifiers[["id"]])
  #     # zenodo_metadata$addRelatedIdentifier("isIdenticalTo", entity$relations[[1]]$link)
  #   # }
  # }
  
  if(is.null(zenodo_metadata)){
    zenodo_metadata <- ZENODO$createEmptyRecord()
    zenodo_metadata$addRelatedIdentifier("isIdenticalTo", paste("urn", entity$identifiers[["id"]], sep=":"))
    zenodo_metadata$addRelatedIdentifier("isAlternateIdentifier", entity$relations[[1]]$link)
  }
  
  # if(is.null(zenodo_metadata)){     
  #   zenodo_metadata <- ZENODO$createEmptyRecord()     
  # zenodo_metadata$addRelatedIdentifier("isIdenticalTo", paste("urn", entity$identifiers[["id"]], sep=":"))     
  # zenodo_metadata$addRelatedIdentifier("isAlternateIdentifier", 'https://iotc.org/')   }
  
  
  doi <- zenodo_metadata$metadata$prereserve_doi$doi
  #if entity already comes with a DOI, we set it (this might be a preset DOI from Zenodo or elsewhere)
  if(!is.null(entity$identifiers[["doi"]])){
    doi <- entity$identifiers[["doi"]]
  }
  if(regexpr("zenodo", doi)<0) zenodo_metadata$setDOI(doi)
  
  #basic record description
  zenodo_metadata$setTitle(entity$title)
  zenodo_metadata$setDescription(entity$descriptions[["abstract"]])
  zenodo_metadata$setLanguage(entity$language)
  
  if(publish==TRUE){
    for(subject in entity$subjects){
      kwds <- c()
      for(kwd in subject$keywords){
        # zenodo_metadata$addKeyword(kwd)
        kwds <- c(kwds, kwd$name)
      }
      config$logger.info(kwds)
      zenodo_metadata$setKeywords(kwds)
    }
  }
  
  #upload type
  #TODO think on how to map upload types between Dublin core, ISO/OGC metadata, Zenodo  
  # zenodo_metadata$setUploadType(entity$type)
  if(entity$type=='dataset'|entity$type=='poster'|entity$type=='presentation'|entity$type=='video'|entity$type=='software'|entity$type=='lesson'|entity$type=='other'){
    zenodo_metadata$setUploadType(entity$type)
  }else{
    if(entity$type=='book'|entity$type=='section'|entity$type=='conferencepaper'|entity$type=='article'|entity$type=='patent'|entity$type=='preprint'|entity$type=='report'|entity$type=='softwaredocumentation'|entity$type=='thesis'|entity$type=='technicalnote'|entity$type=='workingpaper'){
      zenodo_metadata$setPublicationType(entity$type)
    }
    if(entity$type=='figure'|entity$type=='plot'|entity$type=='drawing'|entity$type=='diagram'|entity$type=='photo'){
      zenodo_metadata$setImageType(entity$type)
    }
  }
  #contacts
  #TODO think if correct handle all contacts (whatever roles) as creators (author/co-authors)
  contact_added <- list()
  zenodo_metadata$metadata$creators <- list()
  for(contact in entity$contacts){
    
    #manage orcid?
    orcid <- NULL
    contact_ids <- contact$identifiers
    if(any(sapply(contact_ids, function(x){x$key=="orcid"}))){
      contact_ids <- contact_ids[sapply(contact_ids, function(x){x$key=="orcid"})]
      if(length(contact_ids)>0) orcid <- contact_ids[[1]]$value
    }
    #add/update creators
    if(!(contact$id %in% contact_added)){
      config$logger.info(sprintf("Contact:%s",contact$id))
      # lastname = gsub(paste0(firstname," "),"",contact$id),
      # lastname = gsub(firstname,"",contact$id),
      # lastname =substr(contact$id, nchar(firstname)+1, length(contact$id)),
      
      # if(grepl(" ", contact$id)){
      #   config$logger.info(sprintf("Contact:%s",contact$id))
      #   config$logger.info(sprintf("firstname:%s",strsplit(contact$id," ")[[1]][1]))
      #   config$logger.info(sprintf("lastname:%s",strsplit(contact$id," ")[[1]][2]))
      #   zenodo_metadata$addCreator(
      #     firstname <- strsplit(contact$id," ")[[1]][1],
      #     lastname <- strsplit(contact$id," ")[[1]][2],
      #     affiliation = "-"
      #     # orcid = orcid
      #   )
      # }else{
      if(grepl("nation", contact$id)){
      config$logger.info(sprintf("Contact:%s",contact$id))
      config$logger.info(sprintf("firstname:%s",''))
      config$logger.info(sprintf("lastname:%s",strsplit(contact$id,"nation")[[1]][2]))
      zenodo_metadata$addCreator(
        firstname <- '',
        lastname <- toTitleCase(strsplit(contact$id,"nation")[[1]][2]),
        affiliation = "-"
        # orcid = orcid
      )
    }else{
        config$logger.info(sprintf("Contact:%s",contact$id))
        zenodo_metadata$addCreator(
          firstname = "Secretariat", 
          lastname = "IOTC", 
          affiliation = "Indian Ocean Tuna Commission",
          orcid = "0000-0003-1082-7207"
        )
      }
      contact_added <- c(contact_added, contact$id)
    }
  }
  
  #TODO myrec$setLicense
  zenodo_metadata$setLicense('CC-BY-NC-4.0')
  
  #TODO myrec$setAccessRight
  
  # #communities
  # if(!is.null(communities)){
  #   for(community in communities) zenodo_metadata$addCommunity(community)
  # }
  zenodo_metadata$setCommunities(c('iotc_ctoi','fisheries'))
  # zenodo_metadata$addCommunity("fisheries")
  # zenodo_metadata$addCommunity("iotc")
  
  zenodo_metadata$setPublicationDate(entity$date)
  
  #file uploads
  if(depositWithFiles){
    if(deleteOldFiles){
      config$logger.info("Zenodo: deleting old files...")
      zen_files <- ZENODO$getFiles(zenodo_metadata$id)
      if(length(zen_files)>0){
        for(zen_file in zen_files){
          ZENODO$deleteFile(zenodo_metadata$id,zen_file$id)
        }
      }
    }
    config$logger.info("Zenodo: uploading files...")
    #upload data files, if any
    data_files <- list.files(file.path(getwd(),"data"))
    if(length(data_files)>0){
      data_files <- data_files[regexpr(entity$identifiers[["id"]],data_files)>0]
      if(length(data_files)>0) data_files <- data_files[!endsWith(data_files, ".rds")]
      if(length(data_files)>0){
        config$logger.info("Zenodo: uploading data files...")
        for(data_file in data_files){
          config$logger.info(sprintf("Zenodo: uploading data file '%s'", data_file))
          ZENODO$uploadFile(file.path(getwd(), "data", data_file), zenodo_metadata$id)
        }
      }
    }
    #upload metadata files, if any
    metadata_files <- list.files(file.path(getwd(),"metadata"))
    if(length(metadata_files)>0){
      metadata_files <- metadata_files[regexpr(entity$identifiers[["id"]],metadata_files)>0]
      if(length(metadata_files)>0) metadata_files <- metadata_files[!endsWith(metadata_files, ".rds")]
      if(length(metadata_files)>0){
        config$logger.info("Zenodo: uploading metadata files...")
        for(metadata_file in metadata_files){
          config$logger.info(sprintf("Zenodo: uploading metadata file '%s'", metadata_file))
          ZENODO$uploadFile(file.path(getwd(), "metadata",metadata_file), zenodo_metadata$id)
        }
      }
    }
  }
  
  #deposit (and publish, if specified in options)
  if(publish){
    #double verification for publish action, need to have the DOI specified in the entity table
    if(is.null(entity$identifiers[["doi"]])){
      config$logger.warn("No DOI specified in entity. Zenodo 'publish' action ignored!")
      publish <- FALSE
    }
    if(!is.null(entity$identifiers[["doi"]])){
      if(regexpr("zenodo", doi)>0) if(doi != zenodo_metadata$metadata$prereserve_doi$doi){ 
        config$logger.warn(sprintf("DOI specified (%s) in entity doesn't match Zenodo record DOI (%s). Zenodo 'publish' action ignored!", 
                                   doi, zenodo_metadata$metadata$prereserve_doi$doi))
        publish <- FALSE
      }
    }
  }
  
  config$logger.info(sprintf("Deposit record with id '%s' - publish = %s", zenodo_metadata$id, tolower(as.character(publish))))
  out <- ZENODO$depositRecord(zenodo_metadata, publish = publish)
  if(!is(out,"ZenodoRecord")){
    errMsg <- sprintf("Zenodo: %s", out$errors[[1]]$message)
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #we set the (prereserved) doi to the entity in question
  config$logger.info(sprintf("Setting DOI '%s' to save and export for record",zenodo_metadata$metadata$prereserve_doi$doi))
  for(i in 1:length(config$metadata$content$entities)){
    ent <- config$metadata$content$entities[[i]]
    # ent$subjects[[1]]$keywords<-NULL
    # zenodo_metadata$setKeywords(entity$subjects[[1]]$getKeywords(pretty=TRUE)$keyword_name)
    if(ent$identifiers[["id"]]==entity$identifiers[["id"]]){
      if(regexpr("zenodo", doi)>0) config$metadata$content$entities[[i]]$identifiers[["doi_to_save"]] <- zenodo_metadata$metadata$prereserve_doi$doi
      break;
    }
  }
  
}


