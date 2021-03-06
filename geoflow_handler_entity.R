#' handle_entities_df
#' @export
handle_entities_df <- function(config, source){
  
  if(!is(source, "data.frame")){
    errMsg <- "Error in 'handle_entities_df': source parameter should be an object of class 'data.frame'"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  entities <- list()
  rowNum <- nrow(source)
  config$logger.info(sprintf("Parsing %s entities from tabular source", rowNum))
  for(i in 1:rowNum){
    config$logger.info(sprintf("Parsing entity %s", i))
    source_entity <- source[i,]
    entity <- geoflow_entity$new()
    
    entity$setDate(as.Date(source_entity[,"Date"]))
    
    if(!is.na(source_entity[,"Type"])) entity$setType(source_entity[,"Type"])
    
    #identifier
    identifiers <- unlist(strsplit(sanitize_str(source_entity[,"Identifier"]), ";"))
    for(identifier in identifiers){
      if(regexpr(":",identifier) == -1){
        entity$setIdentifier("id", identifier)
      }else{
        id_kvp <- extract_kvp(identifier)
        entity$setIdentifier(id_kvp$key, id_kvp$values[[1]])
      }
    }
    
    #title
    entity$setTitle(source_entity[,"Title"])
    
    entity$setLanguage(source_entity[,"Standard_language"])
    
    # entity$addRelation('isIdenticalTo',paste(''))
    
    #description
    src_description <- sanitize_str(source_entity[,"Description"])
    descriptions <- if(!is.na(src_description)) unlist(strsplit(src_description, ";")) else list()
    if(length(descriptions)>0){
      if(length(descriptions)==1){
        entity$setDescription("abstract", descriptions)
      }else{
        for(description in descriptions){
          if(regexpr(":",description) == -1)
            entity$setDescription("abstract", description)
          else
            des_kvp <- extract_kvp(description)
            entity$setDescription(des_kvp$key, des_kvp$values[[1]])
        }
      }
    }
    
    #subjects
    src_subject <- sanitize_str(source_entity[,"Subject"])
    subjects <- if(!is.na(src_subject)) unlist(strsplit(src_subject, ";")) else list()
    if(length(subjects)>0){
      invisible(lapply(subjects, function(subject){
        subject_obj <- geoflow_subject$new(str = subject)
        entity$addSubject(subject_obj)
      }))
    }
      
    #contacts
    src_contact <- sanitize_str(source_entity[,"Creator"])
    contacts <- if(!is.na(src_contact)) unlist(strsplit(src_contact, ";")) else list()
    if(length(contacts)>0){
      invisible(lapply(contacts, function(contact){
        contact_splits <- unlist(strsplit(contact, ":"))
        contact_ids <- tolower(unlist(strsplit(contact_splits[2],",")))
        for(contact_id in contact_ids){
          if(is.na(contact_id)){
            config$logger.warn(sprintf("Warning: In entity %s, empty contact id will be ignored!", i))
          }else if(contact_id==""){
            config$logger.warn(sprintf("Warning: In entity %s, empty contact id will be ignored!", i))
          }else{
            contact_obj <- geoflow_contact$new()
            contact_obj$setId(contact_id)
            contact_obj$setRole(contact_splits[1])
            entity$addContact(contact_obj)
          }
        }
      }))
    }
    
      #relations
      # src_relation <- paste0('https://iotc.org/',sanitize_str(source_entity[,"Path"]))
      src_relation <- sanitize_str(source_entity[,"Relation"])
      relations <- if(!is.na(src_relation)) unlist(strsplit(src_relation, ";")) else list()
      if(length(relations)>0){
        invisible(lapply(relations, function(relation){
          relation_obj <- geoflow_relation$new(str = relation)
          entity$addRelation(relation_obj)
        }))
      }
      
    #spatial extent
    spatial_cov <- source_entity[,"SpatialCoverage"]
    if(!is.na(spatial_cov)){
      if(!startsWith(spatial_cov,"SRID=")) 
        stop("The spatial coverage should be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry")
      spatial_cov <- unlist(strsplit(spatial_cov,";"))
      if(length(spatial_cov)!=2) 
        stop("The spatial coverage should be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry")
      spatial_srid <- as.integer(unlist(strsplit(spatial_cov[1],"SRID="))[2])
      spatial_cov <- spatial_cov[2]
      entity$setSrid(spatial_srid)
      entity$setSpatialExtent(spatial_cov, crs = spatial_srid)
    }
    
    #temporal extent
    if(!is.na(source_entity[,"TemporalCoverage"])) entity$setTemporalExtent(source_entity[,"TemporalCoverage"])
    
    #Rights
    src_rights <- sanitize_str(source_entity[,"Rights"])
    rights <- if(!is.na(src_rights)) unlist(strsplit(src_rights, ";")) else list()
    if(length(rights)>0){
      invisible(lapply(rights, function(right){
        right_obj <- geoflow_right$new(str = right)
        entity$addRight(right_obj)
      }))
    }
    
    #Provenance
    prov <- sanitize_str(source_entity[,"Provenance"])
    if(!is.na(prov)){
      prov_obj <- geoflow_provenance$new(str = prov)
      entity$setProvenance(prov_obj)
    }
    
    #data
    data <- sanitize_str(source_entity[,"Data"])
    if(!is.na(data)){
      data_obj <- geoflow_data$new(str = data)
      entity$setData(data_obj)
    }
    
    #enrich metadata with dynamic properties
    if(!is.null(entity$data)) entity$enrichWithData(config)
    
    entities <- c(entities, entity)
  }
  return(entities)
}

#' handle_entities_gsheets
#' @export
handle_entities_gsheet <- function(config, source){
  
  if(!require("gsheet")) stop("Package 'gsheet' is required!")
  
  #read gsheet URL
  source <- as.data.frame(gsheet::gsheet2tbl(source))
  
  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}

#' handle_entities_csv
#' @export
handle_entities_csv <- function(config, source){
  
  #read csv TODO -> options management: sep, encoding etc
  source <- read.csv(source)
  
  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}

#' handle_entities_excel
#' @export
handle_entities_excel <- function(config, source){
  
  #read excel TODO -> options management: sep, encoding etc
  source <- as.data.frame(readxl::read_excel(source))
  
  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}


