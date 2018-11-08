#   Copyright 2016 Thomas Kelder, Ferry Jagers & Tim van den Broek
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

library(RJSONIO)
library(rjson)
library(RCurl)
library(digest)

.vars = new.env()

.set = function(name, value) {
  assign(name, value, env=.vars)
}

.get = function(name) {
  v = get(name, .vars)
  v
}

.genDeviceID = function(user) {
  digest(paste0(.get("deviceIDBase"), user), algo="md5", serialize = F)
}

.set("deviceIDBase", paste(Sys.info(), collapse=".")) ##TODO: replace with real unique machine id
.set("defaultBase", "https://dashin.eu/interventionstudies/api/")
.set("authSequence", 0)
.set("authKey", "")
.set("authToken", "")
.set("verbose", F)

.getValidation = function() {
  .set('authSequence', .get('authSequence') + 1)
  str = paste(.get('authToken'), .get('authSequence'), .get('authKey'), sep="")
  if(.get("verbose")) {
    message("Sequence: ", .get('authSequence'))
    message("Validation string: ", str)
  }
  digest(str, algo = "md5", serialize = F)
}

.createUrl = function(base, cmd, pars = list()) {
  url = paste(base, cmd, "?", sep="")
  for(p in names(pars)) {
    v = pars[[p]]
    if(is.null(v) || v != "") url = paste(url, p, "=", v, "&", sep="")
  }
  gsub("[\\?&]{1}$", "", url)
}

.getUrlErr = function(...) {
  if(.get("verbose")) print(url)
  err= ""
  r = getURL(..., verbose = .get("verbose"))
  if(!isValidJSON(r, asText = T)) {
    stop(r)
  }
  r
}

setPhenotypeDatabaseBaseUrl = function(baseUrl = "https://dashin.eu/interventionstudies/api/") {
  .set("defaultBase", baseUrl)
}

getPhenotypeDatabaseBaseUrl = function() .get("defaultBase")

setPhenotypeDatabaseDeviceId = function(deviceId) {
  .set("deviceID", deviceId)
}

getPhenotypeDatabaseDeviceId = function() .get("deviceID")

setPhenotypeDatabaseVerbose = function(verbose = F) {
  .set("verbose", verbose)
}

authenticate = function
### Authenticate with the Phenotype Database
(user, ##<< The username
 pass, ##<< The password
 shared.key ##<< The shared key (can be found at your profile page in the Phenotype Database web interface)
) {
  devId = .genDeviceID(user)
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "authenticate", list(deviceID = devId))
  resp = .getUrlErr(url, .opts = curlOptions(userpwd = paste(user, pass, sep=":")))
  auth = fromJSON(resp)
  .set("authSequence", auth$sequence)
  .set("authKey", shared.key)
  .set("authToken", auth$token)
  .set("deviceID", devId)
  T
}

.fieldAsName = function(x, f = "token") {
  names(x) = as.character(sapply(x, function(y) y[f]))
  x
}

getStudies = function
### Get a list of the available studies
() {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getStudies", list(validation = .getValidation(), deviceID = .get("deviceID")))
  resp = .getUrlErr(url)
  r = fromJSON(resp)
  .fieldAsName(r$studies)
  ### A named list with the available studies
}

getSubjectsForStudy = function
### Get subjects for a given study
(studyToken ##<< The token of the study to get the subjects for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getSubjectsForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r = fromJSON(resp)
  .fieldAsName(r$subjects)
  ### A named list with the subjects
}

getSubjectGroupsForStudy = function
### Get subjects for a given study
(studyToken ##<< The token of the study to get the subjects for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getSubjectGroupsForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r = fromJSON(resp)
  .fieldAsName(r$subjectGroups)
  ### A named list with the subjects
}

getSampleAndTreatmentGroupsForStudy = function
### Get sample & treatment groups for a given study
(studyToken ##<< The token of the study to get the event groups for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getSampleAndTreatmentGroupsForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r = fromJSON(resp)
  .fieldAsName(r$sampleAndTreatmentGroups)
  ### A named list with the sample & treatment groups
}

getTreatmentTypesForStudy = function
### Get treatment types for a given study
(studyToken ##<< The token of the study to get the events for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getTreatmentTypesForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r = fromJSON(resp)
  .fieldAsName(r$treatmentTypes)
  ### A named list with the treatment types
}

getSampleTypesForStudy = function
### Get sample types for a given study
(studyToken ##<< The token of the study to get the sampling events for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getSampleTypesForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r = fromJSON(resp)
  .fieldAsName(r$sampleTypes)
  ### A named list with the sample types
}

getAssaysForStudy = function
### Get the available assays for a given study
(studyToken ##<< The token of the study to get the assays for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getAssaysForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r	= fromJSON(resp)
  .fieldAsName(r$assays)
  ### A named list of the assays
}

getSamplesForStudy = function
### Get samples for a given study
(studyToken ##<< The token of the study to get the samples for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getSamplesForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r = fromJSON(resp)
  .fieldAsName(r$samples)
  ### A named list with the samples
}

getSubjectsForAssay = function
### Get subjects for a given assay
(assayToken ##<< The token of the assay to get the samples for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getSubjectsForAssay", list(assayToken = assayToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r = fromJSON(resp)
  .fieldAsName(r$subjects)
  ### A named list of the available subjects
}

getSamplesForAssay = function
### Get samples for a given assay
(assayToken ##<< The token of the assay to get the samples for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getSamplesForAssay", list(assayToken = assayToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r = fromJSON(resp)
  .fieldAsName(r$samples)
  ### A named list of the available samples
}

getFeaturesForAssay = function
### Get data for a given assay
(assayToken ##<< The token of the assay to get the data for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getFeaturesForAssay", list(assayToken = assayToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r = fromJSON(resp)
  r$features
}

getMeasurementDataForAssay = function
### Get data for a given assay
(assayToken ##<< The token of the assay to get the data for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getMeasurementDataForAssay", list(assayToken = assayToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r = fromJSON(resp)
  r
}


getMoltenAssayList <- function(study_token){
  parsePhenoDbTime <- function(x){
    
    ptime  <- list(w=0,d=0,h=0,m=0,s=0)
    
    val    <- unlist(strsplit(x,split = '[a-z]'))
    
    header <- unlist(strsplit(x,split = '[0-9]'))
    header <- header[!header %in% c('','-')]
    
    val        <- as.numeric(val)
    names(val) <- header
    val        <- as.list(val)
    
    for(dtype in names(val) )
      ptime[[dtype]] <- val[[dtype]]
    
    unlist(ptime)
    
  }
  
  sanitize_str <- function(str){
    str                  <- gsub("\\s*\\([^\\)]+\\)","",str)
    str                  <- gsub("[[:punct:]]", "", str)
    str                  <- gsub("[[:space:]]", "", str)
    
    str
  }
  
  assays       <- getAssaysForStudy(study_token)
  assay_tokens <- lapply(assays, function(x) x[['token']])# tokens for all assays
  
  
  sampleData    <- getSubjectsForStudy(study_token)
  subject_data  <- do.call(rbind,lapply(sampleData,unlist ))
  
  experiments  <- list()
  sample_maps  <- list()    
  
  molten_list <-  lapply(assay_tokens, function(token){ 
    assay_name =   sanitize_str( assays[[token]][['name']] )
    
    assayFeatures = getFeaturesForAssay(token)
    assayData     = getMeasurementDataForAssay(token)
    
    features_measurements = do.call(cbind, lapply(assayData$measurements, function(x){
      names(x) <- gsub('[.]','',names(x))
      unlist(x)
    } ))
    
    if(length(features_measurements) == 0 ){
      warning(paste0('Empty assay for token ',token,' name ',assay_name))
      return(NULL)
    }
    
    # map sample ids                                              
    mapped_sample_list <- lapply( strsplit(rownames(features_measurements),split = '[.]'),function(x){
      type <- x[1]
      t1   <- x[2]   
      t2   <- x[3]
      subj <- x[4]
      
      tvec <- parsePhenoDbTime(t1) + parsePhenoDbTime(t2)
      tstr <- paste0(paste0(tvec,names(tvec)),collapse = '')
      
      c(subj,tstr)
    })
    
    mapped_samples           <- do.call(rbind,mapped_sample_list)
    colnames(mapped_samples) <- c('name','strtime')
    mapped_samples           <- as.data.frame(mapped_samples)
    
    
    features_measurements_molten       <- reshape2::melt(data = data.frame(features_measurements,mapped_samples,stringsAsFactors = F),
                                                         id.var=c('name','strtime'))
    
    features_measurements_molten$assay <- assay_name
    features_measurements_molten       <- features_measurements_molten[,c('assay','name','strtime','variable','value')]
  })
  # cleanup list names
  names(molten_list) <- paste0('token_',gsub('-','_',names(molten_list)))
  
  return(molten_list)
}

mapAssayMeasurement <- function(molten_assay_list,token,variable,group_map){
  
  molten_assay_list = paris_molten_assay_list[[token]]
  
  row_select        = molten_assay_list$variable == variable & paste(molten_assay_list$strtime) %in% names(group_map)
  molten_assay_list = molten_assay_list[row_select,]
  
  time_mapper <- function(x){
    group_map[[x]]
  }
  
  molten_assay_list$study_time <- sapply(molten_assay_list$strtime, time_mapper)
  molten_assay_list
}                         



getMultiAssayExp <- function(study_token){
  require(MultiAssayExperiment)
  require(reshape2)
  
  
  parsePhenoDbTime <- function(x){
    
    ptime  <- list(w=0,d=0,h=0,m=0,s=0)
    
    val    <- unlist(strsplit(x,split = '[a-z]'))
    
    header <- unlist(strsplit(x,split = '[0-9]'))
    header <- header[!header %in% c('','-')]
    
    val        <- as.numeric(val)
    names(val) <- header
    val        <- as.list(val)
    
    for(dtype in names(val) )
      ptime[[dtype]] <- val[[dtype]]
    
    unlist(ptime)
    
  }
  
  sanitize_str <- function(str){
    str                  <- gsub("\\s*\\([^\\)]+\\)","",str)
    str                  <- gsub("[[:punct:]]", "", str)
    str                  <- gsub("[[:space:]]", "", str)
    
    str
  }
  
  assays       <- getAssaysForStudy(study_token)
  assay_tokens <- lapply(assays, function(x) x[['token']])# tokens for all assays
  
  sampleData    <- getSubjectsForStudy(study_token)
  subject_data  <- do.call(rbind,lapply(sampleData,unlist ))
  
  experiments  <- list()
  sample_maps  <- list()
  
  
  for(token in assay_tokens){
    
    assayFeatures = getFeaturesForAssay(token)
    assayData     = getMeasurementDataForAssay(token)
    
    features_measurements = do.call(cbind, lapply(assayData$measurements, function(x){
      names(x) <- gsub('[.]','',names(x))
      unlist(x)
    } ))
    
    if(length(features_measurements) == 0 ){
      warning(paste0('Empty assay for token ',token,' name ',assay_name))
      next
    }
    
    # map sample ids                                              
    mapped_sample_list <- lapply( strsplit(rownames(features_measurements),split = '[.]'),function(x){
      type <- x[1]
      t1   <- x[2]   
      t2   <- x[3]
      subj <- x[4]
      
      tvec <- parsePhenoDbTime(t1) + parsePhenoDbTime(t2)
      tstr <- paste0(paste0(tvec,names(tvec)),collapse = '')
      
      c(subj,tstr)
    })
    
    mapped_samples           <- do.call(rbind,mapped_sample_list)
    colnames(mapped_samples) <- c('name','strtime')
    mapped_samples           <- as.data.frame(mapped_samples)
    
    
    features_measurements_molten <- reshape2::melt(data = data.frame(features_measurements,mapped_samples,stringsAsFactors = F),
                                                   id.var=c('name','strtime'))
    
    feature_data                 <- reshape2::dcast(features_measurements_molten, fill = NA,drop = F,
                                                    value.var = 'value',
                                                    formula = name+strtime~variable)
    
    primary_key               <- paste0(feature_data$name,'_',feature_data$strtime)         
    colname_key               <- primary_key    
    
    rownames(feature_data)    <- primary_key
    feature_data$name         <- NULL  
    feature_data$strtime      <- NULL  
    
    
    # add times
    # add treatment/group information    
    assay_sample_map      = data.frame(primary = as.character(primary_key),
                                       colname = as.character(colname_key) )
    
    # retrive and sanitize assay name
    assay_name                  <- assays[[token]][['name']]
    assay_name                  <- sanitize_str(assay_name)
    
    sample_maps[[ assay_name ]] <-  assay_sample_map
    experiments[[ assay_name ]] <-  t(feature_data)
  }
  
  col_data           <- do.call(rbind,lapply(sample_maps,function(x) x))
  col_data           <- col_data[!duplicated(col_data$primary),]
  col_data$name      <- unlist(lapply(strsplit(paste(col_data$primary),split = '_'),function(x) x[[1]] ))
  col_data           <- merge(col_data,subject_data,by='name')
  rownames(col_data) <- col_data$primary
  col_data$str_time  <- unlist(lapply(strsplit(paste(col_data$primary),split = '_'),function(x)x[[2]]))
  col_data$colname   <- NULL
  
  multi_sample_map <- listToMap(sample_maps)                                 
  
  multi_assay_exp  <- MultiAssayExperiment(experiments = experiments,
                                           colData   = col_data,
                                           sampleMap = multi_sample_map)
  return(multi_assay_exp)
  
}