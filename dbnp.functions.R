#   Copyright 2012 Thomas Kelder
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

.set("deviceID", digest(paste(Sys.info(), collapse="."), algo="md5", serialize = F)) ##TODO: replace with real unique machine id
.set("defaultBase", "http://studies.dbnp.org/api/")
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

setGscfBaseUrl = function(baseUrl = "http://studies.dbnp.org/api/") {
  .set("defaultBase", baseUrl)
}

getGscfBaseUrl = function() .get("defaultBase")

setGscfDeviceId = function(deviceId) {
  .set("deviceID", deviceId)
}

getGscfDeviceId = function() .get("deviceID")

setGscfVerbose = function(verbose = F) {
  .set("verbose", verbose)
}

authenticate = function
### Authenticate with GSCF
(user, ##<< The username
 pass, ##<< The password
 shared.key ##<< The shared key (can be found at your profile page in the GSCF web interface)
 ) {
	url = .createUrl(.get("defaultBase"), "authenticate", list(deviceID = .get("deviceID")))
	resp = .getUrlErr(url, .opts = curlOptions(userpwd = paste(user, pass, sep=":")))
	auth = fromJSON(resp)
  .set("authSequence", auth$sequence)
	.set("authKey", shared.key)
	.set("authToken", auth$token)
	T
}

.fieldAsName = function(x, f = "token") {
	names(x) = as.character(sapply(x, function(y) y[f]))
	x
}

getStudies = function
### Get a list of the available studies
() {
	url = .createUrl(getGscfBaseUrl(), "getStudies", list(validation = .getValidation(), deviceID = .get("deviceID")))
	resp = .getUrlErr(url)
	r = fromJSON(resp)
	.fieldAsName(r$studies)
  ### A named list with the available studies
}

getSubjectsForStudy = function
### Get subjects for a given study
(studyToken ##<< The token of the study to get the subjects for
 ) {
	url = .createUrl(getGscfBaseUrl(), "getSubjectsForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
	resp = .getUrlErr(url)
	r = fromJSON(resp)
	.fieldAsName(r$subjects)
  ### A named list with the subjects
}

getEventGroupsForStudy = function
### Get event groups for a given study
(studyToken ##<< The token of the study to get the event groups for
 ) {
	url = .createUrl(getGscfBaseUrl(), "getEventGroupsForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
	resp = .getUrlErr(url)
	r = fromJSON(resp)
	.fieldAsName(r$eventGroups)
  ### A named list with the event groups
}

getEventsForStudy = function
### Get events for a given study
(studyToken ##<< The token of the study to get the events for
 ) {
	url = .createUrl(getGscfBaseUrl(), "getEventsForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
	resp = .getUrlErr(url)
	r = fromJSON(resp)
	.fieldAsName(r$events)
  ### A named list with the events
}

getSamplingEventsForStudy = function
### Get sampling events for a given study
(studyToken ##<< The token of the study to get the sampling events for
 ) {
	url = .createUrl(getGscfBaseUrl(), "getSamplingEventsForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
	resp = .getUrlErr(url)
	r = fromJSON(resp)
	.fieldAsName(r$samplingEvents)
  ### A named list with the sampling events
}

getSamplesForStudy = function
### Get samples for a given study
(studyToken ##<< The token of the study to get the samples for
 ) {
	url = .createUrl(getGscfBaseUrl(), "getSamplesForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
	resp = .getUrlErr(url)
	r = fromJSON(resp)
	.fieldAsName(r$samples)
  ### A named list with the samples
}

getAssaysForStudy = function
### Get the available assays for a given study
(studyToken ##<< The token of the study to get the assays for
 ) {
	url = .createUrl(getGscfBaseUrl(), "getAssaysForStudy", list(studyToken = studyToken, deviceID = .get("deviceID"), validation = .getValidation()))
	resp = .getUrlErr(url)
	r	= fromJSON(resp)
	.fieldAsName(r$assays)
  ### A named list of the assays
}

getSamplesForAssay = function
### Get samples for a given assay
(assayToken ##<< The token of the assay to get the samples for
 ) {
	url = .createUrl(getGscfBaseUrl(), "getSamplesForAssay", list(assayToken = assayToken, deviceID = .get("deviceID"), validation = .getValidation()))
	resp = .getUrlErr(url)
	r = fromJSON(resp)
	.fieldAsName(r$samples)
  ### A named list of the available samples
}

getMeasurementDataForAssay = function
### Get data for a given assay
(assayToken ##<< The token of the assay to get the data for
 ) {
	url = .createUrl(getGscfBaseUrl(), "getMeasurementDataForAssay", list(assayToken = assayToken, deviceID = .get("deviceID"), validation = .getValidation()))
	resp = .getUrlErr(url)
	r = fromJSON(resp)
	r$measurements
  ### Named list with available measurement data
}

assayDataAsMatrix = function
### Convenience function to load all data and samples for given assays.
(assayTokens ##<< The tokens of the assays to get the data for
 ) {
	assayData = lapply(assayTokens, function(a) {
		if(.get("verbose")) message("Getting data for assay: ", a)
		samples = getSamplesForAssay(a)
		data = getMeasurementDataForAssay(a)
		list(samples = samples, data = data)
	})
	names(assayData) = assayTokens
	
	data = c("sampleToken", "sampleName", "measurementName", "value")

	for(a in names(assayData)) {
		ad = assayData[[a]]$data
		samples = assayData[[a]]$samples
		sampleNames = lapply(samples, function(x) x$name)
		
		for(s in names(ad)) {
			row = ad[[s]]
			row[sapply(row, is.null)] = NA
			for(m in names(row)) {
				rd = as.character(c(s, sampleNames[s], m, row[m]))
				data = rbind(data, rd)
			}
		}
	}
  if(!is.null(dim(data))) {
  	colnames(data) = data[1,]
  	data = data[2:nrow(data),]
  	rownames(data) = NULL
  	data = as.data.frame(data, stringsAsFactors = F)
  	
  	list(data = data, raw = assayData)
  } else {
    if(.get("verbose")) print(data)
    warning("No data!")
  }
	### Returns a named list with the following items $data (data as pivot table),
  ### $raw (data in list form as returned by getSamplesForAssay and getMeasurementDataForAssay)
}
