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
  .fieldAsName(r$features)
}

getMeasurementDataForAssay = function
### Get data for a given assay
(assayToken ##<< The token of the assay to get the data for
) {
  url = .createUrl(getPhenotypeDatabaseBaseUrl(), "getMeasurementDataForAssay", list(assayToken = assayToken, deviceID = .get("deviceID"), validation = .getValidation()))
  resp = .getUrlErr(url)
  r = fromJSON(resp$measurements)
  r
}