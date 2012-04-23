##########################################
## Functions to query the dbNP REST API ##
## Thomas Kelder, 2012                  ##
##########################################
library(RJSONIO)
library(RCurl)
library(digest)

.defaultDeviceID = "R"
.defaultBase = "http://studies.dbnp.org/api/"
.authSequence = 0
.authKey = ""
.authToken = ""

.getValidation = function() {
	.authSequence <<- .authSequence + 1
	print(.authSequence)
	str = paste(.authToken, .authSequence, .authKey, sep="")
	message("Validation string: ", str)
	digest(str, algo = "md5", serialize = F)
}

createUrl = function(base, cmd, pars = list()) {
	url = paste(base, cmd, "?", sep="")
	for(p in names(pars)) {
		v = pars[[p]]
		if(is.null(v) || v != "") url = paste(url, p, "=", v, "&", sep="")
	}
	gsub("[\\?&]{1}$", "", url)
}

getUrlErr = function(..., verbose = T) {
	if(verbose) print(url)
	err= ""
	r = getURL(..., verbose = verbose)
	if(!isValidJSON(r, asText = T)) {
		message("ERROR:\n", r)
	}
	r
}

authenticate = function(user, pass, shared.key, urlBase = .defaultBase, deviceID = .defaultDeviceID) {
	url = createUrl(urlBase, "authenticate", list(deviceID = .defaultDeviceID))
	resp = getUrlErr(url, .opts = curlOptions(userpwd = paste(user, pass, sep=":"), verbose = T))
	auth = fromJSON(resp)
	.authSequence <<- auth$sequence
	.authKey <<- shared.key
	.authToken <<- auth$token
	T
}

fieldAsName = function(x, f = "token") {
	names(x) = as.character(sapply(x, function(y) y[f]))
	x
}

getStudies = function(urlBase = .defaultBase, validation = .getValidation(), deviceID = .defaultDeviceID) {
	url = createUrl(urlBase, "getStudies", list(validation = validation, deviceID = deviceID))
	resp = getUrlErr(url)
	r = fromJSON(resp)
	fieldAsName(r$studies)
}

getSubjectsForStudy = function(studyToken, validation = .getValidation(), deviceID = .defaultDeviceID, urlBase = .defaultBase) {
	url = createUrl(urlBase, "getSubjectsForStudy", list(studyToken = studyToken, deviceID = deviceID, validation = validation))
	resp = getUrlErr(url)
	r = fromJSON(resp)
	fieldAsName(r$subjects, 'id')
}

getAssaysForStudy = function(studyToken, validation = .getValidation(), deviceID = .defaultDeviceID, urlBase = .defaultBase) {
	url = createUrl(urlBase, "getAssaysForStudy", list(studyToken = studyToken, deviceID = deviceID, validation = validation))
	resp = getUrlErr(url)
	r	= fromJSON(resp)
	fieldAsName(r$assays)
}

getSamplesForAssay = function(assayToken, validation = .getValidation(), deviceID = .defaultDeviceID, urlBase = .defaultBase) {
	url = createUrl(urlBase, "getSamplesForAssay", list(assayToken = assayToken, deviceID = deviceID, validation = validation))
	resp = getUrlErr(url)
	r = fromJSON(resp)
	fieldAsName(r$samples)
}

getMeasurementDataForAssay = function(assayToken, validation = .getValidation(), deviceID = .defaultDeviceID, urlBase = .defaultBase) {
	url = createUrl(urlBase, "getMeasurementDataForAssay", list(assayToken = assayToken, deviceID = deviceID, validation = validation))
	resp = getUrlErr(url)
	r = fromJSON(resp)
	r$measurements
}

##########################################################################
## Convenience function to load all data and samples for given assays.  ##
## Returns a list with the data in the following forms:                 ##
## - data: as molten data frame                                         ##
## - raw: a list with the raw json data for getSamplesForAssay and      ##
##   getMeasurementDataForAssay                                         ##
##########################################################################
assayDataAsMatrix = function(assayTokens) {
	assayData = lapply(assayTokens, function(a) {
		message("Getting data for assay: ", a)
		samples = getSamplesForAssay(a)
		data = getMeasurementDataForAssay(a)
		list(samples = samples, data = data)
	})
	names(assayData) = assayTokens
	
	data = c("sampleToken", "sampleName", "measurement", "value")

	for(a in names(assayData)) {
		ad = assayData[[a]]$data
		samples = assayData[[a]]$samples
		sampleNames = lapply(samples, function(x) x['name'])
		
		for(s in names(ad)) {
			row = ad[[s]]
			row[sapply(row, is.null)] = NA
			for(m in names(row)) {
				rd = as.character(c(s, sampleNames[s], m, row[m]))
				data = rbind(data, rd)
			}
		}
	}
	colnames(data) = data[1,]
	data = data[2:nrow(data),]
	rownames(data) = NULL
	data = as.data.frame(data, stringsAsFactors = F)
	
	list(data = data, raw = assayData)
}