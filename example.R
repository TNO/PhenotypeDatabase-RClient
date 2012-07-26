#################################
## Example on GSCF R functions ##
#################################
library(GSCFClient)

## Specify the GSCF instance
setGscfBaseUrl("http://old.studies.dbnp.org/api/")

## Login to the GSCF instance
#user = "yourUsername"
#pass = "yourPass"
#skey = "yourSharedKey"
authenticate(user, pass, skey)

## Get available studies
studies = getStudies()

## Look for the NuGO PPS2 study
study = studies[[grep("PPS2", sapply(studies, function(x) x$title))]]
studyToken = study['token']

## Get the subject information for the study
subjects = getSubjectsForStudy(studyToken)

## Get some assay data for the study
assays = getAssaysForStudy(studyToken)
assayNames = sapply(assays, function(x) x$name)
samAssay = names(assayNames[grep("chemistry", assayNames)])
samData = assayDataAsMatrix(samAssay)

## Get the sample information for the assay
samples = getSamplesForAssay(samAssay)

## Extract the mouse number from the sample name
sampleMice = sapply(samples, function(x) gsub("_.+$", "", x$name))

## Get the body weight from subjects
bw = sapply(samples, function(s) {
  mouse = gsub("_.+$", "", s$name)
  subjects[sapply(subjects, function(x) x$name) == mouse][[1]]$`bodyWeight(g)`
})

## Reformat data to plot
plotData = samData$data[, c('sampleToken', 'measurement', 'value')]
plotData = cbind(plotData, bodyWeight = as.numeric(bw[plotData$sampleToken]))
plotData$value = as.numeric(plotData$value)

## Plot adiponectin and cholesterol data for all mice
library(ggplot2)
p = ggplot(plotData, aes(bodyWeight, value))
p = p + geom_point(aes(colour = measurement))
p