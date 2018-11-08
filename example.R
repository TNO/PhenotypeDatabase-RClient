#function to check if a list entry is empty
is.NullOb <- function(x) if(!(is.function(x))) is.null(x) | all(sapply(x, is.null)) else FALSE

##function to recursively step down into list, removing all empty objects 
rmNullObs <- function(x) {
  if(!(is.function(x))) {
    x = x[!(sapply(x, is.NullOb))]
    lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
  }
}

#Uncomment lines below if you want to install PhenotypeDatabaseRClient via Github
#library(devtools)
#install_github("TNO/PhenotypeDatabase-RClient")

#load dependencies
require(PhenotypeDatabaseRClient)
require(reshape2)
require(dplyr)

#authentication procedure
setPhenotypeDatabaseBaseUrl("https://dashin.eu/interventionstudies/api/")
user = "<username>"
skey = "<apikey>"
authenticate(user, paste(readline()), skey)
#break here to enter password in console
#authenticate(user, winDialogString("Password:", ""), skey)

#get available studies & select specific study
studies = getStudies()
study = studies[[grep("<studyName>", sapply(studies, function(x) x$code))]]
studyToken = study['token']

## Get assays for the selected study
assays = getAssaysForStudy(studyToken)
assayTokens = names(assays)
assayNames = sapply(assays, function(x) x$name)

#get subjects in study
studySubjects = getSubjectsForStudy(studyToken)
studySubjects = do.call(cbind, studySubjects)

#get features in study and subjects in each assay
studyFeatures = list()
studySubjectsPerAssay = list()
for (i in seq_along(assayTokens)){
  assayToken = assayTokens[i]
  studyFeatures[[i]] = getFeaturesForAssay(assayToken)
  studySubjectsPerAssay[[i]] = getSubjectsForAssay(assayToken)
}
studyFeaturesWithData = sapply(studyFeatures, function(x) length(x) > 0)
studyFeatures = rmNullObs(studyFeatures)
studyFeaturesLong = melt(studyFeatures)
colnames(studyFeaturesLong) = c("Value", "Property", "Feature", "Assay")
studyFeaturesLong$Assay = assayNames[studyFeaturesWithData][studyFeaturesLong$Assay]
studyFeaturesWide = dcast(data = studyFeaturesLong, formula = Assay + Feature ~ Property, value.var = "Value")

names(studySubjectsPerAssay) = assayNames
studySubjectsPerAssay = rmNullObs(studySubjectsPerAssay)

#get all study data
studyData = list()
for (i in seq_along(assayTokens)){
  assayToken = assayTokens[i]
  studyData[[i]] = getMeasurementDataForAssay(assayToken)$measurements
}
names(studyData) = assayNames

#studyData = melt(studyData[sapply(studyData, function(x) x$count > 0)])
studyData = melt(studyData)
colnames(studyData) = c("Value", "Subject", "SampleType", "SubjectGroup", "RelTime", "AbsTime", "EventGroup", "Variable", "Assay")
