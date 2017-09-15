###############################################
## Example on Phenotype Database R functions ##
###############################################
library(devtools)
install_github("TNO/PhenotypeDatabase-RClient")
library(PhenotypeDatabaseRClient)

## Specify the Phenotype Database instance
setPhenotypeDatabaseBaseUrl("https://dashin.eu/interventionstudies/api/")

## Login to the Phenotype Database instance
user = "yourUsername"

## Set your password here or type it in console by uncommenting the second line below
pass = "yourPass"
#pass = readline("Enter your password in console: ")

## Set your API key (found in your Phenotype Database user profile)
skey = "yourSharedKey"

authenticate(user, pass, skey)

## Get available studies
studies = getStudies()

## Look for the Diclofenac study
study = studies[[grep("Diclofenac", sapply(studies, function(x) x$title))]]
studyToken = study['token']

## Get some assay data for the study
assays = getAssaysForStudy(studyToken)
assayNames = sapply(assays, function(x) x$name)
assayToken = names(assayNames[grep("GCMS", assayNames)])

assaySubjects = getSubjectsForAssay(assayToken)
assayFeatures = getFeaturesForAssay(assayToken)
assayData = getMeasurementDataForAssay(assayToken)

print(paste("Fetched a total of", assayData[['count']], "measurements for the assay."))

## Get all available timepoints for Inositol
inositolTimepoints = names(assayData[['measurements']][['Inositol']])

## View all available timepoints for Inositol
print(inositolTimepoints)

## Get measurments for timepoint 0
inositolBaseline = assayData[['measurements']][['Inositol']][['0s']]
## Get measurments for timepoint 1 week 2 days 3 hours
inositolLast = assayData[['measurements']][['Inositol']][['1w2d3h']]

## Create data.frame
assayDataFrame <- data.frame(
  subject = c(names(inositolBaseline),names(inositolLast)),
  timepoint = rep(c("Baseline", "Last"), each = 19),
  value = c(inositolBaseline, inositolLast)
)

## View data frame
print(assayDataFrame)

## Plot data frame (using ggplot2)
library(ggplot2)
p = ggplot(assayDataFrame, aes(x=subject, y=value, fill=timepoint)) +
  geom_bar(stat="identity", position=position_dodge())
p

## This requires an Opal (obiba.org/pages/products/opal) instance on the same server as your PhenotypeDatabase instance and some additional Phenotype Database configuration
exportAssayToOpal(assayToken)
