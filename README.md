R2GSCF
======

An R client to connect to [GSCF](https://github.com/PhenotypeFoundation/GSCF).

### How to install ##
Download the package file from github:
https://github.com/thomaskelder/R2GSCF/blob/master/GSCFClient_1.0.tar.gz

And install it in R:

```R
install.packages('/path/to/GSCFClient_1.0.tar.gz', repo=NULL)
```

### How to use ###
Below is a simple manual on how to use this R client. For a more complete example, please see [this script](example.R).

Load the R client package:

```R
library(GSCFClient)
```

Or. if you prefer to load the library without installing, you can also run it directly from source:

```R
library(devtools)
source_url("https://raw.github.com/PhenotypeFoundation/GSCF-RClient/master/dbnp.functions.R")
```

Specify to which instance of GSCF you wish to connect:

```R
setGscfBaseUrl("http://old.studies.dbnp.org/api/")
```

Specify your authentication information and login to GSCF. You can lookup your shared key on the GSCF website, under user -> profile. Do not forget to ask an admin to give you ROLE_CLIENT privileges.

```R
user = "yourUsername"
pass = "yourPass"
skey = "yourSharedKey"
authenticate(user, pass, skey)
```

Now you can call the GSCF API functions in your R script, e.g.:

```R
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

print(samData$data[1:3,c("sampleToken", "measurement", "value")])
## This will give:
##                           sampleToken measurement value
## 1 a9756533-108c-43d4-b1e0-d82d6a7d0781 adiponectin  8.64
## 2 a9756533-108c-43d4-b1e0-d82d6a7d0781 Cholesterol 3.182
## 3 8ac44d05-8203-4171-85f4-f81e78399d6a Cholesterol  4.49
```
