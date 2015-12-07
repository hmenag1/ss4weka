
libs <- c("data.table")
lapply(libs, require, character.only = TRUE)

fundirs <- file.path("J:", "SANDBOX", "R", "Packages", "ss4weka", "R")
source(file.path(fundirs,"flagBioRecs.R"), echo = TRUE)
source(file.path(fundirs,"myUtilities.R"), echo = TRUE)
source(file.path(fundirs,"prep.raw.data.R"), echo = TRUE)
source(file.path(fundirs,"filterBioByWords.R"), echo = TRUE)
source(file.path(fundirs,"bio_CustDisplay.R"), echo = TRUE)


dfA <- read.csv(file.path("J:", "Documents", "Work", "KDHE", "Projects",
                          "Syndromic Surveillance", "SS Data",
                          "dfdata_spec.csv"), stringsAsFactors = FALSE )

dfB <- read.csv(file.path("J:", "Documents", "Work", "KDHE", "Projects",
                          "Syndromic Surveillance", "SS Data",
                          "dfdata.csv"), stringsAsFactors = FALSE )

## Source prep.raw.data.R before running next line
tidydata <- tidy_BioSense(dfB)

str(tidydata)

inclusions<- paste("HYPOTHERM|FROST|COLD EXPOS|EXPOSURE TO COLD|9916|991.6|EXTRM COLD")
exclusions<- paste("780.65|78065|DIABET|FROSTING|SEPSIS|THYROID|DIALYS|SEPTIC|CVA|HYPERGLYCEM|IDDM|INSULIN")


## source
# filterBioByWords AND
# myUtilities AND
# flagBioRecs
## before running the next line

ls <- filterBioByWords(tidydata, inclusions = inclusions, exclusions = exclusions)

eval1df <- ls[["eval1"]]

ev <- moveCol(eval1df)

ct <- ls[["cut3"]]
cutnames <- names(ct)
changes <- data.frame("from"= c(49, 68,43, 86), "to"= c(3,4,5,6))
ct <- moveCols(ct, changes)


columns <- c("Row_Number", "Facility_Name",
             "Chief_Complaint","Triage_Notes",
             "Diagnosis_Text","Diagnosis_Code",
             "Age", "Gender", "Earliest_Date","County_Name")

bio_CustDisplay(dfB,"KS_100nmrm00-01-9520151109","Unique_Visiting_ID")

bio_CustDisplay(dfB, 1, colList = columns)
