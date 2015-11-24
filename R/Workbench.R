
libs <- c("lubridate")
lapply(libs, require, character.only = TRUE)

dfA <- read.csv(file.path("J:", "Documents", "Work", "KDHE", "Projects",
                          "Syndromic Surveillance", "SS Data",
                          "dfdata_spec.csv"), stringsAsFactors = FALSE )

dfB <- read.csv(file.path("J:", "Documents", "Work", "KDHE", "Projects",
                          "Syndromic Surveillance", "SS Data",
                          "dfdata.csv"), stringsAsFactors = FALSE )


tidydata <- tidy_BioSense(dfA)

str(tidydata)

inclusions<- paste("HYPOTHERM|FROST|COLD EXPOS|EXPOSURE TO COLD|9916|991.6|EXTRM COLD")
exclusions<- paste("780.65|78065|DIABET|FROSTING|SEPSIS|THYROID|DIALYS|SEPTIC|CVA|HYPERGLYCEM|IDDM|INSULIN")

eval1 <- getHits(tidydata, inclusions)

keyfields <- paste("Chief_Complaint|Triage_Notes|Diagnosis_Text|Diagnosis_Code")

allfields <- names(tidydata)

allfields %in% searchfields
grep(searchfields, allfields)
