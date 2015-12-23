

tidy_BioSense <- function(df){
    #browser()
    checklib("lubridate")
    ## Fix the date/time fields
    df$Create_Date_Time <- fixDateTime(df$Create_Date_Time, 'datetime')
    df$Update_Date_Time <- fixDateTime(df$Update_Date_Time, 'datetime')
    df$Earliest_Date_Time <- fixDateTime(df$Earliest_Date_Time, 'datetime')
    df$Admit_Date_Time <- fixDateTime(df$Admit_Date_Time, 'datetime')
    df$Event_Date_Time <- fixDateTime(df$Event_Date_Time, 'datetime')
    df$Message_Date_Time <- fixDateTime(df$Message_Date_Time, 'datetime')
    df$Observation_Date_Time <- fixDateTime(df$Observation_Date_Time, 'datetime')
    df$Disposition_Date_Time <- fixDateTime(df$Disposition_Date_Time, 'datetime')
    df$Diagnosis_Date_Time <- fixDateTime(df$Diagnosis_Date_Time, 'datetime')
    df$Procedure_Date_Time <- fixDateTime(df$Procedure_Date_Time, 'datetime')
    df$Death_Date_Time <- fixDateTime(df$Death_Date_Time, 'datetime')

    ## Fix the date fields
    df$Create_Date <- fixDateTime(df$Create_Date, 'date')
    df$Update_Date <- fixDateTime(df$Update_Date, 'date')
    df$Earliest_Date <- fixDateTime(df$Earliest_Date, 'date')
    df$Date_of_Onset <- fixDateTime(df$Date_of_Onset, 'date')
    df$DoB <- fixDateTime(df$DoB, 'date')

    ## fix the age units field
    df$Age_Units[which(df$Age_Units %in% c('Year','YEAR'))] <- "YEARS"
    df$Age_Units[which(df$Age_Units %in% c('Month','MONTH'))] <- "MONTHS"
    df$Age_Units[which(df$Age_Units %in% c('Day','DAY'))] <- "DAYS"

    df$Age_years <- NA
    df$Age_years[which(df$Age_Units=="YEARS")] <- df$Age
    df$Age_years[which(df$Age_Units=="MONTHS")] <- df$Age/12
    df$Age_years[which(df$Age_Units=="DAYS")] <- df$Age/(12*30.25)

    ## Combine Visit fields
    df$text <- paste(df$Chief_Complaint, df$Triage_Notes,
                     df$Diagnosis_Text, df$Diagnosis_Code)

    return(df)

}


fixDateTime <- function(datdate, dateType='date'){
#     if(grep("lubridate", search())){
#         library(lubridate)
#    }

    dat <- datdate

    if(dateType =='date'){
        dat <- suppressMessages(lubridate::ymd(dat))
    } else if(dateType =='datetime'){
        dat <- suppressMessages(lubridate::ymd_hms(dat))
    }

    if(identical(dat,datdate)){
        stop(print("Error!! Date format is unacceptable"))
    } else {
        return(dat)
    }

}



