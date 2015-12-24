

#' Tidies-up Raw NSSP Data
#'
#' @param df
#'
#' @return Returns a data frame with additional calculated fields.
#' @export
#' @details This function converts date and time fields to the right format for R. Also, it adds new calculated fields including one for the age of the patient in years (Age_years) and another one containing the concatenation of the literal fields (Text). In addition, it attempts to standardize the entries in the Age_Units field.

tidy_BioSense <- function(df){
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
    df$Text <- paste(df$Chief_Complaint, df$Triage_Notes,
                     df$Diagnosis_Text, df$Diagnosis_Code)

    return(df)

}



#' Converts Date Strings into R Date/Time Format
#'
#' @param datdate a string representing a date and/or time. NOTE: This
#'   implementation is incomplete.
#' @param dateType a string indicating the type of the date/time represented by
#'   the string to be converted.
#'
#' @return an R datetime object
#'
#' @details This implementation is inclomplete at this time. For datdate
#'   variable the acceptable values are 'date' and 'datetime'. Other options are
#'   forthcomming.
#'
#' @export
fixDateTime <- function(datdate, dateType='date'){

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



