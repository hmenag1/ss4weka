

#' Summarizes the visits by age groups and by day
#'
#' @param condition a string indicating the condition under consideration
#' @param df a data.frame containing individual records with date and age
#'   variables formatted properly.
#' @param fields a vector of column indexes to be included in the final output
#'
#' @return a list containg the raw reduced data.frame, a data.frame containing
#'   the number of events for each day, and a third data.frame containing the
#'   number of events per facility
#' @export
#'
#' @details The raw data.frame can be accessed using the label "tmpdf". The
#'   events per day data.frame can be accessed with the label "events.per.day".
#'   The number of events per day for a facility can be accessed with the label
#'   "events.per.day.per.facility".
summaryofEvents <- function(condition, df, fields){

    eventslst <- list(tidydf=NULL,eventsperday=NULL, eventsdetails=NULL)
    tmpdf <- df[,fields]

    # Add column naming each as a syndrome
    tmpdf$condition <- condition

    # Format dates
    tmpdf$Visit_Date <- tmpdf$Earliest_Date

    # Add age groups
    attach(tmpdf)
    tmpdf$Agecat[Age >= 65] <- "65+"
    tmpdf$Agecat[Age > 4 & Age <= 64] <- "45-64"
    tmpdf$Agecat[Age > 17 & Age <= 44] <- "18-44"
    tmpdf$Agecat[Age > 4 & Age <= 17] <- "5-17"
    tmpdf$Agecat[Age <5] <- "<5"
    detach(tmpdf)

    # Add and reformat Facility ID
    tmpdf$facilityID <- tmpdf$FacilityID_UUID


    eventslst[[1]]<- tmpdf

    # Create summary datasets for display
    events.per.day.per.facility <- ddply(tmpdf,.(Visit_Date, facilityID), summarise,
                                         Number_of_Visits=length(Visit_Date))

    events.per.day <- ddply(tmpdf, .(Visit_Date), summarise,
                            Number_of_Visits=length(Visit_Date))


    # Add continuous Date sequence for Time Series Graph
    dates<- data.frame(Visit_Date=
                           as.Date(
                               seq.Date(as.Date(StartDate),
                                        as.Date(EndDate), by="1 day")))

    events.per.day<-merge(x=dates, y=events.per.day,
                          by="Visit_Date", all.x=TRUE)
    events.per.day$condition <- condition
    events.per.day[is.na(events.per.day)] <- 0


    events.per.day.per.facility <- merge(x=dates, y=events.per.day.per.facility,
                                         by="Visit_Date", all.x=TRUE)
    events.per.day.per.facility$condition <- condition
    events.per.day.per.facility[is.na(events.per.day.per.facility)] <- 0


    eventslst[[2]]<- events.per.day

    eventslst[[3]] <- events.per.day.per.facility

    return(eventslst)
}
