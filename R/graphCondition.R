
#' Line Graph and Pie Graphs for Sex and Age Groups for NSSP Data
#'
#' @param dfevents a data.frame containing the values to display in the graphs.
#' @param dfcond a data.frame containing the predicted positive cases for the condition
#' @param condition a string used in the title and other places of the graphs
#'
#' @return a list containing graphical objects
#'
#' @details The dfevents data.frame contains the following columns: Visit_Date (a sequence of days that spans over a selected period), Number_of_Visits.x (the total number of cases identified for each day for the whole state), condition (a column containing one label, that of the condition), Number_of_Visits.y (the total number of ED visits for the whole state), and pct (the percentage of ED visits identified as positive cases) .
#' The purpose of the dfcond data.frame is to provide data for the pie graphs. The dfcond data.frame must contain the following columns: Gender and Agecat, which represents the age category. So any data set can be used as long as it contains the two variables. Please note both columns Gender and Agecat are factors but they can be strings that are coerced by the function into categories.
#'  Sometimes there may be issues with ggplot2 and it gives you an error such as 'Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state'. In that case issuing the command dev.off() may help solve the problem.
#'
#' @export
graphCondition <- function(dfevents,dfcond,condition){
    graphList <- list(ts=NULL,pSex=NULL,pAge=NULL)
    begining <- min(dfevents$Visit_Date, na.rm = TRUE)
    ending <- max(dfevents$Visit_Date, na.rm = TRUE)
    # Plot Time Series
    ts <- ggplot(dfevents) +
        aes(as.Date(Visit_Date), as.numeric(pct), color=condition) +
        geom_line(size=1) +
        labs(x=paste ("From", begining, "to", ending,"\n","Source: Kansas BioSense Data"), y="per 10,000 Visits", title= paste0("Daily Rates of Emergency Department Visits","\n","for " ,condition, "-related Conditions"))

    graphList[[1]] <- ts

    #Pie Chart of Sex
    pSex <- ggplot(data=dfcond,aes(x=factor(1), fill=Gender)) +
        geom_bar(width=1)
    pSex <- pSex + coord_polar(theta = "y") +
        ggtitle(paste0(condition, " ED Visits by Sex"))

    graphList[[2]] <- pSex

    #Pie Chart for Age Group
    pAge <- ggplot(data=dfcond,aes(x=factor(1), fill=Agecat)) +
        geom_bar(width=1)
    pAge <- pAge + coord_polar(theta = "y") +
        ggtitle(paste0(condition, " ED Visits by Age Group"))


    graphList[[3]] <- pAge

    return(graphList)

}
