

#' Custom Display of NSSP ED Patient Record
#'
#' @param df a data.frame of patient records
#' @param UID the index number of the row to be displayed
#' @param colUID the index number of the column that contains the unique row
#'   identifiers
#' @param colList a vector of names of columns to be displayed.
#'
#' @return a list containing information for a single record
#'
#' @details This function displays a selected record as a list. It needs a
#'   data.frame and a unique identifier for the record. The function can also
#'   display a record based on other unique identifiers such as the row record
#'   number. In that case the index number of the column with the unique
#'   identifier using the colUID argument. The number of columns displayed may
#'   also be reduced by providing a vector with the name of the columns of
#'   interest using the colList argument.
#'   This function is particularly useful when inspecting and labeling cases.
#'
#' @export
bio_Cust_Rec_Display <- function (df, UID, colUID=NULL,colList=NULL) {
    if (is.null(colList)){
        colList <- c("Unique_Visiting_ID", "Facility_Name",
                     "Chief_Complaint","Triage_Notes", "Diagnosis_Text",
                     "Diagnosis_Code", "Age", "Gender", "Earliest_Date",
                     "County_Name")

    }

    blnames <- names(df) %in% colList

    df <- df[, blnames]

    if (is.null(colUID)){
        rec <- as.list(df[UID,])
    }else {
        numcolUID <- grep(paste0("\\b", colUID, "\\b"), names(df))
        rec <- as.list(df[which(df[,colUID]==UID),])
    }
    return(rec)
}
