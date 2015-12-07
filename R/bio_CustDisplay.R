
## Display functions

#The shortDisp function displays a selected record as a list. It needs a data frame
#and the row record number (row name) of interest. The function can also display
#records based on other unique identifiers other than the row record number. In
#that case one need to provide the index number of the column with the unique
#identifier using the colUID argument. The number of columns displayed may also
#be reduced by providing a vector with the name of the columns of interest using
#the colList argument.


bio_CustDisplay <- function (df, UID, colUID=NULL,colList=NULL) {
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
