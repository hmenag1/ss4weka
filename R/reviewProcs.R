## Create data set for review and classification

createReviewSet <- function(df, labcol= NULL, colList = NULL, savename=NULL){
    #browser()
    if(!is.null(labcol)){
        df <- dplyr::mutate(df, tmpcol=NA)
        df <- data.table::setnames(df, "tmpcol", labcol)
    }else df <- dplyr::mutate(df, case=NA)

    if(!is.null(colList)){
        nms <- names(df)
        sn <- nms %in% colList
        df <- df[,sn]
    }
    if(!is.null(savename)){
        savefile(sfile = df, tfile = savename, wd=FALSE)
    }

    return(df)
}


bio_Cust_df <- function (df, colList=NULL) {
    if (is.null(colList)){
        colList <- c("Unique_Visiting_ID", "Facility_Name",
                     "Chief_Complaint","Triage_Notes", "Diagnosis_Text",
                     "Diagnosis_Code", "Age", "Age_Units","Gender", "Earliest_Date",
                     "County_Name")

    }

    blnames <- names(df) %in% colList

    df <- df[, blnames]

    return(df)
}

