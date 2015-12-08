## Create data set for review and classification

createReviewSet <- function(df, labcol= NULL, savepath = NULL, savename=NULL){
    #browser()
    if(!is.null(labcol)){
        df <- dplyr::mutate(df, tmpcol=NA)
        df <- data.table::setnames(df, "tmpcol", labcol)
    }else df <- dplyr::mutate(df, case=FALSE)

    if(!is.null(savepath)){
        fp <- savepath
    } else{
        fp <- choose.dir()
        fp <- gsub("\\\\","/",fp)
    }

    if(!is.null(savename)){
        fname <- file.path(fp, savename)
    }else fname <- file.path(fp, "data_for_review.csv")

    write.csv(df, fname, row.names = FALSE)

    return(df)
}


bio_Cust_df <- function (df, colList=NULL) {
    if (is.null(colList)){
        colList <- c("Unique_Visiting_ID", "Facility_Name",
                     "Chief_Complaint","Triage_Notes", "Diagnosis_Text",
                     "Diagnosis_Code", "Age", "Gender", "Earliest_Date",
                     "County_Name")

    }

    blnames <- names(df) %in% colList

    df <- df[, blnames]

    return(df)
}

