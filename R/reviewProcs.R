## Create data set for review and classification


#' Creates a data.frame containing records for manual review and labeling
#'
#' @param df a data.frame containing the raw data to be prepared for review
#' @param labcol a string indicating the name of the label column
#' @param colList a vector of column names to be included in the final review
#'   data.frame.
#' @param savename a string (without extension) indicating the name of the
#'   review data.frame to be saved on the hard drive.
#'
#' @return a data.frame with a label column at the end.
#' @export
#'
#' @details If the savename is not provided the file will be saved in the
#'   working directory. The savename can contain the full path of the file.
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
        savefile(sfile = df, tfile = "ReviewSet", wd=TRUE)
    }else savefile(sfile = df, tfile = savename, wd=FALSE)

    return(df)
}




