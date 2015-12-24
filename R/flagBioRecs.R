
#' Flags records based on set keywords and specified columns in a data.frame
#'
#' @param df a NSSP data.frame with original column names
#' @param terms a vector of stings and words of interest
#' @param searchfields the index of the columns targeted for the search
#'
#' @return a table with the Unique_Visiting_ID as the first column and a column
#'   for each field where one or more of the terms have been found.
#'
#' @export
#'
#' @details In the final table, each field will receive a 1 if there is a hit or
#'   a 0 otherwise. If a target column doesn't contain any of the sought terms
#'   it does not get included in the final table. It helps identify which field
#'   is the most relevant for this particular terms string.
flagBioRecs <- function (df,terms, searchfields=NULL){
    #browser()
    if(is.null(searchfields)){
        allfields <- names(df)
        knownfields <- paste("Chief_Complaint|Triage_Notes|Diagnosis_Text|Diagnosis_Code")
        keyfields <- grep(knownfields, allfields)

    } else {
        keyfields <- searchfields
    }
  dfx <-data.frame("Unique_Visiting_ID"=NULL,"HIT"=NULL); #browser()
  for (intx in 1:length(keyfields)){
      df[,keyfields[intx]]<-gsub(":SEP:"," ",df[,keyfields[intx]] )
    newrecs<-subset(df, grepl(terms, df[,keyfields[intx]]))
    if (dim(newrecs)[1]>0){
      dft <- data.frame("Unique_Visiting_ID"=newrecs$Unique_Visiting_ID)
      dft$HIT <- colnames(df)[keyfields[intx]] ; #browser()
      dfx <- rbind(dfx,dft)
    }else dfx <- dfx

  }

  if (dim(dfx)[1]>0){
    dtab <- xtabs(~Unique_Visiting_ID+HIT,data=dfx)
  } else {
    dfx <- data.frame(Unique_Visiting_ID=rep(NA,4),
                      HIT=c("Chief_Complaint","Diagnosis_Text","Triage_Notes","Diagnosis_Code"))
    dtab <- xtabs(~Unique_Visiting_ID+HIT,data=dfx)
  }
  return(dtab)

}




#' Retrieves a specific inclusion terms or exclusion terms for case definitions
#'
#' @param condition a string indicating the name of the condition of interest
#' @param incl_excl a string indicating "inclusions" or "exclusions"
#' @param default_utilities a boolean variable indicating if the default file
#'   containing the inclusion/exclusion terms should be used or not.
#' @param date a single R date indicating the signature date of the definition
#'
#' @return A vector of one string that represents the search term
#' @export
#'
#' @details The date part of this function is not fully implemented yet. It is
#'   important that the names of the condition is spelled correctly.

getDefinitions <- function(condition, incl_excl, default_utilities = TRUE, date = NULL){
    if(default_utilities != TRUE){
        fi <- fetchFile();
        fn <- fi[["filepathR"]]
    }else fn <- file.path("J:", "Documents", "Work", "KDHE", "Projects", "Syndromic Surveillance", "Utilities", "Search Terms History.xlsx")

    wb <- XLConnect::loadWorkbook(fn)
    ws <- XLConnect::readWorksheet(wb, sheet = condition, header = TRUE, colTypes = c("POSIXct", rep("character",3)))
    ws <- dplyr::arrange(ws, desc(date))
    if(incl_excl == "inclusions" | incl_excl == 2){
        txt <- paste0(ws[1,2])
    }else {
        if (incl_excl == "exclusions" | incl_excl == 3) txt <- paste0(ws[1,3])
    }

    return(txt)
}
