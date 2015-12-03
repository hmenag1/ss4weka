#####################################################
## The flagBioRecs function returns a crosstab with the unique Unique_Visiting_ID on the left and a column for each field sought for the terms of interest. Each field will receive a 1 if there is a hit or a 0 if there is no hit. If a field being sought did not receive at least 1 hit it will not be represented in the final table. It helps identify whic field is the most relevant for this particular terms string.

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
  #browser()
  if (dim(dfx)[1]>0){
    dtab <- xtabs(~Unique_Visiting_ID+HIT,data=dfx)
  } else {
    dfx <- data.frame(Unique_Visiting_ID=rep(NA,4),
                      HIT=c("Chief_Complaint","Diagnosis_Text","Triage_Notes","Diagnosis_Code"))
    dtab <- xtabs(~Unique_Visiting_ID+HIT,data=dfx)
  }
  return(dtab)

}
