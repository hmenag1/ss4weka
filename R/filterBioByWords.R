### filterBioByWords

filterBioByWords <- function(df, inclusions=NULL, exclusions=NULL){
    checklib("sqldf")
    lstEval <- list()
    eval1 <- flagBioRecs(df, terms=inclusions)
    lstEval[["eval1"]] <- xtable2df(eval1)

    if (dim(eval1)[1]>0){
        eval1_num <- as.data.frame.matrix(eval1)
        eval1_num$Unique_Visiting_ID <- rownames(eval1_num)
        eval1_num <- eval1_num[, c(length(eval1_num), 1:(length(eval1_num)-1))]

        cut1 <- sqldf("SELECT t2.* FROM eval1_num t1 LEFT JOIN df t2 ON t1.Unique_Visiting_ID = t2.Unique_Visiting_ID")


        fips <- read.csv("FIPSPLUS.csv", stringsAsFactors = FALSE)

        cut1 <- sqldf("SELECT t1.*, t2.COUNTY AS County_Name FROM cut1 t1 LEFT JOIN fips t2 ON t1.County=t2.FIPS")

        lstEval[["cut1"]] <- cut1


        eval2 <- flagBioRecs(df, terms=exclusions)
        lstEval[["eval2"]] <- xtable2df(eval2)

        if (length(eval2)>0){
            eval2_num <- as.data.frame.matrix(eval2)
            eval2_num$Unique_Visiting_ID <- rownames(eval2_num)
            eval2_num <- eval2_num[, c(length(eval2_num), 1:(length(eval2_num)-1))]

            cut2 <- sqldf("SELECT t2.* FROM eval2_num t1 LEFT JOIN cut1 t2 ON t1.Unique_Visiting_ID= t2.Unique_Visiting_ID")
            lstEval[["cut2"]] <- cut2

            # Remove false positive cases
            cut3 <- sqldf("SELECT * FROM cut1 WHERE Unique_Visiting_ID NOT IN
                          (SELECT Unique_Visiting_ID FROM eval2_num )" )
            lstEval[["cut3"]] <- cut3
        }else {
            cut3 <- cut1
            lstEval[["cut2"]] <- NULL
            lstEval[["cut3"]] <- cut3
        }
        return(lstEval)
    }else {
        stop("No records found!")
        quit("ask")
    }

}
