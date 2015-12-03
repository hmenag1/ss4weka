### myUtilities

checklib <- function(lib){
    sc <- grep(lib, search())
    if(length(sc)==0){
        lapply(lib, require, character.only=TRUE)
    }
}


xtable2df <- function(xtable, rname2ID=TRUE){
    df <- as.data.frame.matrix(xtable)
    idcol <- names(dimnames(xtable)[1])
    df$tmpID <- rownames(df)
    allcols <- colnames(df)
    allcols[length(allcols)] <- idcol
    colnames(df) <- allcols
    rownames(df) <- NULL
    rownames(df) <- row(df, as.factor = FALSE)[,1]

    return(df)

}


moveCol <- function(df, from="last", to=1){

    xnum <- vector(mode = "numeric")
    cnum <- ncol(df)
    if(from=="last") from <- cnum

    if (to>from){
        df <- df[, cnum:1]
        alpha <- cnum-from+1
        omega <- cnum-to+1
        xnum <- permOrder(alpha, omega, cnum)
        df <- df[, xnum]
        df <- df[, cnum:1]


    }else {
        alpha <- from
        omega <- to
        xnum <- permOrder(alpha, omega, cnum)
        df <- df[, xnum]
    }

    if(alpha==omega) stop("to and from must be different")

    for(i in 1:N){
        if(i==omega){
            xnum[i]<- alpha
            w <- alpha+1
            for (z in w:N){
                xnum[i+1] <- z
            }
            #i <- i+1
            #if (i> ncol(df)) break()
        }else {
            if(i < omega){
                y <- i
            } else {
                y <- i-1
            }
            xnum[i] <- y
        }
    }

    if (to>from){
        df <- df[, N:1]
    }

    df<- df[,xnum]

    return(df)


}

permOrder <- function(alpha, omega, cnum){

    if(alpha==omega) stop("to and from must be different")
    xnum <- vector(mode = "numeric")
    part2 <- seq(omega, alpha-1)
    if(alpha<cnum) part3 <- seq(alpha+1, cnum) else part3 <- NULL
    if(omega>1) part1 <- seq(1,omega-1) else part1 <- NULL
    xnum <- c(part1, alpha, part2, part3)
    return(xnum)
}
