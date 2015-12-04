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
        xnum <- permIntVec.1by1(alpha, omega, cnum)
        df <- df[, xnum]
        df <- df[, cnum:1]


    }else {
        alpha <- from
        omega <- to
        xnum <- permIntVec.1by1(alpha, omega, cnum)
        df <- df[, xnum]
    }

}


permIntVec.1by1 <- function(alpha, omega, cnum){
    # Coerce input to integer if possible
    alpha <- as.integer(alpha); omega <- as.integer(omega); cnum <- as.integer(cnum)

    # Check if the parameters are integers
    if(is.integer(alpha) & is.integer(omega) & is.integer(cnum)){
        #browser()
        # if cnum less than both alpha or omega
        if((cnum >= alpha) & (cnum >= omega)){
            # If alpha and omega are the equal, do nothing.
            if(alpha==omega) {
                break()
            } else {
                # alpha and omega are not equal then check for direction
                l2r <- logical()

                if(alpha < omega){
                    l2r <- TRUE

                } else l2r <- FALSE

                xnum <- vector(mode = "integer") # container

                # Move item from left to rigth.
                if(l2r==TRUE){
                    # Move item from left to right
                    part2 <- seq(alpha+1, omega)
                    if(omega<cnum) part3 <- seq(omega+1, cnum) else part3 <- NULL
                    if(alpha>1) part1 <- seq(1,alpha-1) else part1 <- NULL
                    xnum <- c(part1, part2, alpha,  part3)

                } else {

                    # Move item from right to left
                    part2 <- seq(omega, alpha-1)
                    if(alpha<cnum) part3 <- seq(alpha+1, cnum) else part3 <- NULL
                    if(omega>1) part1 <- seq(1,omega-1) else part1 <- NULL
                    xnum <- c(part1, alpha, part2, part3)
                }
            }
        } else stop("Cannot move item beyond the length of the input vector.")


    } else stop("Function's parameters must be all integers.")

        return(xnum)
}


moveCols <- function(df, chng){
    nr <- nrow(chng)
    cnum <- ncol(df)
    labs <- colnames(df)

    if(nr>0){

        for(intx in 1:nr){
            if(intx>1) nlabs <- colnames(df) else nlabs <- labs
            ofrm <- chng[intx, 1]; oto <- chng[intx,2]
            lfrm <- labs[ofrm]; lto <- labs[oto]
            nfrm <- grep(paste0("\\b",lfrm, "\\b"), nlabs); #browser()
            #nto <- grep(paste0("\\b",lto, "\\b"), nlabs)
            df <- moveCol(df, nfrm, oto)
        }

    }

    return(df)

}
